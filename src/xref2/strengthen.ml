(* Note for future improvement (suggested by @lpw25):

You can actually do something even more interesting with that case for strengthening.

If you have:
[module type S = sig type t end]

and you want to strengthen [S] with the module path [M] then you can produce:
[S with type t = M.t]

The compiler doesn't do this because it doesn't actually have a representation for `with` in its type algebra
(`with` is always just expanded away right after paprsing). But since we do have a representation for it, 
this is probably the best thing to produce in this case.

*)

let rec signature (prefix : Cpath.Resolved.module_) sg =
  let open Component.Signature in
  let open Odoc_model.Names in
  let items =
    List.map
      (fun item ->
        match item with
        | Module (id, r, m) ->
            Module
              ( id,
                r,
                Component.Delayed.put (fun () ->
                    module_
                      (`Module
                        ( `Module prefix,
                          ModuleName.of_string (Ident.Name.module_ id) ))
                      (Component.Delayed.get m)) )
        | ModuleType (id, mt) ->
            ModuleType
              ( id,
                Component.Delayed.put (fun () ->
                    module_type
                      (`ModuleType
                        ( `Module prefix,
                          ModuleTypeName.of_string (Ident.Name.module_type id)
                        ))
                      (Component.Delayed.get mt)) )
        | Type (id, r, t) ->
            Type
              ( id,
                r,
                Component.Delayed.put (fun () ->
                    type_decl
                      (`Type
                        ( `Module prefix,
                          TypeName.of_string (Ident.Name.type_ id) ))
                      (Component.Delayed.get t)) )
        | Exception _ | TypExt _ | Value _ | External _ | Class _ | ClassType _
        | Include _ | ModuleSubstitution _ | TypeSubstitution _ | Comment _
        | Open _ ->
            item)
      sg.items
  in
  (* The identity substitution used here is to rename all of the bound idents in the signature *)
  Subst.signature Subst.identity { items; removed = sg.removed }

and module_ : Cpath.Resolved.module_ -> Component.Module.t -> Component.Module.t
    =
 fun prefix m ->
  match m.Component.Module.type_ with
  | Alias _ -> m
  | ModuleType (Signature sg) ->
      { m with type_ = ModuleType (Signature (signature prefix sg)) }
  | ModuleType _ -> m

and module_type :
    Cpath.Resolved.module_type ->
    Component.ModuleType.t ->
    Component.ModuleType.t =
 fun prefix m ->
  let open Component.ModuleType in
  let expr =
    match m.expr with
    | None -> Some (Component.ModuleType.Path (`Resolved prefix))
    | Some (Component.ModuleType.Path _p) ->
        (* TODO *)
        m.expr
    | Some (Component.ModuleType.With (_, _)) ->
        (* TODO *)
        m.expr
    | Some (Component.ModuleType.Signature _) -> m.expr
    | Some (Component.ModuleType.Functor _) -> m.expr
    | Some (Component.ModuleType.TypeOf _) -> m.expr
  in
  { m with expr }

and type_decl :
    Cpath.Resolved.type_ -> Component.TypeDecl.t -> Component.TypeDecl.t =
 fun path t ->
  let equation =
    let e = t.Component.TypeDecl.equation in
    let open Component.TypeDecl.Equation in
    let constr_params =
      List.map
        (fun (desc, _) ->
          match desc with
          | Odoc_model.Lang.TypeDecl.Var x -> Component.TypeExpr.Var x
          | Any -> Any)
        e.params
    in
    let manifest =
      match e.manifest with
      | None -> Some (Component.TypeExpr.Constr (`Resolved path, constr_params))
      | _ -> e.manifest
    in
    {
      params = e.params;
      private_ = e.private_;
      manifest;
      constraints = e.constraints;
    }
  in
  { t with equation }
