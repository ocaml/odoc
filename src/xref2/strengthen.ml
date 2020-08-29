(* Note for future improvement (suggested by @lpw25):

You can actually do something even more interesting with that case for strengthening.

If you have:
[module type S = sig type t end]

and you want to strengthen [S] with the module path [M] then you can produce:
[S with type t = M.t]

The compiler doesn't do this because it doesn't actually have a representation for `with` in its type algebra
(`with` is always just expanded away right after parsing). But since we do have a representation for it, 
this is probably the best thing to produce in this case.

*)

open Component
open Delayed

let rec signature :
    Cpath.module_ ->
    ?canonical:Cpath.module_ * Odoc_model.Paths.Reference.Module.t ->
    Signature.t ->
    Signature.t =
 fun prefix ?canonical sg ->
  let open Signature in
  let items =
    List.map
      (fun item ->
        match item with
        | Module (id, r, m) ->
            let name = Ident.Name.module_ id in
            let canonical =
              match canonical with
              | Some (p, r) ->
                  Some
                    ( `Dot (p, name),
                      `Dot
                        ((r :> Odoc_model.Paths.Reference.LabelParent.t), name)
                    )
              | None -> None
            in
            Module
              ( id,
                r,
                put (fun () -> module_ ?canonical (`Dot (prefix, name)) (get m))
              )
        | ModuleType (id, mt) ->
            ModuleType
              ( id,
                put (fun () ->
                    module_type
                      (`Dot (prefix, Ident.Name.module_type id))
                      (get mt)) )
        | Type (id, r, t) ->
            Type
              ( id,
                r,
                put (fun () ->
                    type_decl (`Dot (prefix, Ident.Name.type_ id)) (get t)) )
        | Exception _ | TypExt _ | Value _ | External _ | Class _
        | ClassType _ | Include _ | ModuleSubstitution _ | TypeSubstitution _
        | Comment _ | Open _ ->
            item)
      sg.items
  in
  (* The identity substitution used here is to rename all of the bound idents in the signature *)
  Subst.signature Subst.identity { items; removed = sg.removed }

and module_ :
    ?canonical:Cpath.module_ * Odoc_model.Paths.Reference.Module.t ->
    Cpath.module_ ->
    Component.Module.t ->
    Component.Module.t =
 fun ?canonical prefix m ->
  match m.type_ with
  | Alias _ -> m
  | ModuleType _ -> { m with canonical; type_ = Alias (prefix, None) }

(* nuke the expansion as this could otherwise lead to inconsistencies - e.g. 'AlreadyASig' *)
and module_type :
    Cpath.module_type -> Component.ModuleType.t -> Component.ModuleType.t =
 fun prefix m ->
  let expr =
    match m.expr with None -> Some (ModuleType.Path {p_path=prefix; p_expansion=None}) | Some _ -> m.expr
  in
  { m with expr }

and type_decl : Cpath.type_ -> TypeDecl.t -> TypeDecl.t =
 fun path t ->
  let equation =
    let e = t.TypeDecl.equation in
    let open TypeDecl.Equation in
    let constr_params =
      List.map
        (fun {Odoc_model.Lang.TypeDecl.desc; _} ->
          match desc with
          | Odoc_model.Lang.TypeDecl.Var x -> TypeExpr.Var x
          | Any -> Any)
        e.params
    in
    let manifest =
      match e.manifest with
      | None -> Some (TypeExpr.Constr (path, constr_params))
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
