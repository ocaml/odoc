open Odoc_model.Paths
open Odoc_model.Names
open Reference
open Utils.OptionMonad

type module_lookup_result =
  Resolved.Module.t * Cpath.Resolved.module_ * Component.Module.t

type module_type_lookup_result =
  Resolved.ModuleType.t * Cpath.Resolved.module_type * Component.ModuleType.t

type signature_lookup_result =
  Resolved.Signature.t * Cpath.Resolved.parent * Component.Signature.t

type type_lookup_result =
  Resolved.Type.t
  * [ `T of Component.TypeDecl.t
    | `C of Component.Class.t
    | `CT of Component.ClassType.t ]

type value_lookup_result = Resolved.Value.t

type label_parent_lookup_result =
  Resolved.LabelParent.t
  * Cpath.Resolved.parent option
  * [ `S of Component.Signature.t
    | `CS of Component.ClassSignature.t
    | `Page of (string * Identifier.Label.t) list ]

type label_parent_result' =
  [ `S of Resolved.Signature.t * Cpath.Resolved.parent * Component.Signature.t
  | `CS of
    Resolved.Parent.t * Cpath.Resolved.parent * Component.ClassSignature.t
  | `Page of Resolved.Page.t * (string * Identifier.Label.t) list ]

let rec choose l =
  match l with
  | [] -> None
  | x :: rest -> ( match x () with Some _ as x -> x | None -> choose rest )

let restrict_label_parent_result : _ -> label_parent_result' option =
  function
  | ( ( ( `Identifier #Odoc_model.Paths.Identifier.Signature.t
        | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ | `Hidden _
          ) as parent_ref ),
      Some parent_path,
      `S parent_sg ) ->
      Some (`S (parent_ref, parent_path, parent_sg))
  | ( ( ( `Identifier #Odoc_model.Paths_types.Identifier.path_type
        | `Type _ | `Class _ | `ClassType _ ) as parent_ref ),
      Some parent_path,
      `CS csg ) ->
      Some (`CS (parent_ref, parent_path, csg))
  | ( (`Identifier #Odoc_model.Paths_types.Identifier.page as parent_ref),
      _,
      `Page page ) ->
      Some (`Page (parent_ref, page))
  | _, _, _ -> None

let signature_lookup_result_of_label_parent :
    label_parent_lookup_result -> signature_lookup_result option =
 fun (rr, cp, c) ->
  match (rr, cp, c) with
  | (`Identifier #Odoc_model.Paths.Identifier.Signature.t as rr'), Some p, `S s
    ->
      Some (rr', p, s)
  | ( ( (`SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ | `Hidden _)
      as rr' ),
      Some p,
      `S s ) ->
      Some (rr', p, s)
  | _ -> None


let module_lookup_to_signature_lookup :
    Env.t -> module_lookup_result -> signature_lookup_result option =
 fun env (ref, cp, m) ->
  match Tools.signature_of_module env m with
  | Ok sg -> Some ((ref :> Resolved.Signature.t), `Module cp, sg)
  | Error _ -> None
  | exception _ -> None

let module_type_lookup_to_signature_lookup :
    Env.t -> module_type_lookup_result -> signature_lookup_result option =
 fun env (ref, cp, m) ->
  match Tools.signature_of_module_type env m with
  | Ok sg -> Some ((ref :> Resolved.Signature.t), `ModuleType cp, sg)
  | Error _ -> None

(** Module *)

let rec module_of_component env m base_path' base_ref' : module_lookup_result =
  let base_path, base_ref =
    if m.Component.Module.hidden then (`Hidden base_path', `Hidden base_ref')
    else (base_path', base_ref')
  in
  let p, r =
    match Tools.get_module_path_modifiers env true m with
    | None -> (base_path, base_ref)
    | Some (`SubstAliased cp) ->
        let cp = Tools.reresolve_module env cp in
        let p = Lang_of.(Path.resolved_module empty cp) in
        (`SubstAlias (cp, base_path), `SubstAlias (p, base_ref))
    | Some (`Aliased cp) ->
        let cp = Tools.reresolve_module env cp in
        let p = Lang_of.(Path.resolved_module empty cp) in
        (`Alias (cp, base_path), `SubstAlias (p, base_ref))
    | Some (`SubstMT cp) ->
        let cp = Tools.reresolve_module_type env cp in
        (`Subst (cp, base_path), base_ref)
  in
  (r, p, m)

and resolve_module_reference env (r : Module.t) :
    module_lookup_result option =
  match r with
  | `Resolved _r -> failwith "What's going on!?"
  (*        Some (resolve_resolved_module_reference env r ~add_canonical)*)
  | `Dot (parent, name) ->
      module_in_label_parent' env parent name
  | `Module (parent, name) ->
      module_in_signature_parent' env parent name
  | `Root (name, _) -> module_in_env env name

and module_in_signature_parent env
    ((parent, parent_cp, sg) : signature_lookup_result) name :
    module_lookup_result option =
  let parent_cp = Tools.reresolve_parent env parent_cp in
  let sg = Tools.prefix_signature (parent_cp, sg) in
  Find.module_in_sig sg (ModuleName.to_string name) >>= fun m ->
  Some
    (module_of_component env m
       (`Module (parent_cp, name))
       (`Module (parent, name)))

and module_in_signature_parent' env parent name =
  resolve_signature_reference env parent >>= fun p ->
  module_in_signature_parent env p name

and module_in_label_parent' env parent name =
  resolve_label_parent_reference env parent
  >>= signature_lookup_result_of_label_parent
  >>= fun p ->
  module_in_signature_parent env p (ModuleName.of_string name)

and module_of_element env (`Module (id, m)) :
    module_lookup_result option =
  let base = `Identifier id in
  Some (module_of_component env m base base)

and module_in_env env name : module_lookup_result option =
  match Env.lookup_module_by_name (UnitName.to_string name) env with
  | Some (id, m) ->
      module_of_element env (`Module (id, m))
  | None -> (
      match Env.lookup_root_module (UnitName.to_string name) env with
      | Some (Env.Resolved (_, id, m)) ->
          module_of_element env (`Module (id, m))
      | _ -> None )

(** Module type *)

and module_type_of_component env mt base_path base_ref :
    module_type_lookup_result =
  match
    mt.Component.ModuleType.expr >>= Tools.get_substituted_module_type env
  with
  | Some p -> (base_ref, `SubstT (p, base_path), mt)
  | None -> (base_ref, base_path, mt)

and module_type_in_signature_parent env
    ((parent', parent_cp, sg) : signature_lookup_result) name :
    module_type_lookup_result option =
  let sg = Tools.prefix_signature (parent_cp, sg) in
  Find.module_type_in_sig sg (ModuleTypeName.to_string name) >>= fun mt ->
  Some
    (module_type_of_component env mt
       (`ModuleType (parent_cp, name))
       (`ModuleType (parent', name)))

and module_type_in_signature_parent' env parent name :
    module_type_lookup_result option =
  resolve_signature_reference env parent >>= fun p ->
  module_type_in_signature_parent env p name

and module_type_in_label_parent' env parent name :
    module_type_lookup_result option =
  resolve_label_parent_reference env parent
  >>= signature_lookup_result_of_label_parent
  >>= fun p ->
  let name = ModuleTypeName.of_string name in
  module_type_in_signature_parent env p name

and module_type_in_env env name : module_type_lookup_result option =
  Env.lookup_module_type_by_name (UnitName.to_string name) env
  >>= module_type_of_element env

and module_type_of_element _env (`ModuleType (id, mt)) :
    module_type_lookup_result option =
  Some (`Identifier id, `Identifier id, mt)

(***)
and resolve_type_reference : Env.t -> Type.t -> type_lookup_result option =
  let open Utils.OptionMonad in
  fun env r ->
    match r with
    | `Resolved _r -> failwith "unhandled"
    | `Root (name, _) -> (
        Env.lookup_datatype_by_name
          (Odoc_model.Names.UnitName.to_string name)
          env
        >>= function
        | `Type (id, t) ->
            return
              ( `Identifier
                  (id :> Odoc_model.Paths_types.Identifier.reference_type),
                `T t )
        | `Class (id, t) ->
            return
              ( `Identifier
                  (id :> Odoc_model.Paths_types.Identifier.reference_type),
                `C t )
        | `ClassType (id, t) ->
            return
              ( `Identifier
                  (id :> Odoc_model.Paths_types.Identifier.reference_type),
                `CT t ) )
    | `Dot (parent, name) ->
        resolve_label_parent_reference env parent
        >>= signature_lookup_result_of_label_parent
        >>= fun (parent', cp, sg) ->
        let sg = Tools.prefix_signature (cp, sg) in
        Find.type_in_sig sg name >>= fun t ->
        return (`Type (parent', TypeName.of_string name), t)
    | `Class (parent, name) -> (
        resolve_signature_reference env parent >>= fun (parent', _, sg) ->
        Find.type_in_sig sg (ClassName.to_string name) >>= function
        | `C _ as c -> return (`Class (parent', name), c)
        | _ -> None )
    | `ClassType (parent, name) -> (
        resolve_signature_reference env parent >>= fun (parent', cp, sg) ->
        let sg = Tools.prefix_signature (cp, sg) in
        Find.type_in_sig sg (ClassTypeName.to_string name) >>= function
        | `CT _ as c -> return (`ClassType (parent', name), c)
        | _ -> None )
    | `Type (parent, name) -> (
        resolve_signature_reference env parent >>= fun (parent', cp, sg) ->
        let sg = Tools.prefix_signature (cp, sg) in
        Find.type_in_sig sg (TypeName.to_string name) >>= function
        | `T _ as c -> return (`Type (parent', name), c)
        | _ -> None )

and resolve_label_parent_reference :
    Env.t -> LabelParent.t -> label_parent_lookup_result option =
  let open Utils.OptionMonad in
  fun env r ->
    let label_parent_res_of_sig_res :
        signature_lookup_result -> label_parent_lookup_result option =
     fun (r', cp, sg) -> return ((r' :> Resolved.LabelParent.t), Some cp, `S sg)
    in
    match r with
    | `Resolved _ -> failwith "unimplemented"
    | ( `Module _ | `ModuleType _
      | `Root (_, #Odoc_model.Paths_types.Reference.tag_module) ) as sr ->
        resolve_signature_reference env sr >>= label_parent_res_of_sig_res
    | `Dot (parent, name) ->
        choose
          [
            (fun () ->
              resolve_label_parent_reference env parent
              >>= signature_lookup_result_of_label_parent
              >>= fun p ->
              module_in_signature_parent env p (ModuleName.of_string name)
              >>= module_lookup_to_signature_lookup env
              >>= label_parent_res_of_sig_res);
            (fun () ->
              resolve_label_parent_reference env parent
              >>= signature_lookup_result_of_label_parent
              >>= fun p ->
              module_type_in_signature_parent env p
                (ModuleTypeName.of_string name)
              >>= module_type_lookup_to_signature_lookup env
              >>= label_parent_res_of_sig_res);
          ]
    | `Root (name, _) ->
        Env.lookup_page (UnitName.to_string name) env >>= fun p ->
        let labels =
          List.fold_right
            (fun element l ->
              match element.Odoc_model.Location_.value with
              | `Heading (_, (`Label (_, name) as x), _nested_elements) ->
                  (LabelName.to_string name, x) :: l
              | _ -> l)
            p.Odoc_model.Lang.Page.content []
        in
        return
          ( `Identifier (p.Odoc_model.Lang.Page.name :> Identifier.LabelParent.t),
            None,
            `Page labels )
    | _ -> None

and resolve_signature_reference :
    Env.t -> Signature.t -> signature_lookup_result option =
  let open Utils.OptionMonad in
  fun env' r ->
    (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_from_resolved_path: looking up %a\n%!" Component.Fmt.resolved_path p; *)
    let resolve env =
      (* Format.fprintf Format.err_formatter "B"; *)
      match r with
      | `Resolved _r ->
          failwith "What's going on here then?"
          (* Some (resolve_resolved_signature_reference env r ~add_canonical) *)
      | `Root (name, `TModule) ->
          module_in_env env name
          >>= module_lookup_to_signature_lookup env
      | `Module (parent, name) ->
          module_in_signature_parent' env parent name
          >>= module_lookup_to_signature_lookup env
      | `Root (name, `TModuleType) ->
          module_type_in_env env name
          >>= module_type_lookup_to_signature_lookup env
      | `ModuleType (parent, name) ->
          module_type_in_signature_parent' env parent name
          >>= module_type_lookup_to_signature_lookup env
      | `Root (name, `TUnknown) -> (
          Env.lookup_signature_by_name (UnitName.to_string name) env
          >>= function
          | `Module (_, _) as e ->
              module_of_element env e
              >>= module_lookup_to_signature_lookup env
          | `ModuleType (_, _) as e ->
              module_type_of_element env e
              >>= module_type_lookup_to_signature_lookup env )
      | `Dot (parent, name) -> (
          resolve_label_parent_reference env parent
          >>= signature_lookup_result_of_label_parent
          >>= fun (parent, parent_cp, sg) ->
          let parent_cp = Tools.reresolve_parent env parent_cp in
          let sg = Tools.prefix_signature (parent_cp, sg) in
          Find.signature_in_sig sg name >>= function
          | `Module (_, _, m) ->
              let name = ModuleName.of_string name in
              module_lookup_to_signature_lookup env
                (module_of_component env
                   (Component.Delayed.get m)
                   (`Module (parent_cp, name))
                   (`Module (parent, name)))
          | `ModuleType (_, mt) ->
              let name = ModuleTypeName.of_string name in
              module_type_lookup_to_signature_lookup env
                (module_type_of_component env (Component.Delayed.get mt)
                   (`ModuleType (parent_cp, name))
                   (`ModuleType (parent, name))) )
    in
    resolve env'

and resolve_value_reference : Env.t -> Value.t -> value_lookup_result option =
  let open Utils.OptionMonad in
  fun env r ->
    match r with
    | `Root (name, _) -> (
        Env.lookup_value_by_name (UnitName.to_string name) env >>= function
        | `Value (id, _x) -> return (`Identifier id)
        | `External (id, _x) -> return (`Identifier id) )
    | `Dot (parent, name) -> (
        resolve_label_parent_reference env parent
        >>= signature_lookup_result_of_label_parent
        >>= fun (parent', _, sg) ->
        match Find.opt_value_in_sig sg name with
        | Some _v -> return (`Value (parent', ValueName.of_string name))
        | None -> None )
    | `Value (parent, name) -> (
        resolve_signature_reference env parent >>= fun (parent', _, sg) ->
        match Find.opt_value_in_sig sg (ValueName.to_string name) with
        | Some _v -> return (`Value (parent', name))
        | None -> None )
    | `Resolved r -> Some r

and resolve_label_reference : Env.t -> Label.t -> Resolved.Label.t option =
  let open Utils.OptionMonad in
  fun env r ->
    match r with
    | `Resolved r -> Some r
    | `Root (name, _) -> (
        Env.lookup_label_by_name (UnitName.to_string name) env >>= function
        | `Label id -> return (`Identifier id) )
    | `Dot (parent, name) -> (
        resolve_label_parent_reference env parent >>= fun (p, _env, sg) ->
        match sg with
        | `S sg ->
            Find.opt_label_in_sig sg name >>= fun _ ->
            Some (`Label (p, LabelName.of_string name))
        | `CS _sg -> None
        | `Page p -> (
            try Some (`Identifier (List.assoc name p)) with _ -> None ) )
    | `Label (parent, name) -> (
        resolve_label_parent_reference env parent >>= fun (p, _, sg) ->
        match sg with
        | `S sg ->
            Find.opt_label_in_sig sg (LabelName.to_string name) >>= fun _ ->
            Some (`Label (p, name))
        | `CS _sg -> None
        | `Page p -> (
            try Some (`Identifier (List.assoc (LabelName.to_string name) p))
            with _ -> None ) )

let resolve_reference_dot_sg env ~parent_path ~parent_ref ~parent_sg name =
  let resolved (r, _, _) = Some (r :> Resolved.t) in
  Find.any_in_sig parent_sg name >>= function
  | `Module (_, _, m) ->
    let name = ModuleName.of_string name in
    resolved
      (module_of_component env
          (Component.Delayed.get m)
          (`Module (parent_path, name))
          (`Module (parent_ref, name)))
  | `ModuleType (_, mt) ->
    let name = ModuleTypeName.of_string name in
    resolved
      (module_type_of_component env (Component.Delayed.get mt)
          (`ModuleType (parent_path, name))
          (`ModuleType (parent_ref, name)))
  | _ -> None

let resolve_reference_dot env parent name =
  resolve_label_parent_reference env parent
  >>= restrict_label_parent_result
  >>= function
  | `S (parent_ref, parent_path, parent_sg) ->
      let parent_path = Tools.reresolve_parent env parent_path in
      let parent_sg = Tools.prefix_signature (parent_path, parent_sg) in
      resolve_reference_dot_sg ~parent_path ~parent_ref ~parent_sg env name
  | `CS _ | `Page _ -> None

let resolve_reference : Env.t -> t -> Resolved.t option =
  let resolved (r, _, _) = Some (r :> Resolved.t) in
  fun env r ->
    match r with
    | `Root (name, `TUnknown) -> (
        match Env.lookup_any_by_name (UnitName.to_string name) env with
        | (`Module (_, _) as e) :: _ ->
            module_of_element env e >>= resolved
        | (`ModuleType (_, _) as e) :: _ ->
            module_type_of_element env e >>= resolved
        | `Value (id, _) :: _ ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Type (id, _) :: _ ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Label id :: _ ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Class (id, _) :: _ ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `ClassType (id, _) :: _ ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `External (id, _) :: _ ->
            return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | [] -> None )
    | `Resolved r -> Some r
    | `Root (name, `TModule) ->
        module_in_env env name >>= resolved
    | `Module (parent, name) ->
        module_in_signature_parent' env parent name
        >>= resolved
    | `Root (name, `TModuleType) -> module_type_in_env env name >>= resolved
    | `ModuleType (parent, name) ->
        module_type_in_signature_parent' env parent name
        >>= resolved
    | (`Root (_, `TType) | `Type (_, _)) as r ->
        resolve_type_reference env r >>= fun (x, _) -> return (x :> Resolved.t)
    | (`Root (_, `TValue) | `Value (_, _)) as r ->
        resolve_value_reference env r >>= fun x -> return (x :> Resolved.t)
    | (`Root (_, `TLabel) | `Label (_, _)) as r ->
        resolve_label_reference env r >>= fun x -> return (x :> Resolved.t)
    | `Root (name, `TPage) -> (
        match Env.lookup_page (UnitName.to_string name) env with
        | Some p ->
            Some (`Identifier (p.Odoc_model.Lang.Page.name :> Identifier.t))
        | None -> None )
    | `Dot (parent, name) as r ->
        choose
          [
            (fun () -> resolve_reference_dot env parent name);
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying type reference\n%!"; *)
              resolve_type_reference env r >>= fun (x, _) ->
              return (x :> Resolved.t));
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying label reference\n%!"; *)
              resolve_label_reference env r >>= fun x -> return (x :> Resolved.t));
            (fun () ->
              (* Format.fprintf Format.err_formatter "Trying value reference\n%!"; *)
              resolve_value_reference env r >>= fun x -> return (x :> Resolved.t));
          ]
    | _ -> None

