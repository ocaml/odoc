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

type datatype_lookup_result = Resolved.DataType.t * Component.TypeDecl.t

type class_lookup_result = Resolved.Class.t * Component.Class.t

type class_type_lookup_result = Resolved.ClassType.t * Component.ClassType.t

type type_lookup_result =
  [ `T of datatype_lookup_result
  | `C of class_lookup_result
  | `CT of class_type_lookup_result ]

type value_lookup_result = Resolved.Value.t

type label_parent_lookup_result =
  [ `S of signature_lookup_result
  | type_lookup_result
  | `Page of Resolved.Page.t * (string * Identifier.Label.t) list ]

type class_signature_lookup_result =
  Resolved.ClassSignature.t * Component.ClassSignature.t

let rec choose l =
  match l with
  | [] -> None
  | x :: rest -> ( match x () with Some _ as x -> x | None -> choose rest )

let signature_lookup_result_of_label_parent :
    label_parent_lookup_result -> signature_lookup_result option = function
  | `S r -> Some r
  | `T _ | `C _ | `CT _ | `Page _ -> None

let class_lookup_result_of_type :
    type_lookup_result -> class_lookup_result option = function
  | `C r -> Some r
  | _ -> None

let class_type_lookup_result_of_type :
    type_lookup_result -> class_type_lookup_result option = function
  | `CT r -> Some r
  | _ -> None

module Hashable = struct
  type t = bool * Resolved.Signature.t

  let equal = ( = )

  let hash = Hashtbl.hash
end

module Memos1 = Hashtbl.Make (Hashable)

(*  let memo = Memos1.create 91*)

module Hashable2 = struct
  type t = bool * Signature.t

  let equal = ( = )

  let hash = Hashtbl.hash
end

module Memos2 = Hashtbl.Make (Hashable2)

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

let type_lookup_to_class_signature_lookup :
    Env.t -> type_lookup_result -> class_signature_lookup_result option =
  let resolved p' cs = Some ((p' :> Resolved.ClassSignature.t), cs) in
  fun env -> function
    | `T _ -> None
    | `C (p', c) -> Tools.class_signature_of_class env c >>= resolved p'
    | `CT (p', ct) -> Tools.class_signature_of_class_type env ct >>= resolved p'

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

and resolve_module_reference env (r : Module.t) : module_lookup_result option =
  match r with
  | `Resolved _r -> failwith "What's going on!?"
  (*        Some (resolve_resolved_module_reference env r ~add_canonical)*)
  | `Dot (parent, name) -> module_in_label_parent' env parent name
  | `Module (parent, name) -> module_in_signature_parent' env parent name
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
  >>= fun p -> module_in_signature_parent env p (ModuleName.of_string name)

and module_of_element env (`Module (id, m)) : module_lookup_result option =
  let base = `Identifier id in
  Some (module_of_component env m base base)

and module_in_env env name : module_lookup_result option =
  match Env.lookup_module_by_name (UnitName.to_string name) env with
  | Some (id, m) -> module_of_element env (`Module (id, m))
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

(** Type *)

and class_of_element _env (`Class (id, t)) : class_lookup_result =
  (`Identifier id, t)

and class_type_of_element _env (`ClassType (id, t)) : class_type_lookup_result =
  ((`Identifier id :> Resolved.ClassType.t), t)

and class_in_env env name =
  Env.lookup_class_by_name (UnitName.to_string name) env >>= fun e ->
  Some (class_of_element env e)

and class_type_in_env env name =
  Env.lookup_class_type_by_name (UnitName.to_string name) env >>= fun e ->
  Some (class_type_of_element env e)

and type_in_env env name : type_lookup_result option =
  Env.lookup_datatype_by_name (UnitName.to_string name) env >>= function
  | `Type (id, t) -> Some (`T (`Identifier id, t))
  | `Class _ as e -> Some (`C (class_of_element env e))
  | `ClassType _ as e -> Some (`CT (class_type_of_element env e))

and class_of_component _env c ~parent_ref name : class_lookup_result option =
  Some (`Class (parent_ref, ClassName.of_string name), c)

and classtype_of_component _env ct ~parent_ref name :
    class_type_lookup_result option =
  Some (`ClassType (parent_ref, ClassTypeName.of_string name), ct)

and datatype_of_component _env t ~parent_ref name :
    datatype_lookup_result option =
  Some (`Type (parent_ref, TypeName.of_string name), t)

and type_in_signature_parent _env
    ((parent', parent_cp, sg) : signature_lookup_result) name :
    type_lookup_result option =
  let sg = Tools.prefix_signature (parent_cp, sg) in
  Find.type_in_sig sg name >>= function
  | `T t -> Some (`T (`Type (parent', TypeName.of_string name), t))
  | `C c -> Some (`C (`Class (parent', ClassName.of_string name), c))
  | `CT ct ->
      Some (`CT (`ClassType (parent', ClassTypeName.of_string name), ct))

(* Don't handle name collisions between class, class types and type decls *)
and type_in_signature_parent' env parent name =
  resolve_signature_reference env parent >>= fun p ->
  type_in_signature_parent env p name

and type_in_label_parent' env parent name =
  resolve_label_parent_reference env parent
  >>= signature_lookup_result_of_label_parent
  >>= fun p -> type_in_signature_parent env p name

and class_in_signature_parent' env parent name =
  type_in_signature_parent' env parent (ClassName.to_string name)
  >>= class_lookup_result_of_type

and classtype_in_signature_parent' env parent name =
  type_in_signature_parent' env parent (ClassTypeName.to_string name)
  >>= class_type_lookup_result_of_type

and datatype_of_element _env (`Type (id, t)) : datatype_lookup_result =
  (`Identifier id, t)

and datatype_in_env env name : datatype_lookup_result option =
  Env.lookup_datatype_by_name (UnitName.to_string name) env >>= function
  | `Type _ as e -> Some (datatype_of_element env e)
  | _ -> None

and datatype_in_signature_parent _env
    ((parent', parent_cp, sg) : signature_lookup_result) name :
    datatype_lookup_result option =
  let sg = Tools.prefix_signature (parent_cp, sg) in
  Find.datatype_in_sig sg (TypeName.to_string name) >>= fun t ->
  Some (`Type (parent', name), t)

and datatype_in_signature_parent' env parent name =
  resolve_signature_reference env parent >>= fun p ->
  datatype_in_signature_parent env p name

and datatype_in_label_parent' env parent name =
  resolve_label_parent_reference env parent
  >>= signature_lookup_result_of_label_parent
  >>= fun p -> datatype_in_signature_parent env p (TypeName.of_string name)

(***)
and label_parent_in_env env name : label_parent_lookup_result option =
  Env.lookup_label_parent_by_name (UnitName.to_string name) env >>= function
  | `Module _ as e ->
      module_of_element env e >>= module_lookup_to_signature_lookup env
      >>= fun r -> Some (`S r)
  | `ModuleType _ as e ->
      module_type_of_element env e
      >>= module_type_lookup_to_signature_lookup env
      >>= fun r -> Some (`S r)
  | `Type _ as e -> Some (`T (datatype_of_element env e))
  | `Class _ as e -> Some (`C (class_of_element env e))
  | `ClassType _ as e -> Some (`CT (class_type_of_element env e))

and resolve_label_parent_reference :
    Env.t -> LabelParent.t -> label_parent_lookup_result option =
  let open Utils.OptionMonad in
  fun env r ->
    let label_parent_res_of_sig_res :
        signature_lookup_result -> label_parent_lookup_result option =
     fun (r', cp, sg) -> return (`S (r', cp, sg))
    in
    match r with
    | `Resolved _ -> failwith "unimplemented"
    | `Root (name, `TUnknown) -> label_parent_in_env env name
    | (`Module _ | `ModuleType _ | `Root (_, (`TModule | `TModuleType))) as sr
      ->
        resolve_signature_reference env sr >>= label_parent_res_of_sig_res
    | `Root (name, `TType) -> datatype_in_env env name >>= fun r -> Some (`T r)
    | `Type (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        datatype_in_signature_parent env p name >>= fun r -> Some (`T r)
    | `Root (name, `TClass) -> class_in_env env name >>= fun r -> Some (`C r)
    | `Class (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        type_in_signature_parent env p (ClassName.to_string name)
        >>= class_lookup_result_of_type
        >>= fun r -> Some (`C r)
    | `Root (name, `TClassType) ->
        class_type_in_env env name >>= fun r -> Some (`CT r)
    | `ClassType (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        type_in_signature_parent env p (ClassTypeName.to_string name)
        >>= class_type_lookup_result_of_type
        >>= fun r -> Some (`CT r)
    | `Dot (parent, name) ->
        resolve_label_parent_reference env parent
        >>= signature_lookup_result_of_label_parent
        >>= fun p ->
        choose
          [
            (fun () ->
              module_in_signature_parent env p (ModuleName.of_string name)
              >>= module_lookup_to_signature_lookup env
              >>= label_parent_res_of_sig_res);
            (fun () ->
              module_type_in_signature_parent env p
                (ModuleTypeName.of_string name)
              >>= module_type_lookup_to_signature_lookup env
              >>= label_parent_res_of_sig_res);
            (fun () ->
              datatype_in_signature_parent env p (TypeName.of_string name)
              >>= fun r -> Some (`T r));
            (* TODO: Class *)
            (* TODO: ClassType *)
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
        return (`Page (`Identifier p.Odoc_model.Lang.Page.name, labels))

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
          module_in_env env name >>= module_lookup_to_signature_lookup env
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
              module_of_element env e >>= module_lookup_to_signature_lookup env
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
                (module_of_component env (Component.Delayed.get m)
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

and resolve_datatype_reference :
    Env.t -> DataType.t -> datatype_lookup_result option =
 fun env r ->
  match r with
  | `Resolved _ -> failwith "TODO"
  | `Root (name, (`TType | `TUnknown)) -> datatype_in_env env name
  | `Type (parent, name) -> datatype_in_signature_parent' env parent name
  | `Dot (parent, name) -> datatype_in_label_parent' env parent name

(** Value *)

and value_in_env env name : value_lookup_result option =
  Env.lookup_value_by_name (UnitName.to_string name) env >>= function
  | `Value (id, _x) -> return (`Identifier id)
  | `External (id, _x) -> return (`Identifier id)

and value_of_component _env ~parent_ref name : value_lookup_result option =
  Some (`Value (parent_ref, ValueName.of_string name))

and external_of_component _env ~parent_ref name : value_lookup_result option =
  (* Should add an [`External] reference ? *)
  Some (`Value (parent_ref, ValueName.of_string name))

and value_in_signature_parent' env parent name : value_lookup_result option =
  resolve_signature_reference env parent >>= fun (parent', _, sg) ->
  Find.opt_value_in_sig sg (ValueName.to_string name) >>= fun _ ->
  Some (`Value (parent', name))

(** Label *)

and label_in_env env name : Resolved.Label.t option =
  Env.lookup_label_by_name (UnitName.to_string name) env >>= fun (`Label id) ->
  Some (`Identifier id)

and label_in_page _env (`Page (_, p)) name : Resolved.Label.t option =
  try Some (`Identifier (List.assoc name p)) with Not_found -> None

and label_of_component _env ~parent_ref name : Resolved.Label.t option =
  Some
    (`Label ((parent_ref :> Resolved.LabelParent.t), LabelName.of_string name))

and label_in_label_parent' env parent name : Resolved.Label.t option =
  resolve_label_parent_reference env parent >>= function
  | `S (p, _, sg) ->
      Find.opt_label_in_sig sg (LabelName.to_string name) >>= fun _ ->
      Some (`Label ((p :> Resolved.LabelParent.t), name))
  | `T _ | `C _ | `CT _ -> None
  | `Page _ as page -> label_in_page env page (LabelName.to_string name)

(** Extension constructor *)

and extension_in_env env name : Resolved.Constructor.t option =
  Env.lookup_extension_by_name (UnitName.to_string name) env
  >>= fun (`Extension (id, _)) -> Some (`Identifier id :> Resolved.Constructor.t)

and extension_of_component _env ~parent_ref name : Resolved.Constructor.t option
    =
  Some (`Extension (parent_ref, ExtensionName.of_string name))

and extension_in_signature_parent' env parent name :
    Resolved.Constructor.t option =
  resolve_signature_reference env parent >>= fun (parent', parent_cp, sg) ->
  let sg = Tools.prefix_signature (parent_cp, sg) in
  Find.extension_in_sig sg (ExtensionName.to_string name) >>= fun _ ->
  Some (`Extension (parent', name))

(** Exception *)

and exception_in_env env name : Resolved.Exception.t option =
  Env.lookup_exception_by_name (UnitName.to_string name) env
  >>= fun (`Exception (id, _)) -> Some (`Identifier id)

and exception_of_component _env ~parent_ref name : Resolved.Exception.t option =
  Some (`Exception (parent_ref, ExceptionName.of_string name))

and exception_in_signature_parent' env parent name : Resolved.Exception.t option
    =
  resolve_signature_reference env parent >>= fun (parent', parent_cp, sg) ->
  let sg = Tools.prefix_signature (parent_cp, sg) in
  Find.exception_in_sig sg (ExceptionName.to_string name) >>= fun _ ->
  Some (`Exception (parent', name))

(** Constructor *)

and constructor_in_env env name : Resolved.Constructor.t option =
  Env.lookup_constructor_by_name (UnitName.to_string name) env
  >>= fun (`Constructor (id, _)) ->
  Some (`Identifier id :> Resolved.Constructor.t)

and constructor_in_datatype _env ((parent', t) : datatype_lookup_result) name :
    Resolved.Constructor.t option =
  Find.any_in_type t (ConstructorName.to_string name) >>= function
  | `Constructor _ -> Some (`Constructor (parent', name))
  | `Field _ -> None

and constructor_in_datatype' env parent name =
  resolve_datatype_reference env parent >>= fun p ->
  constructor_in_datatype env p name

and constructor_of_component _env parent name =
  Some (`Constructor (parent, ConstructorName.of_string name))

(** Field *)

and field_in_env env name : Resolved.Field.t option =
  Env.lookup_field_by_name (UnitName.to_string name) env
  >>= fun (`Field (id, _)) -> Some (`Identifier id :> Resolved.Field.t)

and field_in_parent' env parent name : Resolved.Field.t option =
  resolve_label_parent_reference env (parent : Parent.t :> LabelParent.t)
  >>= function
  | `S (parent', parent_cp, sg) -> (
      let sg = Tools.prefix_signature (parent_cp, sg) in
      Find.any_in_type_in_sig sg (FieldName.to_string name) >>= function
      | _, `Constructor _ -> None
      | typ_name, `Field _ -> Some (`Field (`Type (parent', typ_name), name)) )
  | `T (parent', t) -> (
      Find.any_in_type t (FieldName.to_string name) >>= function
      | `Constructor _ -> None
      | `Field _ -> Some (`Field ((parent' :> Resolved.Parent.t), name)) )
  | `C _ | `CT _ | `Page _ -> None

and field_of_component _env parent name : Resolved.Field.t option =
  Some
    (`Field
      ( (parent : Resolved.DataType.t :> Resolved.Parent.t),
        FieldName.of_string name ))

(** Method *)

(* TODO: Resolve methods in env *)
and method_in_env _env _name = None

and method_in_label_parent env parent name : Resolved.Method.t option =
  match parent with
  | (`T _ | `C _ | `CT _) as p ->
      type_lookup_to_class_signature_lookup env p >>= fun (parent', cs) ->
      Find.method_in_class_signature cs (MethodName.to_string name) >>= fun _ ->
      Some (`Method (parent', name))
  | `S _ | `Page _ -> None

(***)

let resolved1 r = Some (r :> Resolved.t)

let resolved3 (r, _, _) = resolved1 r

and resolved2 (r, _) = resolved1 r

let resolve_reference_dot_sg env ~parent_path ~parent_ref ~parent_sg name =
  let parent_path = Tools.reresolve_parent env parent_path in
  let parent_sg = Tools.prefix_signature (parent_path, parent_sg) in
  Find.any_in_sig parent_sg name >>= function
  | `Module (_, _, m) ->
      let name = ModuleName.of_string name in
      resolved3
        (module_of_component env (Component.Delayed.get m)
           (`Module (parent_path, name))
           (`Module (parent_ref, name)))
  | `ModuleType (_, mt) ->
      let name = ModuleTypeName.of_string name in
      resolved3
        (module_type_of_component env (Component.Delayed.get mt)
           (`ModuleType (parent_path, name))
           (`ModuleType (parent_ref, name)))
  | `Type (_, _, t) ->
      datatype_of_component env (Component.Delayed.get t) ~parent_ref name
      >>= resolved2
  | `Class (_, _, c) -> class_of_component env c ~parent_ref name >>= resolved2
  | `ClassType (_, _, ct) ->
      classtype_of_component env ct ~parent_ref name >>= resolved2
  | `Value _ -> value_of_component env ~parent_ref name >>= resolved1
  | `External _ -> external_of_component env ~parent_ref name >>= resolved1
  | `Label _ -> label_of_component env ~parent_ref name >>= resolved1
  | `Constructor (typ_name, _, _) ->
      let parent = `Type (parent_ref, typ_name) in
      constructor_of_component env parent name >>= resolved1
  | `Exception _ -> exception_of_component env ~parent_ref name >>= resolved1
  | `ExtConstructor _ ->
      extension_of_component env ~parent_ref name >>= resolved1
  | `Field (typ_name, _, _) ->
      let parent = `Type (parent_ref, typ_name) in
      field_of_component env parent name >>= resolved1
  | _ -> None

let resolve_reference_dot_page env page name =
  label_in_page env page name >>= resolved1

let resolve_reference_dot_type env ~parent_ref t name =
  Find.any_in_type t name >>= function
  | `Constructor _ -> constructor_of_component env parent_ref name >>= resolved1
  | `Field _ -> field_of_component env parent_ref name >>= resolved1

let resolve_reference_dot env parent name =
  resolve_label_parent_reference env parent >>= function
  | `S (parent_ref, parent_path, parent_sg) ->
      resolve_reference_dot_sg ~parent_path ~parent_ref ~parent_sg env name
  | `T (parent_ref, t) -> resolve_reference_dot_type env ~parent_ref t name
  | `C _ | `CT _ -> (* TODO *) None
  | `Page _ as page -> resolve_reference_dot_page env page name

let resolve_reference : Env.t -> t -> Resolved.t option =
  let resolved = resolved3 in
  fun env r ->
    match r with
    | `Root (name, `TUnknown) -> (
        let identifier id =
          return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        in
        match Env.lookup_any_by_name (UnitName.to_string name) env with
        | (`Module (_, _) as e) :: _ -> module_of_element env e >>= resolved
        | (`ModuleType (_, _) as e) :: _ ->
            module_type_of_element env e >>= resolved
        | `Value (id, _) :: _ -> identifier id
        | `Type (id, _) :: _ -> identifier id
        | `Label id :: _ -> identifier id
        | `Class (id, _) :: _ -> identifier id
        | `ClassType (id, _) :: _ -> identifier id
        | `External (id, _) :: _ -> identifier id
        | `Constructor (id, _) :: _ -> identifier id
        | `Exception (id, _) :: _ -> identifier id
        | `Extension (id, _) :: _ -> identifier id
        | `Field (id, _) :: _ -> identifier id
        | [] -> None )
    | `Resolved r -> Some r
    | `Root (name, `TModule) -> module_in_env env name >>= resolved
    | `Module (parent, name) ->
        module_in_signature_parent' env parent name >>= resolved
    | `Root (name, `TModuleType) -> module_type_in_env env name >>= resolved
    | `ModuleType (parent, name) ->
        module_type_in_signature_parent' env parent name >>= resolved
    | `Root (name, `TType) -> (
        type_in_env env name >>= function
        | `T (r, _) -> resolved1 r
        | `C (r, _) -> resolved1 r
        | `CT (r, _) -> resolved1 r )
    | `Type (parent, name) ->
        datatype_in_signature_parent' env parent name >>= resolved2
    | `Class (parent, name) ->
        class_in_signature_parent' env parent name >>= resolved2
    | `ClassType (parent, name) ->
        classtype_in_signature_parent' env parent name >>= resolved2
    | `Root (name, `TValue) -> value_in_env env name >>= resolved1
    | `Value (parent, name) ->
        value_in_signature_parent' env parent name >>= resolved1
    | `Root (name, `TLabel) -> label_in_env env name >>= resolved1
    | `Label (parent, name) ->
        label_in_label_parent' env parent name >>= resolved1
    | `Root (name, `TPage) -> (
        match Env.lookup_page (UnitName.to_string name) env with
        | Some p ->
            Some (`Identifier (p.Odoc_model.Lang.Page.name :> Identifier.t))
        | None -> None )
    | `Dot (parent, name) -> resolve_reference_dot env parent name
    | `Root (name, `TConstructor) -> constructor_in_env env name >>= resolved1
    | `Constructor (parent, name) ->
        constructor_in_datatype' env parent name >>= resolved1
    | `Root (name, `TException) -> exception_in_env env name >>= resolved1
    | `Exception (parent, name) ->
        exception_in_signature_parent' env parent name >>= resolved1
    | `Root (name, `TExtension) -> extension_in_env env name >>= resolved1
    | `Extension (parent, name) ->
        extension_in_signature_parent' env parent name >>= resolved1
    | `Root (name, `TField) -> field_in_env env name >>= resolved1
    | `Field (parent, name) -> field_in_parent' env parent name >>= resolved1
    | `Root (name, `TMethod) -> method_in_env env name >>= resolved1
    | `Method (parent, name) ->
        resolve_label_parent_reference env (parent :> LabelParent.t)
        >>= fun p -> method_in_label_parent env p name >>= resolved1
    | _ -> None
