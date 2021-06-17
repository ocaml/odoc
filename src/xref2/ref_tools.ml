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

let ref_kind_of_element = function
  | `Module _ -> "module"
  | `ModuleType _ -> "module-type"
  | `Type _ -> "type"
  | `Value _ | `External _ -> "val"
  | `Label _ -> "section"
  | `Class _ -> "class"
  | `ClassType _ -> "class-type"
  | `Constructor _ -> "constructor"
  | `Exception _ -> "exception"
  | `Extension _ -> "extension"
  | `Field _ -> "field"

let ref_kind_of_find = function
  | `FModule _ | `FModule_subst _ -> "module"
  | `FModuleType _ | `FModuleType_subst _ -> "module-type"
  | `FType _ | `FType_subst _ -> "type"
  | `FValue _ | `FExternal _ -> "val"
  | `FLabel _ -> "section"
  | `FClass _ -> "class"
  | `FClassType _ -> "class-type"
  | `FConstructor _ | `In_type (_, _, `FConstructor _) -> "constructor"
  | `FExn _ -> "exception"
  | `FExt _ -> "extension"
  | `FField _ | `In_type (_, _, `FField _) -> "field"
  | `FMethod _ -> "method"
  | `FInstance_variable _ -> "instance-variable"

let ambiguous_ref_warning name results =
  let pp_sep pp () = Format.fprintf pp ", "
  and pp_kind pp r = Format.fprintf pp "%s-%s" r name in
  Lookup_failures.report ~kind:`Warning
    "Reference to '%s' is ambiguous. Please specify its kind: %a." name
    (Format.pp_print_list ~pp_sep pp_kind)
    results

let env_lookup_by_name scope name env =
  match Env.lookup_by_name scope name env with
  | Ok x -> Some x
  | Error (`Ambiguous (hd, tl)) ->
      ambiguous_ref_warning name (List.map ref_kind_of_element (hd :: tl));
      Some hd
  | Error `Not_found -> None

let find_ambiguous find sg name =
  match find sg name with
  | [ x ] -> Some x
  | x :: _ as results ->
      ambiguous_ref_warning name (List.map ref_kind_of_find results);
      Some x
  | [] -> None

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

module M = struct
  (** Module *)

  type t = module_lookup_result

  let of_component env m base_path' base_ref' : t =
    let base_path, base_ref =
      if m.Component.Module.hidden then (`Hidden base_path', `Hidden base_ref')
      else (base_path', base_ref')
    in
    let p, r =
      match Tools.get_module_path_modifiers env ~add_canonical:true m with
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

  let in_signature env ((parent, parent_cp, sg) : signature_lookup_result) name
      : t option =
    let parent_cp = Tools.reresolve_parent env parent_cp in
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.module_in_sig sg name >>= fun (`FModule (name, m)) ->
    Some
      (of_component env m (`Module (parent_cp, name)) (`Module (parent, name)))

  let of_element env (`Module (id, m)) : t =
    let m = Component.Delayed.get m in
    let base = `Identifier (id :> Identifier.Path.Module.t) in
    of_component env m base base

  let lookup_root_module env name =
    match Env.lookup_root_module name env with
    | Some (Env.Resolved (_, id, m)) ->
        let base = `Identifier (id :> Identifier.Path.Module.t) in
        Some (of_component env m base base)
    | _ -> None

  let in_env env name : t option =
    match env_lookup_by_name Env.s_module name env with
    | Some e -> Some (of_element env e)
    | None -> lookup_root_module env name
end

module MT = struct
  (** Module type *)

  type t = module_type_lookup_result

  let of_component _env mt base_path base_ref : t = (base_ref, base_path, mt)

  let in_signature env ((parent', parent_cp, sg) : signature_lookup_result) name
      : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.module_type_in_sig sg name >>= fun (`FModuleType (name, mt)) ->
    Some
      (of_component env mt
         (`ModuleType (parent_cp, name))
         (`ModuleType (parent', name)))

  let of_element _env (`ModuleType (id, mt)) : t =
    (`Identifier id, `Identifier id, mt)

  let in_env env name : t option =
    env_lookup_by_name Env.s_module_type name env >>= fun e ->
    Some (of_element env e)
end

module CL = struct
  (** Class *)

  type t = class_lookup_result

  let of_element _env (`Class (id, t)) : t = (`Identifier id, t)

  let in_env env name =
    env_lookup_by_name Env.s_class name env >>= fun e -> Some (of_element env e)

  let of_component _env c ~parent_ref name : t option =
    Some (`Class (parent_ref, name), c)
end

module CT = struct
  type t = class_type_lookup_result

  let of_element _env (`ClassType (id, t)) : class_type_lookup_result =
    ((`Identifier id :> Resolved.ClassType.t), t)

  let in_env env name =
    env_lookup_by_name Env.s_class_type name env >>= fun e ->
    Some (of_element env e)

  let of_component _env ct ~parent_ref name : t option =
    Some (`ClassType (parent_ref, name), ct)
end

module DT = struct
  (** Datatype *)

  type t = datatype_lookup_result

  let of_component _env t ~parent_ref name : t option =
    Some (`Type (parent_ref, name), t)

  let of_element _env (`Type (id, t)) : t = (`Identifier id, t)

  let in_env env name : t option =
    env_lookup_by_name Env.s_type name env >>= fun e -> Some (of_element env e)

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.datatype_in_sig sg name >>= fun (`FType (name, t)) ->
    Some (`Type (parent', name), t)
end

module T = struct
  (** Type *)

  type t = type_lookup_result

  let of_element env : _ -> t = function
    | `Type _ as e -> `T (DT.of_element env e)
    | `Class _ as e -> `C (CL.of_element env e)
    | `ClassType _ as e -> `CT (CT.of_element env e)

  let in_env env name : t option =
    env_lookup_by_name Env.s_datatype name env >>= fun e ->
    Some (of_element env e)

  (* Don't handle name collisions between class, class types and type decls *)
  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.type_in_sig sg name >>= function
    | `FType (name, t) -> Some (`T (`Type (parent', name), t))
    | `FClass (name, c) -> Some (`C (`Class (parent', name), c))
    | `FClassType (name, ct) -> Some (`CT (`ClassType (parent', name), ct))
end

module V = struct
  (** Value *)

  type t = value_lookup_result

  let in_env env name : t option =
    env_lookup_by_name Env.s_value name env >>= function
    | `Value (id, _x) -> return (`Identifier id)
    | `External (id, _x) -> return (`Identifier id)

  let of_component _env ~parent_ref name : t option =
    Some (`Value (parent_ref, name))

  let external_of_component _env ~parent_ref name : t option =
    (* Should add an [`External] reference ? *)
    Some (`Value (parent_ref, name))

  let in_signature _env ((parent', _, sg) : signature_lookup_result) name :
      t option =
    find_ambiguous Find.value_in_sig sg (ValueName.to_string name) >>= fun _ ->
    Some (`Value (parent', name))
end

module L = struct
  (** Label *)

  type t = Resolved.Label.t

  let in_env env name : t option =
    env_lookup_by_name Env.s_label name env >>= fun (`Label id) ->
    Some (`Identifier id)

  let in_page _env (`Page (_, p)) name : t option =
    try Some (`Identifier (List.assoc name p)) with Not_found -> None

  let of_component _env ~parent_ref label : t option =
    Some
      (`Label
        ((parent_ref :> Resolved.LabelParent.t), Ident.Name.typed_label label))

  let in_label_parent env (parent : label_parent_lookup_result) name : t option
      =
    match parent with
    | `S (p, _, sg) ->
        find_ambiguous Find.label_in_sig sg (LabelName.to_string name)
        >>= fun _ -> Some (`Label ((p :> Resolved.LabelParent.t), name))
    | `T _ | `C _ | `CT _ -> None
    | `Page _ as page -> in_page env page (LabelName.to_string name)
end

module EC = struct
  (** Extension constructor *)

  type t = Resolved.Constructor.t

  let in_env env name : t option =
    env_lookup_by_name Env.s_extension name env >>= fun (`Extension (id, _)) ->
    Some (`Identifier id :> t)

  let of_component _env ~parent_ref name : t option =
    Some (`Extension (parent_ref, ExtensionName.make_std name))

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.extension_in_sig sg (ExtensionName.to_string name) >>= fun _ ->
    Some (`Extension (parent', name))
end

module EX = struct
  (** Exception *)

  type t = Resolved.Exception.t

  let in_env env name : t option =
    env_lookup_by_name Env.s_exception name env >>= fun (`Exception (id, _)) ->
    Some (`Identifier id)

  let of_component _env ~parent_ref name : t option =
    Some (`Exception (parent_ref, name))

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.exception_in_sig sg (ExceptionName.to_string name) >>= fun _ ->
    Some (`Exception (parent', name))
end

module CS = struct
  type t = Resolved.Constructor.t
  (** Constructor *)

  let in_env env name : t option =
    env_lookup_by_name Env.s_constructor name env
    >>= fun (`Constructor (id, _)) -> Some (`Identifier id :> t)

  let in_datatype _env ((parent', t) : datatype_lookup_result) name : t option =
    Find.any_in_type t (ConstructorName.to_string name) >>= function
    | `FConstructor _ -> Some (`Constructor (parent', name))
    | `FField _ -> None

  let of_component _env parent name : t option =
    Some (`Constructor (parent, ConstructorName.make_std name))
end

module F = struct
  (** Field *)

  type t = Resolved.Field.t

  let in_env env name : t option =
    env_lookup_by_name Env.s_field name env >>= fun (`Field (id, _)) ->
    Some (`Identifier id :> t)

  let in_parent _env (parent : label_parent_lookup_result) name : t option =
    match parent with
    | `S (parent', parent_cp, sg) -> (
        let sg = Tools.prefix_signature (parent_cp, sg) in
        find_ambiguous Find.any_in_type_in_sig sg (FieldName.to_string name)
        >>= function
        | `In_type (_, _, `FConstructor _) -> None
        | `In_type (typ_name, _, `FField _) ->
            Some (`Field (`Type (parent', typ_name), name)))
    | `T (parent', t) -> (
        Find.any_in_type t (FieldName.to_string name) >>= function
        | `FConstructor _ -> None
        | `FField _ -> Some (`Field ((parent' :> Resolved.Parent.t), name)))
    | `C _ | `CT _ | `Page _ -> None

  let of_component _env parent name : t option =
    Some
      (`Field
        ( (parent : Resolved.DataType.t :> Resolved.Parent.t),
          FieldName.make_std name ))
end

module MM = struct
  (** Method *)

  type t = Resolved.Method.t

  (* TODO: Resolve methods in env *)
  let in_env _env _name = None

  let in_class_signature _env (parent', cs) name : t option =
    Find.method_in_class_signature cs (MethodName.to_string name) >>= fun _ ->
    Some (`Method (parent', name))

  let of_component _env parent' name : t option = Some (`Method (parent', name))
end

module MV = struct
  type t = Resolved.InstanceVariable.t
  (** Instance variable *)

  (* TODO: Resolve instance variables in env *)
  let in_env _env _name = None

  let in_class_signature _env (parent', cs) name : t option =
    Find.instance_variable_in_class_signature cs
      (InstanceVariableName.to_string name)
    >>= fun _ -> Some (`InstanceVariable (parent', name))

  let of_component _env parent' name : t option =
    Some (`InstanceVariable (parent', name))
end

module LP = struct
  (** Label parent *)

  type t = label_parent_lookup_result

  let of_element env : _ -> t option = function
    | `Module _ as e ->
        M.of_element env e |> module_lookup_to_signature_lookup env >>= fun r ->
        Some (`S r)
    | `ModuleType _ as e ->
        MT.of_element env e |> module_type_lookup_to_signature_lookup env
        >>= fun r -> Some (`S r)
    | `Type _ as e -> Some (`T (DT.of_element env e))
    | `Class _ as e -> Some (`C (CL.of_element env e))
    | `ClassType _ as e -> Some (`CT (CT.of_element env e))

  let in_env env name : t option =
    env_lookup_by_name Env.s_label_parent name env >>= of_element env

  let in_signature env ((parent', parent_cp, sg) : signature_lookup_result) name
      : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find_ambiguous Find.label_parent_in_sig sg name >>= function
    | `FModule (name, m) ->
        module_lookup_to_signature_lookup env
          (M.of_component env m
             (`Module (parent_cp, name))
             (`Module (parent', name)))
        >>= fun s -> Some (`S s)
    | `FModuleType (name, mt) ->
        module_type_lookup_to_signature_lookup env
          (MT.of_component env mt
             (`ModuleType (parent_cp, name))
             (`ModuleType (parent', name)))
        >>= fun s -> Some (`S s)
    | `FType (name, t) ->
        DT.of_component env ~parent_ref:parent' t name >>= fun t -> Some (`T t)
    | `FClass (name, c) ->
        CL.of_component env ~parent_ref:parent' c name >>= fun c -> Some (`C c)
    | `FClassType (name, ct) ->
        CT.of_component env ~parent_ref:parent' ct name >>= fun ct ->
        Some (`CT ct)
end

let rec resolve_label_parent_reference :
    Env.t -> LabelParent.t -> label_parent_lookup_result option =
  let open Utils.OptionMonad in
  fun env r ->
    let label_parent_res_of_sig_res :
        signature_lookup_result -> label_parent_lookup_result option =
     fun (r', cp, sg) -> return (`S (r', cp, sg))
    and label_parent_res_of_type_res :
        type_lookup_result option -> label_parent_lookup_result option =
     fun r -> (r :> label_parent_lookup_result option)
    in
    match r with
    | `Resolved _ -> failwith "unimplemented"
    | `Root (name, `TUnknown) -> LP.in_env env name
    | (`Module _ | `ModuleType _ | `Root (_, (`TModule | `TModuleType))) as sr
      ->
        resolve_signature_reference env sr >>= label_parent_res_of_sig_res
    | `Root (name, `TType) -> T.in_env env name |> label_parent_res_of_type_res
    | `Type (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        T.in_signature env p (TypeName.to_string name)
        |> label_parent_res_of_type_res
    | `Root (name, `TClass) -> CL.in_env env name >>= fun r -> Some (`C r)
    | `Class (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        T.in_signature env p (ClassName.to_string name)
        >>= class_lookup_result_of_type
        >>= fun r -> Some (`C r)
    | `Root (name, `TClassType) -> CT.in_env env name >>= fun r -> Some (`CT r)
    | `ClassType (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        T.in_signature env p (ClassTypeName.to_string name)
        >>= class_type_lookup_result_of_type
        >>= fun r -> Some (`CT r)
    | `Dot (parent, name) ->
        resolve_label_parent_reference env parent
        >>= signature_lookup_result_of_label_parent
        >>= fun p -> LP.in_signature env p name
    | `Root (name, `TPage) | `Root (name, `TChildPage) ->
        Env.lookup_page name env >>= fun p ->
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
    | `Root (name, `TChildModule) ->
        resolve_signature_reference env (`Root (name, `TModule))
        >>= label_parent_res_of_sig_res

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
          M.in_env env name >>= module_lookup_to_signature_lookup env
      | `Module (parent, name) ->
          resolve_signature_reference env parent >>= fun p ->
          M.in_signature env p (ModuleName.to_string name)
          >>= module_lookup_to_signature_lookup env
      | `Root (name, `TModuleType) ->
          MT.in_env env name >>= module_type_lookup_to_signature_lookup env
      | `ModuleType (parent, name) ->
          resolve_signature_reference env parent >>= fun p ->
          MT.in_signature env p (ModuleTypeName.to_string name)
          >>= module_type_lookup_to_signature_lookup env
      | `Root (name, `TUnknown) -> (
          env_lookup_by_name Env.s_signature name env >>= function
          | `Module (_, _) as e ->
              module_lookup_to_signature_lookup env (M.of_element env e)
          | `ModuleType (_, _) as e ->
              module_type_lookup_to_signature_lookup env (MT.of_element env e))
      | `Dot (parent, name) -> (
          resolve_label_parent_reference env parent
          >>= signature_lookup_result_of_label_parent
          >>= fun (parent, parent_cp, sg) ->
          let parent_cp = Tools.reresolve_parent env parent_cp in
          let sg = Tools.prefix_signature (parent_cp, sg) in
          find_ambiguous Find.signature_in_sig sg name >>= function
          | `FModule (name, m) ->
              module_lookup_to_signature_lookup env
                (M.of_component env m
                   (`Module (parent_cp, name))
                   (`Module (parent, name)))
          | `FModuleType (name, mt) ->
              module_type_lookup_to_signature_lookup env
                (MT.of_component env mt
                   (`ModuleType (parent_cp, name))
                   (`ModuleType (parent, name))))
    in
    resolve env'

and resolve_datatype_reference :
    Env.t -> DataType.t -> datatype_lookup_result option =
 fun env r ->
  match r with
  | `Resolved _ -> failwith "TODO"
  | `Root (name, (`TType | `TUnknown)) -> DT.in_env env name
  | `Type (parent, name) ->
      resolve_signature_reference env parent >>= fun p ->
      DT.in_signature env p (TypeName.to_string name)
  | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent
      >>= fun p -> DT.in_signature env p name

and resolve_module_reference env (r : Module.t) : M.t option =
  match r with
  | `Resolved _r -> failwith "What's going on!?"
  (*        Some (resolve_resolved_module_reference env r ~add_canonical)*)
  | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent
      >>= fun p -> M.in_signature env p name
  | `Module (parent, name) ->
      resolve_signature_reference env parent >>= fun p ->
      M.in_signature env p (ModuleName.to_string name)
  | `Root (name, _) -> M.in_env env name

let resolve_class_signature_reference env (r : ClassSignature.t) =
  (* Casting from ClassSignature to LabelParent.
     TODO: Add [resolve_class_signature_reference] when it's easier to implement. *)
  resolve_label_parent_reference env (r :> LabelParent.t) >>= function
  | (`T _ | `C _ | `CT _) as p -> type_lookup_to_class_signature_lookup env p
  | `S _ | `Page _ -> None

(***)

let resolved1 r = Some (r :> Resolved.t)

let resolved3 (r, _, _) = resolved1 r

and resolved2 (r, _) = resolved1 r

let resolved_type_lookup = function
  | `T (r, _) -> resolved1 r
  | `C (r, _) -> resolved1 r
  | `CT (r, _) -> resolved1 r

let resolve_reference_dot_sg env ~parent_path ~parent_ref ~parent_sg name =
  let parent_path = Tools.reresolve_parent env parent_path in
  let parent_sg = Tools.prefix_signature (parent_path, parent_sg) in
  find_ambiguous Find.any_in_sig parent_sg name >>= function
  | `FModule (name, m) ->
      resolved3
        (M.of_component env m
           (`Module (parent_path, name))
           (`Module (parent_ref, name)))
  | `FModuleType (name, mt) ->
      resolved3
        (MT.of_component env mt
           (`ModuleType (parent_path, name))
           (`ModuleType (parent_ref, name)))
  | `FType (name, t) -> DT.of_component env t ~parent_ref name >>= resolved2
  | `FClass (name, c) -> CL.of_component env c ~parent_ref name >>= resolved2
  | `FClassType (name, ct) ->
      CT.of_component env ct ~parent_ref name >>= resolved2
  | `FValue (name, _) -> V.of_component env ~parent_ref name >>= resolved1
  | `FExternal (name, _) ->
      V.external_of_component env ~parent_ref name >>= resolved1
  | `FLabel label -> L.of_component env ~parent_ref label >>= resolved1
  | `FExn (name, _) -> EX.of_component env ~parent_ref name >>= resolved1
  | `FExt _ -> EC.of_component env ~parent_ref name >>= resolved1
  | `In_type (typ_name, _, r) -> (
      let parent = `Type (parent_ref, typ_name) in
      match r with
      | `FConstructor _ -> CS.of_component env parent name >>= resolved1
      | `FField _ -> F.of_component env parent name >>= resolved1)
  | `FModule_subst _ | `FType_subst _ | `FModuleType_subst _ -> None

let resolve_reference_dot_page env page name =
  L.in_page env page name >>= resolved1

let resolve_reference_dot_type env ~parent_ref t name =
  Find.any_in_type t name >>= function
  | `FConstructor _ -> CS.of_component env parent_ref name >>= resolved1
  | `FField _ -> F.of_component env parent_ref name >>= resolved1

let resolve_reference_dot_class env p name =
  type_lookup_to_class_signature_lookup env p >>= fun (parent_ref, cs) ->
  find_ambiguous Find.any_in_class_signature cs name >>= function
  | `FMethod (name, _) -> MM.of_component env parent_ref name >>= resolved1
  | `FInstance_variable (name, _) ->
      MV.of_component env parent_ref name >>= resolved1

let resolve_reference_dot env parent name =
  resolve_label_parent_reference env parent >>= function
  | `S (parent_ref, parent_path, parent_sg) ->
      resolve_reference_dot_sg ~parent_path ~parent_ref ~parent_sg env name
  | `T (parent_ref, t) -> resolve_reference_dot_type env ~parent_ref t name
  | (`C _ | `CT _) as p -> resolve_reference_dot_class env p name
  | `Page _ as page -> resolve_reference_dot_page env page name

(** Warnings may be generated with [Error.implicit_warning] *)
let resolve_reference : Env.t -> t -> Resolved.t option =
  let resolved = resolved3 in
  fun env r ->
    match r with
    | `Root (name, `TUnknown) -> (
        let identifier id = return (`Identifier (id :> Identifier.t)) in
        env_lookup_by_name Env.s_any name env >>= function
        | `Module (_, _) as e -> resolved (M.of_element env e)
        | `ModuleType (_, _) as e -> resolved (MT.of_element env e)
        | `Value (id, _) -> identifier id
        | `Type (id, _) -> identifier id
        | `Label id -> identifier id
        | `Class (id, _) -> identifier id
        | `ClassType (id, _) -> identifier id
        | `External (id, _) -> identifier id
        | `Constructor (id, _) -> identifier id
        | `Exception (id, _) -> identifier id
        | `Extension (id, _) -> identifier id
        | `Field (id, _) -> identifier id)
    | `Root (name, `TChildPage) -> (
        match Env.lookup_page name env with
        | Some p -> Some (`Identifier (p.name :> Identifier.t))
        | None -> None)
    | `Root (name, `TChildModule) -> (
        match Env.lookup_root_module name env with
        | Some (Resolved (_, id, _)) -> Some (`Identifier (id :> Identifier.t))
        | Some Forward | None -> None)
    | `Resolved r -> Some r
    | `Root (name, `TModule) -> M.in_env env name >>= resolved
    | `Module (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        M.in_signature env p (ModuleName.to_string name) >>= resolved
    | `Root (name, `TModuleType) -> MT.in_env env name >>= resolved
    | `ModuleType (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        MT.in_signature env p (ModuleTypeName.to_string name) >>= resolved
    | `Root (name, `TType) -> T.in_env env name >>= resolved_type_lookup
    | `Type (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        T.in_signature env p (TypeName.to_string name) >>= resolved_type_lookup
    | `Root (name, `TClass) -> CL.in_env env name >>= resolved2
    | `Class (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        T.in_signature env p (ClassName.to_string name)
        >>= class_lookup_result_of_type >>= resolved2
    | `Root (name, `TClassType) -> CT.in_env env name >>= resolved2
    | `ClassType (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        T.in_signature env p (ClassTypeName.to_string name)
        >>= class_type_lookup_result_of_type >>= resolved2
    | `Root (name, `TValue) -> V.in_env env name >>= resolved1
    | `Value (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        V.in_signature env p name >>= resolved1
    | `Root (name, `TLabel) -> L.in_env env name >>= resolved1
    | `Label (parent, name) ->
        resolve_label_parent_reference env parent >>= fun p ->
        L.in_label_parent env p name >>= resolved1
    | `Root (name, `TPage) -> (
        match Env.lookup_page name env with
        | Some p ->
            Some (`Identifier (p.Odoc_model.Lang.Page.name :> Identifier.t))
        | None -> None)
    | `Dot (parent, name) -> resolve_reference_dot env parent name
    | `Root (name, `TConstructor) -> CS.in_env env name >>= resolved1
    | `Constructor (parent, name) ->
        resolve_datatype_reference env parent >>= fun p ->
        CS.in_datatype env p name >>= resolved1
    | `Root (name, `TException) -> EX.in_env env name >>= resolved1
    | `Exception (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        EX.in_signature env p name >>= resolved1
    | `Root (name, `TExtension) -> EC.in_env env name >>= resolved1
    | `Extension (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        EC.in_signature env p name >>= resolved1
    | `Root (name, `TField) -> F.in_env env name >>= resolved1
    | `Field (parent, name) ->
        resolve_label_parent_reference env (parent : Parent.t :> LabelParent.t)
        >>= fun p -> F.in_parent env p name >>= resolved1
    | `Root (name, `TMethod) -> MM.in_env env name >>= resolved1
    | `Method (parent, name) ->
        resolve_class_signature_reference env parent >>= fun p ->
        MM.in_class_signature env p name >>= resolved1
    | `Root (name, `TInstanceVariable) -> MV.in_env env name >>= resolved1
    | `InstanceVariable (parent, name) ->
        resolve_class_signature_reference env parent >>= fun p ->
        MV.in_class_signature env p name >>= resolved1
