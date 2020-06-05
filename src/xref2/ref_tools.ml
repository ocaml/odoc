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

let ambiguous_ref_warning name results =
  let pp_sep pp () = Format.fprintf pp ", "
  and pp_kind pp r = Format.fprintf pp "%s-%s" (ref_kind_of_element r) name in
  Lookup_failures.report ~kind:`Warning
    "Reference to '%s' is ambiguous. Please specify its kind: %a." name
    (Format.pp_print_list ~pp_sep pp_kind)
    results

let env_lookup_by_name scope name env =
  match Env.lookup_by_name scope name env with
  | Ok x -> Some x
  | Error (`Ambiguous (hd, tl)) ->
      ambiguous_ref_warning name (hd :: tl);
      Some hd
  | Error `Not_found -> None

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

  let in_signature env ((parent, parent_cp, sg) : signature_lookup_result) name
      : t option =
    let parent_cp = Tools.reresolve_parent env parent_cp in
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.module_in_sig sg name >>= fun m ->
    Some
      (of_component env m (`Module (parent_cp, name)) (`Module (parent, name)))

  let of_element env (`Module (id, m)) : t option =
    let m = Component.Delayed.get m in
    let base = `Identifier (id :> Odoc_model.Paths.Identifier.Path.Module.t) in
    Some (of_component env m base base)

  let in_env env name : t option =
    match env_lookup_by_name Env.s_module name env with
    | Some e -> of_element env e
    | None -> (
        match Env.lookup_root_module name env with
        | Some (Env.Resolved (_, id, _, m)) -> of_element env (`Module (id, m))
        | _ -> None )
end

module MT = struct
  (** Module type *)

  type t = module_type_lookup_result

  let of_component _env mt base_path base_ref : t = (base_ref, base_path, mt)

  let in_signature env ((parent', parent_cp, sg) : signature_lookup_result) name
      : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.module_type_in_sig sg name >>= fun mt ->
    Some
      (of_component env mt
         (`ModuleType (parent_cp, name))
         (`ModuleType (parent', name)))

  let of_element _env (`ModuleType (id, mt)) : t option =
    Some (`Identifier id, `Identifier id, mt)

  let in_env env name : t option =
    env_lookup_by_name Env.s_module_type name env >>= of_element env
end

module CL = struct
  (** Class *)

  type t = class_lookup_result

  let of_element _env (`Class (id, t)) : t = (`Identifier id, t)

  let in_env env name =
    env_lookup_by_name Env.s_class name env >>= fun e -> Some (of_element env e)

  let of_component _env c ~parent_ref name : t option =
    Some (`Class (parent_ref, ClassName.of_string name), c)
end

module CT = struct
  type t = class_type_lookup_result

  let of_element _env (`ClassType (id, t)) : class_type_lookup_result =
    ((`Identifier id :> Resolved.ClassType.t), t)

  let in_env env name =
    env_lookup_by_name Env.s_class_type name env
    >>= fun e -> Some (of_element env e)

  let of_component _env ct ~parent_ref name : t option =
    Some (`ClassType (parent_ref, ClassTypeName.of_string name), ct)
end

module DT = struct
  (** Datatype *)

  type t = datatype_lookup_result

  let of_component _env t ~parent_ref name : t option =
    Some (`Type (parent_ref, TypeName.of_string name), t)

  let of_element _env (`Type (id, t)) : t = (`Identifier id, t)

  let in_env env name : t option =
    env_lookup_by_name Env.s_datatype name env >>= function
    | `Type _ as e -> Some (of_element env e)
    | _ -> None

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.datatype_in_sig sg name >>= fun t -> Some (`Type (parent', name), t)
end

module T = struct
  (** Type *)

  type t = type_lookup_result

  let in_env env name : t option =
    env_lookup_by_name Env.s_datatype name env >>= function
    | `Type (id, t) -> Some (`T (`Identifier id, t))
    | `Class _ as e -> Some (`C (CL.of_element env e))
    | `ClassType _ as e -> Some (`CT (CT.of_element env e))

  (* Don't handle name collisions between class, class types and type decls *)
  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.type_in_sig sg name >>= function
    | `T (name, t) -> Some (`T (`Type (parent', name), t))
    | `C (name, c) -> Some (`C (`Class (parent', name), c))
    | `CT (name, ct) -> Some (`CT (`ClassType (parent', name), ct))
end

module V = struct
  (** Value *)

  type t = value_lookup_result

  let in_env env name : t option =
    env_lookup_by_name Env.s_value name env >>= function
    | `Value (id, _x) -> return (`Identifier id)
    | `External (id, _x) -> return (`Identifier id)

  let of_component _env ~parent_ref name : t option =
    Some (`Value (parent_ref, ValueName.of_string name))

  let external_of_component _env ~parent_ref name : t option =
    (* Should add an [`External] reference ? *)
    Some (`Value (parent_ref, ValueName.of_string name))

  let in_signature _env ((parent', _, sg) : signature_lookup_result) name :
      t option =
    Find.opt_value_in_sig sg (ValueName.to_string name) >>= fun _ ->
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

  let of_component _env ~parent_ref name : t option =
    Some
      (`Label
        ((parent_ref :> Resolved.LabelParent.t), LabelName.of_string name))

  let in_label_parent env (parent : label_parent_lookup_result) name : t option
      =
    match parent with
    | `S (p, _, sg) ->
        Find.opt_label_in_sig sg (LabelName.to_string name) >>= fun _ ->
        Some (`Label ((p :> Resolved.LabelParent.t), name))
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
    Some (`Extension (parent_ref, ExtensionName.of_string name))

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
    Some (`Exception (parent_ref, ExceptionName.of_string name))

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
    | `Constructor _ -> Some (`Constructor (parent', name))
    | `Field _ -> None

  let of_component _env parent name : t option =
    Some (`Constructor (parent, ConstructorName.of_string name))
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
        Find.any_in_type_in_sig sg (FieldName.to_string name) >>= function
        | _, `Constructor _ -> None
        | typ_name, `Field _ -> Some (`Field (`Type (parent', typ_name), name))
        )
    | `T (parent', t) -> (
        Find.any_in_type t (FieldName.to_string name) >>= function
        | `Constructor _ -> None
        | `Field _ -> Some (`Field ((parent' :> Resolved.Parent.t), name)) )
    | `C _ | `CT _ | `Page _ -> None

  let of_component _env parent name : t option =
    Some
      (`Field
        ( (parent : Resolved.DataType.t :> Resolved.Parent.t),
          FieldName.of_string name ))
end

module MM = struct
  (** Method *)

  type t = Resolved.Method.t

  (* TODO: Resolve methods in env *)
  let in_env _env _name = None

  let in_class_signature _env (parent', cs) name : t option =
    Find.method_in_class_signature cs (MethodName.to_string name) >>= fun _ ->
    Some (`Method (parent', name))

  let of_component _env parent' name : t option =
    Some (`Method (parent', MethodName.of_string name))
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
    Some (`InstanceVariable (parent', InstanceVariableName.of_string name))
end

module LP = struct
  (** Label parent *)

  type t = label_parent_lookup_result

  let in_env env name : t option =
    env_lookup_by_name Env.s_label_parent name env >>= function
    | `Module _ as e ->
        M.of_element env e >>= module_lookup_to_signature_lookup env
        >>= fun r -> Some (`S r)
    | `ModuleType _ as e ->
        MT.of_element env e >>= module_type_lookup_to_signature_lookup env
        >>= fun r -> Some (`S r)
    | `Type _ as e -> Some (`T (DT.of_element env e))
    | `Class _ as e -> Some (`C (CL.of_element env e))
    | `ClassType _ as e -> Some (`CT (CT.of_element env e))

  let in_signature env ((parent', parent_cp, sg) : signature_lookup_result) name
      : t option =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    Find.label_parent_in_sig sg name >>= function
    | `M m ->
        let name = ModuleName.of_string name in
        module_lookup_to_signature_lookup env
          (M.of_component env m
             (`Module (parent_cp, name))
             (`Module (parent', name)))
        >>= fun s -> Some (`S s)
    | `MT mt ->
        let name = ModuleTypeName.of_string name in
        module_type_lookup_to_signature_lookup env
          (MT.of_component env mt
             (`ModuleType (parent_cp, name))
             (`ModuleType (parent', name)))
        >>= fun s -> Some (`S s)
    | `T t ->
        DT.of_component env ~parent_ref:parent' t name >>= fun t -> Some (`T t)
    | `C c ->
        CL.of_component env ~parent_ref:parent' c name >>= fun c -> Some (`C c)
    | `CT ct ->
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
    in
    match r with
    | `Resolved _ -> failwith "unimplemented"
    | `Root (name, `TUnknown) -> LP.in_env env name
    | (`Module _ | `ModuleType _ | `Root (_, (`TModule | `TModuleType))) as sr
      ->
        resolve_signature_reference env sr >>= label_parent_res_of_sig_res
    | `Root (name, `TType) -> DT.in_env env name >>= fun r -> Some (`T r)
    | `Type (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        DT.in_signature env p name >>= fun r -> Some (`T r)
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
    | `Root (name, _) ->
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
          M.in_signature env p name >>= module_lookup_to_signature_lookup env
      | `Root (name, `TModuleType) ->
          MT.in_env env name >>= module_type_lookup_to_signature_lookup env
      | `ModuleType (parent, name) ->
          resolve_signature_reference env parent >>= fun p ->
          MT.in_signature env p name
          >>= module_type_lookup_to_signature_lookup env
      | `Root (name, `TUnknown) -> (
          env_lookup_by_name Env.s_signature name env >>= function
          | `Module (_, _) as e ->
              M.of_element env e >>= module_lookup_to_signature_lookup env
          | `ModuleType (_, _) as e ->
              MT.of_element env e >>= module_type_lookup_to_signature_lookup env
          )
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
                (M.of_component env (Component.Delayed.get m)
                   (`Module (parent_cp, name))
                   (`Module (parent, name)))
          | `ModuleType (_, mt) ->
              let name = ModuleTypeName.of_string name in
              module_type_lookup_to_signature_lookup env
                (MT.of_component env (Component.Delayed.get mt)
                   (`ModuleType (parent_cp, name))
                   (`ModuleType (parent, name))) )
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
      DT.in_signature env p name
  | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent
      >>= fun p -> DT.in_signature env p (TypeName.of_string name)

and resolve_module_reference env (r : Module.t) : M.t option =
  match r with
  | `Resolved _r -> failwith "What's going on!?"
  (*        Some (resolve_resolved_module_reference env r ~add_canonical)*)
  | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent
      >>= fun p -> M.in_signature env p (ModuleName.of_string name)
  | `Module (parent, name) ->
      resolve_signature_reference env parent >>= fun p ->
      M.in_signature env p name
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

let resolve_reference_dot_sg env ~parent_path ~parent_ref ~parent_sg name =
  let parent_path = Tools.reresolve_parent env parent_path in
  let parent_sg = Tools.prefix_signature (parent_path, parent_sg) in
  Find.any_in_sig parent_sg name >>= function
  | `Module (_, _, m) ->
      let name = ModuleName.of_string name in
      resolved3
        (M.of_component env (Component.Delayed.get m)
           (`Module (parent_path, name))
           (`Module (parent_ref, name)))
  | `ModuleType (_, mt) ->
      let name = ModuleTypeName.of_string name in
      resolved3
        (MT.of_component env (Component.Delayed.get mt)
           (`ModuleType (parent_path, name))
           (`ModuleType (parent_ref, name)))
  | `Type (_, _, t) ->
      DT.of_component env (Component.Delayed.get t) ~parent_ref name
      >>= resolved2
  | `Class (_, _, c) -> CL.of_component env c ~parent_ref name >>= resolved2
  | `ClassType (_, _, ct) ->
      CT.of_component env ct ~parent_ref name >>= resolved2
  | `Value _ -> V.of_component env ~parent_ref name >>= resolved1
  | `External _ -> V.external_of_component env ~parent_ref name >>= resolved1
  | `Label _ -> L.of_component env ~parent_ref name >>= resolved1
  | `Constructor (typ_name, _, _) ->
      let parent = `Type (parent_ref, typ_name) in
      CS.of_component env parent name >>= resolved1
  | `Exception _ -> EX.of_component env ~parent_ref name >>= resolved1
  | `ExtConstructor _ -> EC.of_component env ~parent_ref name >>= resolved1
  | `Field (typ_name, _, _) ->
      let parent = `Type (parent_ref, typ_name) in
      F.of_component env parent name >>= resolved1
  | `ModuleSubstitution _ | `Removed _ | `TypeSubstitution _ -> None

let resolve_reference_dot_page env page name =
  L.in_page env page name >>= resolved1

let resolve_reference_dot_type env ~parent_ref t name =
  Find.any_in_type t name >>= function
  | `Constructor _ -> CS.of_component env parent_ref name >>= resolved1
  | `Field _ -> F.of_component env parent_ref name >>= resolved1

let resolve_reference_dot_class env p name =
  type_lookup_to_class_signature_lookup env p >>= fun (parent_ref, cs) ->
  Find.any_in_class_signature cs name >>= function
  | `Method _ -> MM.of_component env parent_ref name >>= resolved1
  | `InstanceVariable _ -> MV.of_component env parent_ref name >>= resolved1

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
        let identifier id =
          return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        in
        env_lookup_by_name Env.s_any name env >>= function
        | `Module (_, _) as e -> M.of_element env e >>= resolved
        | `ModuleType (_, _) as e -> MT.of_element env e >>= resolved
        | `Value (id, _) -> identifier id
        | `Type (id, _) -> identifier id
        | `Label id -> identifier id
        | `Class (id, _) -> identifier id
        | `ClassType (id, _) -> identifier id
        | `External (id, _) -> identifier id
        | `Constructor (id, _) -> identifier id
        | `Exception (id, _) -> identifier id
        | `Extension (id, _) -> identifier id
        | `Field (id, _) -> identifier id )
    | `Resolved r -> Some r
    | `Root (name, `TModule) -> M.in_env env name >>= resolved
    | `Module (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        M.in_signature env p name >>= resolved
    | `Root (name, `TModuleType) -> MT.in_env env name >>= resolved
    | `ModuleType (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        MT.in_signature env p name >>= resolved
    | `Root (name, `TType) -> (
        T.in_env env name >>= function
        | `T (r, _) -> resolved1 r
        | `C (r, _) -> resolved1 r
        | `CT (r, _) -> resolved1 r )
    | `Type (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        DT.in_signature env p name >>= resolved2
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
        | None -> None )
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
