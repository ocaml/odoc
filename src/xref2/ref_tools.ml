open Odoc_model.Paths
open Odoc_model.Names
open Reference
open Utils.ResultMonad

type module_lookup_result =
  Resolved.Module.t * Cpath.Resolved.module_ * Component.Module.t

type module_type_lookup_result =
  Resolved.ModuleType.t * Cpath.Resolved.module_type * Component.ModuleType.t

type signature_lookup_result =
  Resolved.Signature.t * Cpath.Resolved.parent * Component.Signature.t

type datatype_lookup_result = Resolved.DataType.t * Component.TypeDecl.t

type class_lookup_result = Resolved.Class.t * Component.Class.t

type class_type_lookup_result = Resolved.ClassType.t * Component.ClassType.t

type page_lookup_result = Resolved.Page.t * Odoc_model.Lang.Page.t

type type_lookup_result =
  [ `T of datatype_lookup_result
  | `C of class_lookup_result
  | `CT of class_type_lookup_result ]

type label_parent_lookup_result =
  [ `S of signature_lookup_result
  | type_lookup_result
  | `P of page_lookup_result ]

type fragment_type_parent_lookup_result =
  [ `S of signature_lookup_result | `T of datatype_lookup_result ]

type 'a ref_result =
  ('a, Errors.Tools_error.reference_lookup_error) Result.result
(** The result type for every functions in this module. *)

let kind_of_find_result = function
  | `S _ -> `S
  | `T _ -> `T
  | `C _ -> `C
  | `CT _ -> `CT
  | `P _ -> `Page

let wrong_kind_error expected r =
  Error (`Wrong_kind (expected, kind_of_find_result r))

let signature_lookup_result_of_label_parent : label_parent_lookup_result -> _ =
  function
  | `S r -> Ok r
  | r -> wrong_kind_error [ `S ] r

let class_lookup_result_of_type : type_lookup_result -> _ = function
  | `C r -> Ok r
  | r -> wrong_kind_error [ `C ] r

let class_type_lookup_result_of_type : type_lookup_result -> _ = function
  | `CT r -> Ok r
  | r -> wrong_kind_error [ `CT ] r

let ref_kind_of_element = function
  | `Module _ -> "module"
  | `ModuleType _ -> "module-type"
  | `Type _ -> "type"
  | `Value _ -> "val"
  | `Label _ -> "section"
  | `Class _ -> "class"
  | `ClassType _ -> "class-type"
  | `Constructor _ -> "constructor"
  | `Exception _ -> "exception"
  | `Extension _ -> "extension"
  | `ExtensionDecl _ -> "extension-decl"
  | `Field _ -> "field"
  | `Page _ -> "page"

let ref_kind_of_find = function
  | `FModule _ | `FModule_subst _ -> "module"
  | `FModuleType _ | `FModuleType_subst _ -> "module-type"
  | `FType _ | `FType_subst _ -> "type"
  | `FValue _ -> "val"
  | `FLabel _ -> "section"
  | `FClass _ -> "class"
  | `FClassType _ -> "class-type"
  | `FConstructor _ | `In_type (_, _, `FConstructor _) -> "constructor"
  | `FExn _ -> "exception"
  | `FExt _ -> "extension"
  | `FExtDecl _ -> "extension-decl"
  | `FField _ | `In_type (_, _, `FField _) -> "field"
  | `FMethod _ -> "method"
  | `FInstance_variable _ -> "instance-variable"

let ambiguous_generic_ref_warning name results =
  (* Sort the results to make sure the result is reproducible. *)
  let results = List.sort String.compare results in
  let pp_sep pp () = Format.fprintf pp ", "
  and pp_kind pp r = Format.fprintf pp "%s-%s" r name in
  Lookup_failures.report_warning
    "Reference to '%s' is ambiguous. Please specify its kind: %a." name
    (Format.pp_print_list ~pp_sep pp_kind)
    results

let ambiguous_label_warning name (labels : Component.Element.any list) =
  (* Sort the results to make sure the result is reproducible. *)
  let pp_kind pp r =
    match r with
    | `Label (_, l) ->
        Odoc_model.Location_.pp_span_start pp l.Component.Label.location
    | _ -> ()
  in
  Lookup_failures.report_warning
    "@[<2>Multiple sections named '%s' found. Please alter one to ensure \
     reference is unambiguous. Locations:@ %a@]"
    name
    (Format.pp_print_list ~pp_sep:Format.pp_force_newline pp_kind)
    labels

let ambiguous_warning name (results : [< Component.Element.any ] list) =
  let results = (results :> Component.Element.any list) in
  if List.for_all (function `Label _ -> true | _ -> false) results then
    ambiguous_label_warning name results
  else ambiguous_generic_ref_warning name (List.map ref_kind_of_element results)

let env_lookup_by_name ?(kind = `Any) scope name env =
  match Env.lookup_by_name scope name env with
  | Ok x -> Ok x
  | Error (`Ambiguous (hd, tl)) ->
      ambiguous_warning name (hd :: tl);
      Ok hd
  | Error `Not_found -> Error (`Lookup_by_name (kind, name))

let find_ambiguous ?(kind = `Any) find sg name =
  match find sg name with
  | [ x ] -> Ok x
  | x :: _ as results ->
      ambiguous_generic_ref_warning name (List.map ref_kind_of_find results);
      Ok x
  | [] -> Error (`Find_by_name (kind, name))

let find find sg name =
  match find sg name with
  | Some x -> Ok x
  | None -> Error (`Find_by_name (`Any, name))

let module_lookup_to_signature_lookup env (ref, cp, m) =
  let rec handle_expansion : Tools.expansion -> _ = function
    | Functor (_, expr) -> (
        match
          Tools.expansion_of_module_type_expr ~mark_substituted:true env expr
        with
        | Ok e -> handle_expansion e
        | Error _ as e -> e)
    | Signature sg -> Ok ((ref :> Resolved.Signature.t), `Module cp, sg)
  in
  Tools.expansion_of_module env m
  >>= handle_expansion
  |> map_error (fun e -> `Parent (`Parent_sig e))

let module_type_lookup_to_signature_lookup env (ref, cp, m) =
  Tools.expansion_of_module_type env m
  |> map_error (fun e -> `Parent (`Parent_sig e))
  >>= Tools.assert_not_functor
  >>= fun sg -> Ok ((ref :> Resolved.Signature.t), `ModuleType cp, sg)

let type_lookup_to_class_signature_lookup =
  let resolved p' cs = Ok ((p' :> Resolved.ClassSignature.t), cs) in
  fun env -> function
    | `T _ as r -> wrong_kind_error [ `C; `CT ] r
    | `C (p', c) ->
        Tools.class_signature_of_class env c
        |> of_option ~error:(`Parent (`Parent_type `OpaqueClass))
        >>= resolved p'
    | `CT (p', ct) ->
        Tools.class_signature_of_class_type env ct
        |> of_option ~error:(`Parent (`Parent_type `OpaqueClass))
        >>= resolved p'

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
      | Some (`Aliased cp) ->
          let cp = Tools.reresolve_module env cp in
          let p = Lang_of.(Path.resolved_module (empty ()) cp) in
          (`Alias (cp, `Resolved base_path, None), `Alias (p, base_ref))
      | Some (`SubstMT cp) ->
          let cp = Tools.reresolve_module_type env cp in
          (`Subst (cp, base_path), base_ref)
    in
    (r, p, m)

  let in_signature env ((parent, parent_cp, sg) : signature_lookup_result) name
      =
    let parent_cp = Tools.reresolve_parent env parent_cp in
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find Find.module_in_sig sg name >>= fun (`FModule (name, m)) ->
    Ok (of_component env m (`Module (parent_cp, name)) (`Module (parent, name)))

  let of_element env (`Module (id, m)) : t =
    let m = Component.Delayed.get m in
    let id = (id :> Identifier.Path.Module.t) in
    of_component env m (`Gpath (`Identifier id)) (`Identifier id)

  let in_env env name =
    match env_lookup_by_name Env.s_module name env with
    | Ok e -> Ok (of_element env e)
    | Error _ -> Error (`Parent (`Parent_module (`Lookup_failure_root name)))
end

module MT = struct
  (** Module type *)

  type t = module_type_lookup_result

  let of_component env mt base_path base_ref : t =
    match Tools.get_module_type_path_modifiers env ~add_canonical:true mt with
    | None -> (base_ref, base_path, mt)
    | Some (`AliasModuleType cp) ->
        let cp = Tools.reresolve_module_type env cp in
        let p = Lang_of.(Path.resolved_module_type (empty ()) cp) in
        (`AliasModuleType (p, base_ref), `AliasModuleType (cp, base_path), mt)

  let in_signature env ((parent', parent_cp, sg) : signature_lookup_result) name
      =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find Find.module_type_in_sig sg name >>= fun (`FModuleType (name, mt)) ->
    Ok
      (of_component env mt
         (`ModuleType (parent_cp, name))
         (`ModuleType (parent', name)))

  let of_element env (`ModuleType (id, mt)) : t =
    of_component env mt (`Gpath (`Identifier id)) (`Identifier id)

  let in_env env name =
    env_lookup_by_name Env.s_module_type name env >>= fun e ->
    Ok (of_element env e)
end

module CL = struct
  (** Class *)

  type t = class_lookup_result

  let of_element _env (`Class (id, t)) : t = (`Identifier id, t)

  let in_env env name =
    env_lookup_by_name Env.s_class name env >>= fun e -> Ok (of_element env e)

  let of_component _env c ~parent_ref name = Ok (`Class (parent_ref, name), c)
end

module CT = struct
  type t = class_type_lookup_result

  let of_element _env (`ClassType (id, t)) : t =
    ((`Identifier id :> Resolved.ClassType.t), t)

  let in_env env name =
    env_lookup_by_name Env.s_class_type name env >>= fun e ->
    Ok (of_element env e)

  let of_component _env ct ~parent_ref name =
    Ok (`ClassType (parent_ref, name), ct)
end

module DT = struct
  (** Datatype *)

  type t = datatype_lookup_result

  let of_component _env t ~parent_ref name = Ok (`Type (parent_ref, name), t)

  let of_element _env (`Type (id, t)) : t = (`Identifier id, t)

  let in_env env name =
    env_lookup_by_name Env.s_datatype name env >>= fun e ->
    Ok (of_element env e)

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find Find.datatype_in_sig sg name >>= function
    | `FType (name, t) -> Ok (`T (`Type (parent', name), t))
end

module T = struct
  (** Type *)

  type t = type_lookup_result

  let of_element env : _ -> t = function
    | `Type _ as e -> `T (DT.of_element env e)
    | `Class _ as e -> `C (CL.of_element env e)
    | `ClassType _ as e -> `CT (CT.of_element env e)

  let in_env env name =
    env_lookup_by_name Env.s_type name env >>= fun e -> Ok (of_element env e)

  (* Don't handle name collisions between class, class types and type decls *)
  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find Find.type_in_sig sg name >>= function
    | `FType (name, t) -> Ok (`T (`Type (parent', name), t))
    | `FClass (name, c) -> Ok (`C (`Class (parent', name), c))
    | `FClassType (name, ct) -> Ok (`CT (`ClassType (parent', name), ct))
end

module V = struct
  (** Value *)

  type t = Resolved.Value.t

  let in_env env name : t ref_result =
    env_lookup_by_name Env.s_value name env >>= fun (`Value (id, _x)) ->
    Ok (`Identifier id)

  let of_component _env ~parent_ref name = Ok (`Value (parent_ref, name))

  let in_signature _env ((parent', _, sg) : signature_lookup_result) name =
    find_ambiguous ~kind:`S Find.value_in_sig sg (ValueName.to_string name)
    >>= fun _ -> Ok (`Value (parent', name))
end

module L = struct
  (** Label *)

  type t = Resolved.Label.t

  let in_env env name : t ref_result =
    env_lookup_by_name Env.s_label name env >>= fun (`Label (id, _)) ->
    Ok (`Identifier id)

  let in_page _env (`P (_, p)) name =
    let rec find = function
      | hd :: tl -> (
          match Odoc_model.Location_.value hd with
          | `Heading
              ( _,
                ({ Odoc_model.Paths.Identifier.iv = `Label (_, name'); _ } as
                 label),
                _ )
            when name = LabelName.to_string name' ->
              Ok (`Identifier label)
          | _ -> find tl)
      | [] -> Error (`Find_by_name (`Page, name))
    in
    find p.Odoc_model.Lang.Page.content

  let of_component _env ~parent_ref label =
    Ok
      (`Label
        ( (parent_ref :> Resolved.LabelParent.t),
          Ident.Name.typed_label label.Component.Label.label ))

  let in_label_parent env (parent : label_parent_lookup_result) name =
    match parent with
    | `S (p, _, sg) ->
        find_ambiguous ~kind:`Label Find.label_in_sig sg
          (LabelName.to_string name)
        >>= fun _ -> Ok (`Label ((p :> Resolved.LabelParent.t), name))
    | (`T _ | `C _ | `CT _) as r -> wrong_kind_error [ `S; `Page ] r
    | `P _ as page -> in_page env page (LabelName.to_string name)
end

module EC = struct
  (** Extension constructor *)

  type t = Resolved.Constructor.t

  let in_env env name =
    env_lookup_by_name Env.s_extension name env
    >>= fun (`Extension (id, _, _)) -> Ok (`Identifier id :> t)

  let of_component _env ~parent_ref name =
    Ok (`Extension (parent_ref, ExtensionName.make_std name))

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find Find.extension_in_sig sg (ExtensionName.to_string name) >>= fun _ ->
    Ok (`Extension (parent', name))
end

module ED = struct
  (** Extension decl *)

  let in_env env name =
    env_lookup_by_name Env.s_extension name env
    >>= fun (`Extension (id, _, te)) ->
    (* Type extensions always have at least 1 constructor.
       The reference to the type extension shares the same name as the first constructor. *)
    match te.constructors with
    | [] -> assert false
    | c :: _ ->
        let id_parent = match id.iv with `Extension (p, _) -> p in
        Ok
          (`Identifier
            (Identifier.Mk.extension_decl
               ( id_parent,
                 (ExtensionName.make_std c.name, ExtensionName.make_std name) )))

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find Find.extension_in_sig sg (ExtensionName.to_string name)
    >>= fun (`FExt (ext, _) : Find.extension) ->
    (* Type extensions always have at least 1 constructor.
       The reference to the type extension shares the same name as the first constructor. *)
    match ext.constructors with
    | [] -> assert false
    | c :: _ ->
        Ok (`ExtensionDecl (parent', ExtensionName.make_std c.name, name))
end

module EX = struct
  (** Exception *)

  type t = Resolved.Exception.t

  let in_env env name : t ref_result =
    env_lookup_by_name Env.s_exception name env >>= fun (`Exception (id, _)) ->
    Ok (`Identifier id)

  let of_component _env ~parent_ref name = Ok (`Exception (parent_ref, name))

  let in_signature _env ((parent', parent_cp, sg) : signature_lookup_result)
      name =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find Find.exception_in_sig sg (ExceptionName.to_string name) >>= fun _ ->
    Ok (`Exception (parent', name))
end

module FTP = struct
  (** Fragment type parent *)

  type t = fragment_type_parent_lookup_result

  let of_element env : _ -> t ref_result = function
    | `Module _ as e ->
        M.of_element env e |> module_lookup_to_signature_lookup env >>= fun r ->
        Ok (`S r)
    | `ModuleType _ as e ->
        MT.of_element env e |> module_type_lookup_to_signature_lookup env
        >>= fun r -> Ok (`S r)
    | `Type _ as e -> Ok (`T (DT.of_element env e))

  let in_env env name =
    env_lookup_by_name Env.s_fragment_type_parent name env >>= of_element env
end

module CS = struct
  (** Constructor *)

  type t = Resolved.Constructor.t

  let in_env env name =
    env_lookup_by_name Env.s_constructor name env
    >>= fun (`Constructor (id, _)) -> Ok (`Identifier id :> t)

  let got_a_field name =
    (* Let's pretend we didn't see the field and say we didn't find anything. *)
    Error (`Find_by_name (`Cons, name))

  let in_parent _env (parent : fragment_type_parent_lookup_result) name =
    let name_s = ConstructorName.to_string name in
    match parent with
    | `S (parent', parent_cp, sg) -> (
        let sg = Tools.prefix_signature (parent_cp, sg) in
        find_ambiguous Find.any_in_type_in_sig sg name_s >>= function
        | `In_type (_, _, `FField _) -> got_a_field name_s
        | `In_type (typ_name, _, `FConstructor _) ->
            Ok (`Constructor (`Type (parent', typ_name), name)))
    | `T (parent', t) -> (
        find Find.any_in_type t name_s >>= function
        | `FField _ -> got_a_field name_s
        | `FConstructor _ ->
            Ok (`Constructor ((parent' : Resolved.DataType.t), name)))

  let of_component _env parent name =
    Ok
      (`Constructor
        ((parent : Resolved.DataType.t), ConstructorName.make_std name))
end

module F = struct
  (** Field *)

  type t = Resolved.Field.t

  let in_env env name =
    env_lookup_by_name Env.s_field name env >>= fun (`Field (id, _)) ->
    Ok (`Identifier id :> t)

  let got_a_constructor name =
    (* Let's pretend we didn't see the constructor and say we didn't find anything. *)
    Error (`Find_by_name (`Field, name))

  let in_parent _env (parent : fragment_type_parent_lookup_result) name =
    let name_s = FieldName.to_string name in
    match parent with
    | `S (parent', parent_cp, sg) -> (
        let sg = Tools.prefix_signature (parent_cp, sg) in
        find_ambiguous Find.any_in_type_in_sig sg name_s >>= function
        | `In_type (_, _, `FConstructor _) -> got_a_constructor name_s
        | `In_type (typ_name, _, `FField _) ->
            Ok
              (`Field
                ((`Type (parent', typ_name) :> Resolved.FieldParent.t), name)))
    | `T (parent', t) -> (
        find Find.any_in_type t name_s >>= function
        | `FConstructor _ -> got_a_constructor name_s
        | `FField _ -> Ok (`Field ((parent' :> Resolved.FieldParent.t), name)))

  let of_component _env parent name =
    Ok
      (`Field
        ( (parent : Resolved.DataType.t :> Resolved.FieldParent.t),
          FieldName.make_std name ))
end

module MM = struct
  (** Method *)

  type t = Resolved.Method.t

  (* TODO: Resolve methods in env *)
  let in_env _env name : t ref_result = Error (`Lookup_by_name (`Any, name))

  let in_class_signature _env (parent', cs) name =
    find Find.method_in_class_signature cs (MethodName.to_string name)
    >>= fun _ -> Ok (`Method (parent', name))

  let of_component _env parent' name = Ok (`Method (parent', name))
end

module MV = struct
  (** Instance variable *)

  type t = Resolved.InstanceVariable.t

  (* TODO: Resolve instance variables in env *)
  let in_env _env name : t ref_result = Error (`Lookup_by_name (`Any, name))

  let in_class_signature _env (parent', cs) name =
    find Find.instance_variable_in_class_signature cs
      (InstanceVariableName.to_string name)
    >>= fun _ -> Ok (`InstanceVariable (parent', name))

  let of_component _env parent' name = Ok (`InstanceVariable (parent', name))
end

module Page = struct
  type t = page_lookup_result

  let in_env env name : t ref_result =
    match Env.lookup_page name env with
    | Some p -> Ok (`Identifier p.Odoc_model.Lang.Page.name, p)
    | None -> Error (`Lookup_by_name (`Page, name))

  let of_element _env (`Page (id, page)) : t = (`Identifier id, page)
end

module LP = struct
  (** Label parent *)

  type t = label_parent_lookup_result

  let of_element env : _ -> t ref_result = function
    | `Module _ as e ->
        M.of_element env e |> module_lookup_to_signature_lookup env >>= fun r ->
        Ok (`S r)
    | `ModuleType _ as e ->
        MT.of_element env e |> module_type_lookup_to_signature_lookup env
        >>= fun r -> Ok (`S r)
    | `Type _ as e -> Ok (`T (DT.of_element env e))
    | `Class _ as e -> Ok (`C (CL.of_element env e))
    | `ClassType _ as e -> Ok (`CT (CT.of_element env e))
    | `Page _ as e -> Ok (`P (Page.of_element env e))

  let in_env env name =
    env_lookup_by_name Env.s_label_parent name env >>= of_element env

  let in_signature env ((parent', parent_cp, sg) : signature_lookup_result) name
      =
    let sg = Tools.prefix_signature (parent_cp, sg) in
    find_ambiguous Find.label_parent_in_sig sg name >>= function
    | `FModule (name, m) ->
        module_lookup_to_signature_lookup env
          (M.of_component env m
             (`Module (parent_cp, name))
             (`Module (parent', name)))
        >>= fun s -> Ok (`S s)
    | `FModuleType (name, mt) ->
        module_type_lookup_to_signature_lookup env
          (MT.of_component env mt
             (`ModuleType (parent_cp, name))
             (`ModuleType (parent', name)))
        >>= fun s -> Ok (`S s)
    | `FType (name, t) ->
        DT.of_component env ~parent_ref:parent' t name >>= fun t -> Ok (`T t)
    | `FClass (name, c) ->
        CL.of_component env ~parent_ref:parent' c name >>= fun c -> Ok (`C c)
    | `FClassType (name, ct) ->
        CT.of_component env ~parent_ref:parent' ct name >>= fun ct ->
        Ok (`CT ct)
end

let rec resolve_label_parent_reference env r =
  let label_parent_res_of_type_res : type_lookup_result -> _ =
   fun r -> Ok (r :> label_parent_lookup_result)
  in
  match r with
  | `Resolved _ -> failwith "unimplemented"
  | `Root (name, `TUnknown) -> LP.in_env env name
  | (`Module _ | `ModuleType _ | `Root (_, (`TModule | `TModuleType))) as sr ->
      resolve_signature_reference env sr >>= fun s -> Ok (`S s)
  | `Root (name, `TType) -> T.in_env env name >>= label_parent_res_of_type_res
  | `Type (parent, name) ->
      resolve_signature_reference env parent >>= fun p ->
      T.in_signature env p (TypeName.to_string name)
      >>= label_parent_res_of_type_res
  | `Root (name, `TClass) -> CL.in_env env name >>= fun r -> Ok (`C r)
  | `Class (parent, name) ->
      resolve_signature_reference env parent >>= fun p ->
      T.in_signature env p (ClassName.to_string name)
      >>= class_lookup_result_of_type
      >>= fun r -> Ok (`C r)
  | `Root (name, `TClassType) -> CT.in_env env name >>= fun r -> Ok (`CT r)
  | `ClassType (parent, name) ->
      resolve_signature_reference env parent >>= fun p ->
      T.in_signature env p (ClassTypeName.to_string name)
      >>= class_type_lookup_result_of_type
      >>= fun r -> Ok (`CT r)
  | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent
      >>= fun p -> LP.in_signature env p name
  | `Root (name, `TPage) | `Root (name, `TChildPage) ->
      Page.in_env env name >>= fun r -> Ok (`P r)
  | `Root (name, `TChildModule) ->
      resolve_signature_reference env (`Root (name, `TModule)) >>= fun s ->
      Ok (`S s)

and resolve_fragment_type_parent_reference (env : Env.t)
    (r : FragmentTypeParent.t) : (fragment_type_parent_lookup_result, _) result
    =
  let fragment_type_parent_res_of_type_res : datatype_lookup_result -> _ =
   fun r -> Ok (`T r)
  in
  match r with
  | `Resolved _ -> failwith "unimplemented"
  | `Root (name, `TUnknown) -> FTP.in_env env name
  | (`Module _ | `ModuleType _ | `Root (_, (`TModule | `TModuleType))) as sr ->
      resolve_signature_reference env sr >>= fun s -> Ok (`S s)
  | `Root (name, `TType) ->
      DT.in_env env name >>= fragment_type_parent_res_of_type_res
  | `Type (parent, name) ->
      resolve_signature_reference env parent >>= fun p ->
      DT.in_signature env p (TypeName.to_string name)
  | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent
      >>= fun p -> DT.in_signature env p name

and resolve_signature_reference :
    Env.t -> Signature.t -> signature_lookup_result ref_result =
 fun env' r ->
  let resolve env =
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
        find_ambiguous ~kind:`S Find.signature_in_sig sg name >>= function
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

and resolve_module_reference env (r : Module.t) : M.t ref_result =
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
  | (`S _ | `P _) as r -> wrong_kind_error [ `T; `C; `CT ] r

(***)

let resolved1 r = Ok (r :> Resolved.t)

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
  | `FLabel label -> L.of_component env ~parent_ref label >>= resolved1
  | `FExn (name, _) -> EX.of_component env ~parent_ref name >>= resolved1
  | `FExt _ -> EC.of_component env ~parent_ref name >>= resolved1
  | `In_type (typ_name, _, r) -> (
      let parent = `Type (parent_ref, typ_name) in
      match r with
      | `FConstructor _ -> CS.of_component env parent name >>= resolved1
      | `FField _ -> F.of_component env parent name >>= resolved1)
  | `FModule_subst _ | `FType_subst _ | `FModuleType_subst _ ->
      Error (`Find_by_name (`Any, name))

let resolve_reference_dot_page env page name =
  L.in_page env page name >>= resolved1

let resolve_reference_dot_type env ~parent_ref t name =
  find Find.any_in_type t name >>= function
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
  | `P _ as page -> resolve_reference_dot_page env page name

(** Warnings may be generated with [Error.implicit_warning] *)
let resolve_reference =
  let resolved = resolved3 in
  fun env r ->
    match r with
    | `Root (name, `TUnknown) -> (
        let identifier id = Ok (`Identifier (id :> Identifier.t)) in
        env_lookup_by_name Env.s_any name env >>= function
        | `Module (_, _) as e -> resolved (M.of_element env e)
        | `ModuleType (_, _) as e -> resolved (MT.of_element env e)
        | `Value (id, _) -> identifier id
        | `Type (id, _) -> identifier id
        | `Label (id, _) -> identifier id
        | `Class (id, _) -> identifier id
        | `ClassType (id, _) -> identifier id
        | `Constructor (id, _) -> identifier id
        | `Exception (id, _) -> identifier id
        | `Extension (id, _, _) -> identifier id
        | `ExtensionDecl (id, _) -> identifier id
        | `Field (id, _) -> identifier id
        | `Page (id, _) -> identifier id)
    | `Resolved r -> Ok r
    | `Root (name, (`TModule | `TChildModule)) -> M.in_env env name >>= resolved
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
    | `Root (name, (`TPage | `TChildPage)) -> Page.in_env env name >>= resolved2
    | `Dot (parent, name) -> resolve_reference_dot env parent name
    | `Root (name, `TConstructor) -> CS.in_env env name >>= resolved1
    | `Constructor (parent, name) ->
        resolve_fragment_type_parent_reference env parent >>= fun p ->
        CS.in_parent env p name >>= resolved1
    | `Root (name, `TException) -> EX.in_env env name >>= resolved1
    | `Exception (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        EX.in_signature env p name >>= resolved1
    | `Root (name, `TExtension) -> EC.in_env env name >>= resolved1
    | `Extension (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        EC.in_signature env p name >>= resolved1
    | `Root (name, `TExtensionDecl) -> ED.in_env env name >>= resolved1
    | `ExtensionDecl (parent, name) ->
        resolve_signature_reference env parent >>= fun p ->
        ED.in_signature env p name >>= resolved1
    | `Root (name, `TField) -> F.in_env env name >>= resolved1
    | `Field (parent, name) ->
        resolve_fragment_type_parent_reference env parent >>= fun p ->
        F.in_parent env p name >>= resolved1
    | `Root (name, `TMethod) -> MM.in_env env name >>= resolved1
    | `Method (parent, name) ->
        resolve_class_signature_reference env parent >>= fun p ->
        MM.in_class_signature env p name >>= resolved1
    | `Root (name, `TInstanceVariable) -> MV.in_env env name >>= resolved1
    | `InstanceVariable (parent, name) ->
        resolve_class_signature_reference env parent >>= fun p ->
        MV.in_class_signature env p name >>= resolved1

let resolve_module_reference env m =
  Odoc_model.Error.catch_warnings (fun () -> resolve_module_reference env m)

let resolve_reference env m =
  Odoc_model.Error.catch_warnings (fun () -> resolve_reference env m)
