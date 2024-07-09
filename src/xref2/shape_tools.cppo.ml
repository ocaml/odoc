#if OCAML_VERSION >= (4, 14, 0)

open Odoc_model.Paths
open Odoc_model.Names
module Kind = Shape.Sig_component_kind

open Odoc_utils.OptionMonad

type t = Shape.t * Odoc_model.Paths.Identifier.SourceLocation.t Shape.Uid.Map.t

(** Project an identifier into a shape. *)
let rec shape_of_id env :
    [< Identifier.NonSrc.t_pv ] Identifier.id -> Shape.t option =
  let proj parent kind name =
    let item = Shape.Item.make name kind in
    match shape_of_id env (parent :> Identifier.NonSrc.t) with
    | Some shape -> Some (Shape.proj shape item)
    | None -> None
  in
  fun id ->
    if Identifier.is_hidden id then None else 
    match id.iv with
    | `Root (_, name) -> (
        match Env.lookup_impl (ModuleName.to_string_unsafe name) env with
        | Some impl -> (
            match impl.shape_info with
            | Some (shape, _) -> Some shape
            | None -> None)
        | _ -> None)
    | `Module (parent, name) ->
        proj parent Kind.Module (ModuleName.to_string_unsafe name)
    | `Result parent ->
        (* Apply the functor to an empty signature. This doesn't seem to cause
           any problem, as the shape would stop resolve on an item inside the
           result of the function, which is what we want. *)
        shape_of_id env (parent :> Identifier.NonSrc.t) >>= fun parent ->
        Some (Shape.app parent ~arg:(Shape.str Shape.Item.Map.empty))
    | `ModuleType (parent, name) ->
        proj parent Kind.Module_type (ModuleTypeName.to_string_unsafe name)
    | `Type (parent, name) -> proj parent Kind.Type (TypeName.to_string_unsafe name)
    | `Value (parent, name) -> proj parent Kind.Value (ValueName.to_string_unsafe name)
    | `Extension (parent, name) ->
        proj parent Kind.Extension_constructor (ExtensionName.to_string name)
    | `ExtensionDecl (parent, name, _) ->
        proj parent Kind.Extension_constructor (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        proj parent Kind.Extension_constructor (ExceptionName.to_string name)
    | `Class (parent, name) -> proj parent Kind.Class (ClassName.to_string_unsafe name)
    | `ClassType (parent, name) ->
        proj parent Kind.Class_type (ClassTypeName.to_string_unsafe name)
    | `Page _ | `LeafPage _ | `Label _ | `CoreType _ | `CoreException _
    | `Constructor _ | `Field _ | `Method _ | `InstanceVariable _ | `Parameter _
      ->
        (* Not represented in shapes. *)
        None

let rec shape_of_module_path env : _ -> Shape.t option =
  let proj parent kind name =
    let item = Shape.Item.make name kind in
    match
      shape_of_module_path env (parent :> Odoc_model.Paths.Path.Module.t)
    with
    | Some shape -> Some (Shape.proj shape item)
    | None -> None
  in
  fun (path : Odoc_model.Paths.Path.Module.t) ->
    match path with
    | `Resolved _ -> None
    | `Root name -> (
        match Env.lookup_impl name env with
        | Some impl -> (
            match impl.shape_info with
            | Some (shape, _) -> Some shape
            | None -> None)
        | _ -> None)
    | `Forward _ -> None
    | `Dot (parent, name) ->
        proj (parent :> Odoc_model.Paths.Path.Module.t) Kind.Module name
    | `Apply (parent, arg) ->
        shape_of_module_path env (parent :> Odoc_model.Paths.Path.Module.t)
        >>= fun parent ->
        shape_of_module_path env (arg :> Odoc_model.Paths.Path.Module.t)
        >>= fun arg -> Some (Shape.app parent ~arg)
    | `Identifier (id, _) ->
        shape_of_id env (id :> Odoc_model.Paths.Identifier.NonSrc.t)
    | `Substituted m ->
        shape_of_module_path env m

let rec shape_of_kind_path env kind :
    _ -> Shape.t option =
  let proj parent kind name =
    let item = Shape.Item.make name kind in
    match shape_of_module_path env parent with
    | Some shape -> Some (Shape.proj shape item)
    | None -> None
  in
  fun path ->
    match path with
    | `Resolved _ -> None
    | `Dot (parent, name) -> proj parent kind name
    | `SubstitutedT t -> shape_of_kind_path env kind t
    | `SubstitutedMT t -> shape_of_kind_path env kind t
    | `SubstitutedCT t -> shape_of_kind_path env kind t
    | `Identifier (id, _) -> shape_of_id env (id :> Odoc_model.Paths.Identifier.NonSrc.t)

module MkId = Identifier.Mk

let unit_of_uid uid =
  match uid with
  | Shape.Uid.Compilation_unit s -> Some s
  | Item { comp_unit; id = _ } -> Some comp_unit
  | Predef _ -> None
  | Internal -> None

#if OCAML_VERSION >= (5,2,0)
let rec traverse_aliases = function
   | Shape_reduce.Resolved uid -> Some uid
   | Approximated id -> id
   | Resolved_alias (_,x) -> traverse_aliases x
   | _ -> None
#endif

let lookup_shape : Env.t -> Shape.t -> Identifier.SourceLocation.t option =
 fun env query ->
#if OCAML_VERSION < (5,2,0)
  let module Reduce = Shape.Make_reduce (struct
    type env = unit
    let fuel = 10
    let read_unit_shape ~unit_name =
      match Env.lookup_impl unit_name env with
      | Some impl -> (
          match impl.shape_info with
          | Some (shape, _) -> Some shape
          | None -> None)
      | _ -> None
    let find_shape _ _ = raise Not_found
  end) in
  let result = try Some (Reduce.reduce () query) with Not_found -> None in
  result >>= fun result ->
  result.uid >>= fun uid ->
#else
  let module Reduce = Shape_reduce.Make(struct
    let fuel = 10
    let read_unit_shape ~unit_name =
      match Env.lookup_impl unit_name env with
      | Some impl -> (
          match impl.shape_info with
          | Some (shape, _) -> Some shape
          | None -> None)
       | _ -> None
  end) in
  let result = try Some (Reduce.reduce_for_uid Ocaml_env.empty query) with Not_found -> None in
  result >>= traverse_aliases >>= fun uid ->
#endif
  unit_of_uid uid >>= fun unit_name ->
  match Env.lookup_impl unit_name env with
  | Some { shape_info ; id = Some id ; _} -> (
      let uid_to_id =
        match shape_info with
        | Some (_, uid_to_id) -> uid_to_id
        | None -> Odoc_model.Compat.empty_map
      in
      match Shape.Uid.Map.find_opt uid uid_to_id with
      | Some x -> Some x
      | None ->  Some (MkId.source_location_mod id))
  | None
  | Some { id = None ; _} -> None

let lookup_def :
    Env.t -> Identifier.NonSrc.t -> Identifier.SourceLocation.t option =
 fun env id ->
  match shape_of_id env id with
  | None -> None
  | Some query -> lookup_shape env query

let lookup_module_path env path =
  match shape_of_module_path env path with
  | None -> None
  | Some query -> lookup_shape env query

let lookup_kind_path kind env path =
  match shape_of_kind_path env kind path with
  | None -> None
  | Some query -> lookup_shape env query

let lookup_value_path = lookup_kind_path Kind.Value

let lookup_type_path : Env.t -> Odoc_model.Paths.Path.Type.t -> _ = lookup_kind_path Kind.Type

let lookup_module_type_path = lookup_kind_path Kind.Module_type

let lookup_class_type_path = lookup_kind_path Kind.Class_type

#else

type t = unit

let lookup_def _ _id = None

let lookup_value_path _ _id = None

let lookup_module_path _ _id = None

let lookup_type_path _ _id = None

let lookup_module_type_path _ _id = None

let lookup_class_type_path _ _id = None

#endif
