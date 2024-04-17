#if OCAML_VERSION >= (4, 14, 0)

open Odoc_model.Paths
open Odoc_model.Names
module Kind = Shape.Sig_component_kind

let ( >>= ) m f = match m with Some x -> f x | None -> None

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
    match id.iv with
    | `Root (_, name) -> begin
        match Env.lookup_unit (ModuleName.to_string name) env with
        | Some (Env.Found unit) -> (
          match unit.shape_info with | Some (shape, _) -> Some shape | None -> None)
        | _ -> None
      end
    | `Module (parent, name) ->
        proj parent Kind.Module (ModuleName.to_string name)
    | `Result parent ->
        (* Apply the functor to an empty signature. This doesn't seem to cause
           any problem, as the shape would stop resolve on an item inside the
           result of the function, which is what we want. *)
        shape_of_id env (parent :> Identifier.NonSrc.t)
        >>= fun parent ->
        Some (Shape.app parent ~arg:(Shape.str Shape.Item.Map.empty))
    | `ModuleType (parent, name) ->
        proj parent Kind.Module_type (ModuleTypeName.to_string name)
    | `Type (parent, name) -> proj parent Kind.Type (TypeName.to_string name)
    | `Value (parent, name) -> proj parent Kind.Value (ValueName.to_string name)
    | `Extension (parent, name) ->
        proj parent Kind.Extension_constructor (ExtensionName.to_string name)
    | `ExtensionDecl (parent, name, _) ->
        proj parent Kind.Extension_constructor (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        proj parent Kind.Extension_constructor (ExceptionName.to_string name)
    | `Class (parent, name) -> proj parent Kind.Class (ClassName.to_string name)
    | `ClassType (parent, name) ->
        proj parent Kind.Class_type (ClassTypeName.to_string name)
    | `Page _ | `LeafPage _ | `Label _ | `CoreType _ | `CoreException _
    | `Constructor _ | `Field _ | `Method _ | `InstanceVariable _ | `Parameter _
      ->
        (* Not represented in shapes. *)
        None

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
  
let lookup_shape :
    Env.t ->
    Shape.t ->
    Identifier.SourceLocation.t option =
 fun env query ->
#if OCAML_VERSION < (5,2,0)
  let module Reduce = Shape.Make_reduce (struct
    type env = unit
    let fuel = 10
    let read_unit_shape ~unit_name =
      match Env.lookup_unit unit_name env with
      | Some (Found unit) -> (
        match unit.shape_info with | Some (shape, _) -> Some shape | None -> None)
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
      match Env.lookup_unit unit_name env with
      | Some (Found unit) -> (
          match unit.shape_info with
          | Some (shape, _) -> Some shape
          | None -> None)
       | _ -> None
  end) in
  let result = try Some (Reduce.reduce_for_uid Ocaml_env.empty query) with Not_found -> None in
  result >>= traverse_aliases >>= fun uid ->
#endif
  unit_of_uid uid >>= fun unit_name ->
  match Env.lookup_unit unit_name env with
  | None 
  | Some Forward_reference
  | Some (Not_found) -> None
  | Some (Found unit) -> 
    let uid_to_id =
      match unit.shape_info with
      | Some (_, uid_to_id) -> uid_to_id
      | None -> Odoc_model.Compat.empty_map
    in
    match Shape.Uid.Map.find_opt uid uid_to_id with
    | Some x -> Some x
    | None -> (
      match unit.source_info with
      | Some si -> Some (MkId.source_location_mod si.id)
      | None -> None)


let lookup_def :
    Env.t ->
    Identifier.NonSrc.t ->
    Identifier.SourceLocation.t option =
 fun env id ->
  match shape_of_id env id with
  | None -> None
  | Some query -> lookup_shape env query

#else

type t = unit

let lookup_def _ _id = None

#endif
