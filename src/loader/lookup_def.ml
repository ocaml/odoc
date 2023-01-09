#if OCAML_VERSION >= (4, 14, 0)

open Odoc_model
open Odoc_model.Paths
open Odoc_model.Names
module Kind = Shape.Sig_component_kind

let ( >>= ) m f = match m with Some x -> f x | None -> None

type t = { uid_to_loc : Location.t Shape.Uid.Tbl.t; impl_shape : Shape.t }

(** Project an identifier into a shape. *)
let rec project_id :
    Shape.t -> [< Identifier.t_pv ] Identifier.id -> Shape.t option =
  let proj shape parent kind name =
    let item = Shape.Item.make name kind in
    match project_id shape (parent :> Identifier.t) with
    | Some shape -> Some (Shape.proj shape item)
    | None -> None
  in
  fun shape id ->
    match id.iv with
    | `Root _ ->
        (* TODO: Assert that's the right root *)
        Some shape
    | `Module (parent, name) ->
        proj shape parent Kind.Module (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        proj shape parent Kind.Module_type (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        proj shape parent Kind.Type (TypeName.to_string name)
    | `Value (parent, name) ->
        proj shape parent Kind.Value (ValueName.to_string name)
    | `Extension (parent, name) ->
        proj shape parent Kind.Extension_constructor
          (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        proj shape parent Kind.Extension_constructor
          (ExceptionName.to_string name)
    | `Class (parent, name) ->
        proj shape parent Kind.Class (ClassName.to_string name)
    | `ClassType (parent, name) ->
        proj shape parent Kind.Class_type (ClassTypeName.to_string name)
    | `Page _ | `LeafPage _ | `Label _ | `CoreType _ | `CoreException _
    | `Constructor _ | `Field _ | `Method _ | `InstanceVariable _ | `Parameter _
    | `Result _ ->
        (* Not represented in shapes. *)
        None

let item_exp item : string * Shape.Sig_component_kind.t =
  (* This function is missing in compiler-libs. *)
  Obj.magic item

module MkId = Identifier.Mk

let rec shape_to_module_path shape =
  match shape.Shape.desc with
  | Var _ | Struct _ | Leaf -> None
  | Abs (_, parent) -> shape_to_module_path parent
  | App (left, _) -> shape_to_module_path left
  | Proj (parent, item) -> (
      shape_to_module_path parent >>= fun parent ->
      let name, kind = item_exp item in
      match kind with
      | Module -> Some (`Dot (parent, name))
      | Module_type | Value | Type | Extension_constructor | Class | Class_type
        ->
          None)
  | Comp_unit name -> Some (`Root name)

let shape_to_unresolved shape =
  match shape.Shape.desc with
  | Proj (parent, item) ->
      shape_to_module_path parent >>= fun parent ->
      let name, kind = item_exp item in
      Some
        (match kind with
        | Value -> `Value (parent, name)
        | Type -> `Type (`Dot (parent, name))
        | Module -> `Module (`Dot (parent, name))
        | Module_type -> `ModuleType (`Dot (parent, name))
        | Extension_constructor -> `Extension (parent, name)
        | Class -> `Class (parent, name)
        | Class_type -> `ClassType (`Dot (parent, name)))
  | _ ->
      (shape_to_module_path shape :> Path.Module.t option) >>= fun m ->
      Some (`Module m)

let lookup_def impl_shape id =
  match project_id impl_shape.impl_shape id with
  | None -> None
  | Some query -> (
      let result = Shape.local_reduce query in
      match result.uid with
      | Some uid when Shape.Uid.Tbl.mem impl_shape.uid_to_loc uid ->
          (* Check whether [uid] has a location in the current unit.
             [result.uid] might be [Some] but point to something in an other
             compilation unit, that would need to be resolved again. *)
          let anchor = Uid.string_of_uid (Uid.of_shape_uid uid) in
          Some (Lang.Locations.Resolved { anchor })
      | _ -> (
          match shape_to_unresolved result with
          | Some id -> Some (Lang.Locations.Unresolved id)
          | None -> None))

let of_cmt (cmt : Cmt_format.cmt_infos) =
  match cmt.cmt_impl_shape with
  | Some impl_shape -> Some { uid_to_loc = cmt.cmt_uid_to_loc; impl_shape }
  | None -> None

#else

type t = unit

let lookup_def () _id = None
let of_cmt _ = Some ((), [])

#endif
