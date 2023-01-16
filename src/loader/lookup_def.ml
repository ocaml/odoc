#if OCAML_VERSION >= (4, 14, 0)

open Odoc_model
open Odoc_model.Paths
open Odoc_model.Names
module Kind = Shape.Sig_component_kind

let ( >>= ) m f = match m with Some x -> f x | None -> None

type t = Shape.t

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

module MkId = Identifier.Mk

let comp_unit_of_uid = function
  | Shape.Uid.Compilation_unit s -> Some s
  | Item { comp_unit; _ } -> Some comp_unit
  | _ -> None

let lookup_def lookup_unit impl_shape id =
  match project_id impl_shape id with
  | None -> None
  | Some query ->
      let module Reduce = Shape.Make_reduce (struct
        type env = unit
        let fuel = 10
        let read_unit_shape ~unit_name =
          match lookup_unit unit_name with
          | Some (_, shape) -> Some shape
          | None -> None
        let find_shape _ _ = raise Not_found
      end) in
      let result = try Some (Reduce.reduce () query) with Not_found -> None in
      result >>= fun result ->
      result.uid >>= fun uid ->
      let anchor = Uid.string_of_uid (Uid.of_shape_uid uid) in
      let anchor = { Odoc_model.Lang.Locations.anchor } in
      comp_unit_of_uid uid >>= fun unit_name ->
      lookup_unit unit_name >>= fun (unit, _) ->
      Some (unit.Lang.Compilation_unit.id, anchor)

let of_cmt (cmt : Cmt_format.cmt_infos) = cmt.cmt_impl_shape

#else

type t = unit

let lookup_def _ () _id = None
let of_cmt _ = Some ()

#endif
