#if OCAML_VERSION >= (4, 14, 0)

open Odoc_model
open Odoc_model.Paths
open Odoc_model.Names
module Kind = Shape.Sig_component_kind

let ( >>= ) m f = match m with Some x -> f x | None -> None

type t = Shape.t * Odoc_model.Names.DefName.t Shape.Uid.Tbl.t

let anchor_of_uid uid =
  match Uid.unpack_uid uid with
  | Some (_, Some id) -> Some (Uid.anchor_of_id id)
  | _ -> None

(** Project an identifier into a shape. *)
let rec shape_of_id lookup_shape :
    [< Identifier.NonSrc.t_pv ] Identifier.id -> Shape.t option =
  let proj parent kind name =
    let item = Shape.Item.make name kind in
    match shape_of_id lookup_shape (parent :> Identifier.NonSrc.t) with
    | Some shape -> Some (Shape.proj shape item)
    | None -> None
  in
  fun id ->
    match id.iv with
    | `Root (_, name) ->
        lookup_shape (ModuleName.to_string name) >>= fun (_, (shape, _)) ->
        Some shape
    | `Module (parent, name) ->
        proj parent Kind.Module (ModuleName.to_string name)
    | `Result parent ->
        (* Apply the functor to an empty signature. This doesn't seem to cause
           any problem, as the shape would stop resolve on an item inside the
           result of the function, which is what we want. *)
        shape_of_id lookup_shape (parent :> Identifier.NonSrc.t) >>= fun parent ->
        Some (Shape.app parent ~arg:(Shape.str Shape.Item.Map.empty))
    | `ModuleType (parent, name) ->
        proj parent Kind.Module_type (ModuleTypeName.to_string name)
    | `Type (parent, name) -> proj parent Kind.Type (TypeName.to_string name)
    | `Value (parent, name) -> proj parent Kind.Value (ValueName.to_string name)
    | `Extension (parent, name) ->
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

let anchors_of_shape shape =
  let anchor_of x =
    List.map (fun (name,ty) ->
      match ty with
      | Shape.Sig_component_kind.Value -> "val-" ^ name
      | Type -> "type-" ^ name
      | Module -> "module-" ^ name
      | Module_type -> "module-type-" ^ name
      | Extension_constructor -> "ext-" ^ name
      | Class -> "class-" ^ name
      | Class_type -> "class-type-" ^ name) x |> List.rev |> String.concat "." |> Odoc_model.Names.DefName.make_std
  in
  let rec inner shape uid_names cur =
    let add uid =
      match anchor_of_uid uid with
      | None -> uid_names
      | Some _ -> (uid, anchor_of cur) :: uid_names
    in
    match shape.Shape.uid with
    | None -> uid_names
    | Some uid ->
      match shape.Shape.desc with
      | Shape.Struct m -> (
        Shape.Item.Map.fold (fun item s uid_names ->
          let (name,ty) : (string * Shape.Sig_component_kind.t) = Obj.magic item in
          let name =
            if String.length name > 0 && String.get name 0 = '#'
            then "hash-" ^ String.sub name 1 (String.length name - 1)
            else name
          in
          inner s uid_names ((name,ty)::cur)) m (add uid))
      | Abs (_, s) ->
        inner s (add uid) cur
      | App (_, _) ->
        add uid
      | Leaf 
      | Var _ 
      | Proj (_, _)
      | Comp_unit _ ->
          add uid
  in
  let anchors = inner shape [] [] in
  Shape.Uid.Tbl.of_list anchors

module MkId = Identifier.Mk

let lookup_def :  (string -> (Lang.Compilation_unit.t * t) option) ->
  Identifier.NonSrc.t ->
  Identifier.SourceLocation.t option = fun lookup_unit id ->
  match shape_of_id lookup_unit id with
  | None -> None
  | Some query ->
      let module Reduce = Shape.Make_reduce (struct
        type env = unit
        let fuel = 10
        let read_unit_shape ~unit_name =
          match lookup_unit unit_name with
          | Some (_, (shape, _)) -> Some shape
          | None -> None
        let find_shape _ _ = raise Not_found
      end) in
      let result = try Some (Reduce.reduce () query) with Not_found -> None in
      result >>= fun result ->
      result.uid >>= fun uid ->
      Uid.unpack_uid uid >>= fun (unit_name, _) ->
      lookup_unit unit_name >>= fun (unit, (_, uid_to_anchor)) ->
      let anchor = Shape.Uid.Tbl.find_opt uid_to_anchor uid in
      unit.Lang.Compilation_unit.source_info >>= fun sources ->
      match anchor with
      | Some anchor -> Some (Paths.Identifier.Mk.source_location (sources.id, anchor))
      | None -> Some (Paths.Identifier.Mk.source_location_mod sources.id)

let of_cmt (cmt : Cmt_format.cmt_infos) =
  match cmt.cmt_impl_shape with
  | Some s -> Some (s, anchors_of_shape s)
  | None -> None

let anchor_of_uid ((_, uid_to_anchor) : t) (uid :Shape.Uid.t) =
  Shape.Uid.Tbl.find_opt uid_to_anchor uid
  

#else

type t = unit

let lookup_def _ _id = None
let of_cmt _ = Some ()

#endif
