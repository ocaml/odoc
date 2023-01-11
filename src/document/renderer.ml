(** Standard definition and types for all renderers *)

type syntax = OCaml | Reason

let string_of_syntax = function OCaml -> "ml" | Reason -> "re"

type page = {
  filename : Fpath.t;
  content : Format.formatter -> unit;
  children : page list;
}

let traverse ~f t =
  let rec aux node =
    f node.filename node.content;
    List.iter aux node.children
  in
  List.iter aux t

type 'a t = {
  name : string;
  render : 'a -> Types.Page.t -> Fpath.t -> page list;
}

let document_of_page ~syntax v =
  match syntax with Reason -> Reason.page v | OCaml -> ML.page v

let document_of_compilation_unit ~syntax v =
  match syntax with
  | Reason -> Reason.compilation_unit v
  | OCaml -> ML.compilation_unit v

module Index : sig
  type def = string
  type t = Leaf of def | Node of def * t list
  val empty : t
  val of_page : Odoc_model.Lang.Page.t -> t
  val of_unit : Odoc_model.Lang.Compilation_unit.t -> t
end = struct
  type def = string

  type t = Leaf of def | Node of def * t list

  let empty : t = Node ("", [])

  let pp_id fs x = Format.fprintf fs "%s" (Odoc_model.Paths.Identifier.name x)

  (* TODO: add links *)
  let rec sig_item acc = function
    | Odoc_model.Lang.Signature.Module (_, m) -> (
        let id = Format.asprintf "module %a" pp_id m.id in
        match m.type_ with
        | Alias _ -> Leaf id :: acc
        | ModuleType mt -> (
            match mt with
            | Signature s -> Node (id, sig_items s.items) :: acc
            | _ -> Leaf id :: acc))
    | ModuleType mt -> (
        let id = Format.asprintf "module type %a" pp_id mt.id in
        match mt.expr with
        | Some (Signature s) -> Node (id, sig_items s.items) :: acc
        | _ -> Leaf id :: acc)
    | ModuleSubstitution ms ->
        Leaf (Format.asprintf "module type %a" pp_id ms.id) :: acc
    | ModuleTypeSubstitution mts ->
        Leaf (Format.asprintf "module type %a" pp_id mts.id) :: acc
    | Type (_, t) -> Leaf (Format.asprintf "type %a" pp_id t.id) :: acc
    | TypeSubstitution ts -> Leaf (Format.asprintf "type %a" pp_id ts.id) :: acc
    | Exception e -> Leaf (Format.asprintf "exception %a" pp_id e.id) :: acc
    | Value v -> Leaf (Format.asprintf "val %a" pp_id v.id) :: acc
    | Class (_, c) -> (
        let id = Format.asprintf "class %a" pp_id c.id in
        match c.type_ with
        | ClassType ct -> (
            match ct with
            | Signature s -> Node (id, class_sig_items s.items) :: acc
            | _ -> Leaf id :: acc)
        | Arrow _ -> Leaf id :: acc)
    | ClassType (_, ct) -> (
        let id = Format.asprintf "class type %a" pp_id ct.id in
        match ct.expr with
        | Signature s -> Node (id, class_sig_items s.items) :: acc
        | Constr _ -> Leaf id :: acc)
    | _ -> acc

  and sig_items items = List.rev @@ List.fold_left sig_item [] items

  and class_sig_item acc = function
    | Odoc_model.Lang.ClassSignature.Method m ->
        Leaf (Format.asprintf "method %a" pp_id m.id) :: acc
    | InstanceVariable v -> Leaf (Format.asprintf "val %a" pp_id v.id) :: acc
    | _ -> acc

  and class_sig_items items = List.rev @@ List.fold_left class_sig_item [] items

  let packed_item acc (x : Odoc_model.Lang.Compilation_unit.Packed.item) =
    Leaf (Format.asprintf "module %a" pp_id x.id) :: acc

  let packed items = List.rev @@ List.fold_left packed_item [] items

  let of_unit (v : Odoc_model.Lang.Compilation_unit.t) : t =
    let id = Format.asprintf "module %a" pp_id v.id in
    match v.content with
    | Module m -> Node (id, sig_items m.items)
    | Pack p -> Node (id, packed p)

  let reference acc (r : Odoc_model.Paths.Reference.t) =
    match r with
    | `Module (_s, id) ->
        let id =
          Format.asprintf "module %a" Odoc_model.Names.ModuleName.fmt id
        in
        Leaf id :: acc
    | `ModuleType (_s, id) ->
        let id =
          Format.asprintf "module type %a" Odoc_model.Names.ModuleTypeName.fmt
            id
        in
        Leaf id :: acc
    | `Type (_s, id) ->
        let id = Format.asprintf "type %a" Odoc_model.Names.TypeName.fmt id in
        Leaf id :: acc
    | `Exception (_s, id) ->
        let id =
          Format.asprintf "exception %a" Odoc_model.Names.ExceptionName.fmt id
        in
        Leaf id :: acc
    | `Value (_s, id) ->
        let id = Format.asprintf "val %a" Odoc_model.Names.ValueName.fmt id in
        Leaf id :: acc
    | `Class (_s, id) ->
        let id = Format.asprintf "class %a" Odoc_model.Names.ClassName.fmt id in
        Leaf id :: acc
    | `ClassType (_s, id) ->
        let id =
          Format.asprintf "class type %a" Odoc_model.Names.ClassTypeName.fmt id
        in
        Leaf id :: acc
    | `Method (_s, id) ->
        let id =
          Format.asprintf "method %a" Odoc_model.Names.MethodName.fmt id
        in
        Leaf id :: acc
    | `InstanceVariable (_s, id) ->
        let id =
          Format.asprintf "val %a" Odoc_model.Names.InstanceVariableName.fmt id
        in
        Leaf id :: acc
    | _ -> acc

  let references (l : Odoc_model.Paths.Reference.t list) =
    List.rev @@ List.fold_left reference [] l

  let of_page (p : Odoc_model.Lang.Page.t) : t =
    (* ignore page name *)
    (*let id = Format.asprintf "page %a" pp_id p.name in*)
    let children = references p.children in
    Node ("", children)
end
