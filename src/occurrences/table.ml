let get_key (id : Odoc_model.Paths.Identifier.t) = id.ikey

module H = Hashtbl.Make (String)

type impl_src = {filepath: string list; line_number: int}
type t = item H.t
and item = { direct : int; indirect : int ; impl_src : impl_src option }
type key = string
type identifier = Odoc_model.Paths.Identifier.t

let v_item () = { direct = 0; indirect = 0; impl_src = None }

let v () = H.create 0

let add ?(quantity = 1) tbl id impl_src =
  let rec add ?(kind = `Indirect) id =
    let incr id =
      let { direct; indirect ; impl_src=impl_src_old} =
        try H.find tbl (get_key id) with Not_found -> v_item ()
      in
      let impl_src =
        if Option.is_none impl_src || kind <> `Direct
        then impl_src_old
        else impl_src in
      let direct, indirect =
        match kind with
        | `Direct -> (direct + quantity, indirect)
        | `Indirect -> (direct, indirect + quantity)
      in
      H.replace tbl (get_key id) { direct; indirect ; impl_src };
    in
    let do_ parent =
      add (parent :> identifier);
      incr id
    in
    match id.iv with
    | `InstanceVariable (parent, _) -> do_ parent
    | `Parameter (parent, _) -> do_ parent
    | `Module (parent, _) -> do_ parent
    | `ModuleType (parent, _) -> do_ parent
    | `Method (parent, _) -> do_ parent
    | `Field (parent, _) -> do_ parent
    | `UnboxedField (parent, _) -> do_ parent
    | `Extension (parent, _) -> do_ parent
    | `Type (parent, _) -> do_ parent
    | `Constructor (parent, _) -> do_ parent
    | `Exception (parent, _) -> do_ parent
    | `ExtensionDecl (parent, _, _) -> do_ parent
    | `Class (parent, _) -> do_ parent
    | `Value (parent, _) -> do_ parent
    | `ClassType (parent, _) -> do_ parent
    | `Root _ -> incr id
    | `SourcePage _ | `Page _ | `LeafPage _ | `SourceLocation _ | `Label _
    | `SourceLocationMod _ | `Result _ | `AssetFile _
    | `SourceLocationInternal _ ->
        assert false
  in
  add ~kind:`Direct id

let add_entries item item_opt =
  match item_opt with
  | Some { direct; indirect ; impl_src } ->
    let direct = item.direct + direct in
    let indirect = item.indirect + indirect in
    let impl_src = if Option.is_some item.impl_src then item.impl_src else impl_src in
      {direct ; indirect ; impl_src}
  | None -> item

let get t id = H.find_opt t (get_key id)

let iter = H.iter

let merge_into ~src ~dst =
  iter (fun key item ->
    let dst_item = H.find_opt dst key in
    let new_item = add_entries item dst_item in
    H.replace dst key new_item
  ) src

module Deftbl = struct
  type key = Odoc_model.Paths.Identifier.SourceLocation.t
  type item = impl_src
  module H = Hashtbl.Make (String)
  type t = item H.t

  let v () = H.create 0

  let add t (k: key) (v: item) = H.add t k.ikey v

  let get t (k: key) = H.find_opt t k.ikey
end
