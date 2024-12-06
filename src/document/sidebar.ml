open Odoc_utils
open Types
module Id = Odoc_model.Paths.Identifier

type entry = {
  url : Url.t;
  valid_link : bool;
  content : Inline.t;
  toc_status : [ `Open | `Hidden ] option;
}

open Odoc_index

module Toc : sig
  type t = entry Tree.t

  val of_page_hierarchy : Skeleton.t -> t

  val to_block : prune:bool -> Url.Path.t -> t -> Block.t
end = struct
  type t = entry Tree.t

  let to_block ~prune:_ (current_url : Url.Path.t) (tree : t) =
    let block_tree_of_t (current_url : Url.Path.t) (tree : t) =
      (* When transforming the tree, we use a filter_map to remove the nodes that
         are irrelevant for the current url. However, we always want to keep the
         root. So we apply the filter_map starting from the first children. *)
      let convert_entry { url; valid_link; content; _ } =
        let link =
          if valid_link then
            let target = Target.Internal (Target.Resolved url) in
            let attr =
              if url.page = current_url && Astring.String.equal url.anchor ""
              then [ "current_unit" ]
              else []
            in
            [ inline ~attr @@ Inline.Link { target; content; tooltip = None } ]
          else content
        in
        Types.block @@ Inline link
      in
      let rec convert n =
        let children =
          match n.Tree.node with
          | { url; valid_link = true; toc_status = None; _ }
            when not (Url.Path.is_prefix url.Url.Anchor.page current_url) ->
              []
          | _ -> List.map convert n.children
        in
        { Tree.node = convert_entry n.node; children }
      in
      convert tree
    in
    let rec block_of_block_tree { Tree.node = name; children = content } =
      let content =
        match content with
        | [] -> []
        | _ :: _ ->
            let content = List.map block_of_block_tree content in
            [ block (Block.List (Block.Unordered, content)) ]
      in
      name :: content
    in
    let block_tree = block_tree_of_t current_url tree in
    block_of_block_tree block_tree

  let of_page_hierarchy ({ node = entry; children } : Entry.t Tree.t) : t =
    let map_entry entry =
      match entry.Entry.kind with
      | Dir ->
          let url = Url.from_identifier ~stop_before:false (entry.id :> Id.t) in
          {
            url;
            valid_link = false;
            content = [ inline @@ Text (Id.name entry.id) ];
            toc_status = None;
          }
      | _ ->
          let stop_before =
            match entry.Entry.kind with
            | ModuleType { has_expansion } | Module { has_expansion } ->
                not has_expansion
            | _ -> false
          in
          let url = Url.from_identifier ~stop_before (entry.id :> Id.t) in
          let toc_status =
            match entry.kind with
            | Page { toc_status; _ } -> toc_status
            | _ -> None
          in
          let content =
            match entry.kind with
            | Page { short_title = Some st; _ } -> Comment.link_content st
            | Page { short_title = None; _ } ->
                let title =
                  let open Odoc_model in
                  match Comment.find_zero_heading entry.doc with
                  | Some t -> t
                  | None ->
                      let name =
                        match entry.id.iv with
                        | `LeafPage (Some parent, name)
                          when Astring.String.equal
                                 (Names.PageName.to_string name)
                                 "index" ->
                            Id.name parent
                        | _ -> Id.name entry.id
                      in
                      Location_.[ at (span []) (`Word name) ]
                in
                Comment.link_content title
            | _ ->
                let name = Odoc_model.Paths.Identifier.name entry.id in
                [ inline (Text name) ]
          in
          let valid_link =
            match entry.kind with
            | Page { toc_status = Some `Hidden; _ } -> false
            | _ -> true
          in
          { url; content; toc_status; valid_link }
    in
    let f x =
      match x.Entry.kind with
      | Dir | Page _ | Module _ | Class_type _ | Class _ | ModuleType _ | Impl
        ->
          Some (map_entry x)
      | _ -> None
    in
    let entry = map_entry entry in
    let children = Forest.filter_map ~f children in
    { Tree.node = entry; children }
end

type t = Toc.t list

let of_index (v : Odoc_index.t) = List.map Toc.of_page_hierarchy v

let to_block (sidebar : t) path =
  let sb = List.map (Toc.to_block ~prune:true path) sidebar in
  [ block (Block.List (Block.Unordered, sb)) ]
