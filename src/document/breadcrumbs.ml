open Odoc_model
open Names
module Id = Paths.Identifier

type t = (string * Url.Path.t) list

let empty = []

let of_lang ~index id =
  let module H = Id.Hashtbl.Page in
  let mk_seg id title =
    (title, Url.Path.from_identifier (id :> Url.Path.any))
  in
  let mk_page_seg id title =
    let title =
      match index with
      | Some index -> (
          try H.find index.Lang.Index.pages_short_title (id :> Id.Page.t)
          with Not_found -> title)
      | None -> title
    in
    mk_seg id title
  in
  let rec of_page acc id =
    match id.Id.iv with
    | `Page (parent, pname) ->
        of_parent (mk_page_seg id (PageName.to_string pname) :: acc) parent
  and of_parent acc = function Some p -> of_page acc p | None -> acc in
  let of_odocid (id : Id.OdocId.t) =
    match id with
    | { Id.iv = #Id.ContainerPage.t_pv; _ } as id -> of_page [] id
    | { iv = `LeafPage (parent, pname); _ } as id ->
        of_parent [ mk_page_seg id (PageName.to_string pname) ] parent
    | { iv = `Root (parent, mname); _ } as id ->
        of_parent [ mk_seg id (ModuleName.to_string mname) ] parent
    | { iv = `SourcePage (parent, name); _ } as id ->
        of_page [ mk_seg id ("Source " ^ name) ] parent
    | { iv = `Implementation _ | `AssetFile _; _ } -> []
  in
  List.rev (of_odocid id)
