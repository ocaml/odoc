open Odoc_document

let for_printing url = List.map snd @@ Url.Path.to_list url

let segment_to_string (kind, name) =
  match kind with
  | `Module | `Page | `LeafPage | `Class -> name
  | _ -> Format.asprintf "%a-%s" Odoc_document.Url.Path.pp_kind kind name

let as_filename (url : Url.Path.t) =
  let components = Url.Path.to_list url in
  let dir, path =
    Url.Path.split
      ~is_dir:(function `Page -> `IfNotLast | _ -> `Never)
      components
  in
  let dir = List.map segment_to_string dir in
  let path = String.concat "." (List.map segment_to_string path) in
  let str_path = String.concat Fpath.dir_sep (dir @ [ path ]) in
  Fpath.(v str_path + ".md")

let href ~base_path (url : Url.t) =
  let anchor = match url.anchor with "" -> "" | anchor -> "#" ^ anchor in
  if url.page = base_path then anchor
  else
    let root = Fpath.parent (as_filename base_path)
    and path = as_filename url.page in
    let path =
      match Fpath.relativize ~root path with
      | Some path -> path
      | None -> assert false
    in
    Fpath.to_string path ^ anchor

let should_inline _ = false

let files_of_url url =
  if should_inline url then [] else [ as_filename url ]
