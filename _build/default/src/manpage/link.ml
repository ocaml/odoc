open Odoc_document

let for_printing url = List.map snd @@ Url.Path.to_list url

let segment_to_string (kind, name) =
  Format.asprintf "%a%s" Url.Path.pp_disambiguating_prefix kind name

let as_filename ?(add_ext = true) (url : Url.Path.t) =
  let components = Url.Path.to_list url in
  let dir, path =
    Url.Path.split
      ~is_dir:(function `Page -> `IfNotLast | _ -> `Never)
      components
  in
  let dir = List.map segment_to_string dir in
  let path = String.concat "." (List.map segment_to_string path) in
  let str_path = String.concat Fpath.dir_sep (dir @ [ path ]) in
  if add_ext then Fpath.(v str_path + ".3o") else Fpath.v str_path

let rec is_class_or_module_path (url : Url.Path.t) =
  match url.kind with
  | `Module | `LeafPage | `Page | `Class -> (
      match url.parent with
      | None -> true
      | Some url -> is_class_or_module_path url)
  | _ -> false

let should_inline x = not @@ is_class_or_module_path x
