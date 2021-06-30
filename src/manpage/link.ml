open Odoc_document

let for_printing url = List.map snd @@ Url.Path.to_list url

let segment_to_string (kind, name) =
  match kind with
  | `Module | `ContainerPage | `Page | `Class -> name
  | _ -> Format.asprintf "%a-%s" Odoc_document.Url.Path.pp_kind kind name

let as_filename (url : Url.Path.t) =
  let components = Url.Path.to_list url in
  let dir, path =
    Url.Path.split
      ~is_dir:(function `ContainerPage -> true | _ -> false)
      components
  in
  let dir = List.map segment_to_string dir in
  let path = String.concat "." (List.map segment_to_string path) in
  Fpath.(v (String.concat dir_sep (dir @ [ path ])) + ".3o")

let rec is_class_or_module_path (url : Url.Path.t) =
  match url.kind with
  | `Module | `Page | `ContainerPage | `Class -> (
      match url.parent with
      | None -> true
      | Some url -> is_class_or_module_path url)
  | _ -> false

let should_inline x = not @@ is_class_or_module_path x

let files_of_url url =
  if is_class_or_module_path url then [ as_filename url ] else []
