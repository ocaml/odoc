open Odoc_document

let to_list url =
  let rec loop acc {Url.Path. parent ; name ; kind } =
    match parent with
    | None -> (kind,name) :: acc
    | Some p -> loop ((kind, name) :: acc) p
  in
  loop [] url

let for_printing url = List.map snd @@ to_list url

let segment_to_string (kind, name) =
  if kind = "module" || kind = "package" || kind = "class" || kind = "page"
  then name
  else Printf.sprintf "%s-%s" kind name

let as_filename (url : Url.Path.t) =
  let rec get_components {Url.Path. parent ; name ; kind } =
    if kind = "package" then
      name, []
    else
      match parent with
      | None -> assert false
      | Some p ->
        let dir, path = get_components p in
        dir, segment_to_string (kind, name)::path
  in
  let dir, path = get_components url in
  let s = String.concat "." @@ List.rev path in
  Fpath.(v dir / s + ".3o")

let rec is_class_or_module_path (url : Url.Path.t) = match url.kind with
  | "module" | "package" | "class" ->
    begin match url.parent with
    | None -> true
    | Some url -> is_class_or_module_path url
    end
  | _ -> false

let should_inline x = not @@ is_class_or_module_path x

let files_of_url url =
  if is_class_or_module_path url then
    [as_filename url]
  else
    []
