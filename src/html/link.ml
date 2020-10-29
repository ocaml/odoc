module Url = Odoc_document.Url

(* Translation from Url.Path *)
module Path = struct

  let to_list url =
    let rec loop acc {Url.Path. parent ; name ; kind } =
      match parent with
      | None -> (kind,name) :: acc
      | Some p -> loop ((kind, name) :: acc) p
    in
    loop [] url

  let for_printing url = List.map snd @@ to_list url

  let segment_to_string (kind, name) =
    if kind = "module" || kind = "package" || kind = "page"
    then name
    else Printf.sprintf "%s-%s" kind name
  let for_linking url = List.map segment_to_string @@ to_list url

  let is_page url = (url.Url.Path.kind = "page")

  let rec get_dir {Url.Path. parent ; name ; kind} =
    let s = segment_to_string (kind, name) in
    match parent with
    | None -> Fpath.v s
    | Some p -> Fpath.(get_dir p / s)

  let as_filename (url : Url.Path.t) =
    if is_page url then
      Fpath.(get_dir url + ".html")
    else
      Fpath.(get_dir url / "index.html")
end

let semantic_uris = ref false

type resolve =
  | Current of Url.Path.t
  | Base of string

let rec drop_shared_prefix l1 l2 =
  match l1, l2 with
  | l1 :: l1s, l2 :: l2s when l1 = l2 ->
    drop_shared_prefix l1s l2s
  | _, _ -> l1, l2

let href ~resolve { Url.Anchor. page; anchor; kind } =
  let leaf = if !semantic_uris || kind = "page" then [] else ["index.html"] in
  let target = Path.for_linking page @ leaf in
  match resolve with
  (* If xref_base_uri is defined, do not perform relative URI resolution. *)
  | Base xref_base_uri ->
    let page = xref_base_uri ^ String.concat "/" target in
    begin match anchor with
    | "" -> page
    | anchor -> page ^ "#" ^ anchor
    end
  | Current path ->
    let current_loc =
      let l = Path.for_linking path in
      if Path.is_page path then
        (* Sadness. *)
        List.tl l
      else l
    in
    let current_from_common_ancestor, target_from_common_ancestor =
      drop_shared_prefix current_loc target
    in
    let relative_target =
      List.map (fun _ -> "..") current_from_common_ancestor
      @ target_from_common_ancestor
    in
    let page = String.concat "/" relative_target in
    let page = if kind="page" then page ^ ".html" else page in
    begin match anchor with
    | "" -> page
    | anchor -> page ^ "#" ^ anchor
    end
