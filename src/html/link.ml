module Url = Odoc_document.Url

let flat = ref false

(* Translation from Url.Path *)
module Path = struct
  let for_printing url = List.map snd @@ Url.Path.to_list url

  let segment_to_string (kind, name) =
    match kind with
    | `Module | `ContainerPage -> name
    | _ -> Format.asprintf "%a-%s" Url.Path.pp_kind kind name

  let is_leaf_page url = url.Url.Path.kind = `Page

  let get_dir_and_file url =
    let l = Url.Path.to_list url in
    let is_dir =
      if !flat then function `ContainerPage -> true | _ -> false
      else function `Page -> false | _ -> true
    in
    let dir, file = Url.Path.split ~is_dir l in
    let dir = List.map segment_to_string dir in
    let file =
      match file with
      | [] -> "index.html"
      | [ (`Page, name) ] -> name ^ ".html"
      | xs ->
          assert !flat;
          String.concat "-" (List.map segment_to_string xs) ^ ".html"
    in
    (dir, file)

  let for_linking url =
    let dir, file = get_dir_and_file url in
    dir @ [ file ]

  let as_filename (url : Url.Path.t) =
    Fpath.(v @@ String.concat Fpath.dir_sep @@ for_linking url)
end

let semantic_uris = ref false

type resolve = Current of Url.Path.t | Base of string

let rec drop_shared_prefix l1 l2 =
  match (l1, l2) with
  | l1 :: l1s, l2 :: l2s when l1 = l2 -> drop_shared_prefix l1s l2s
  | _, _ -> (l1, l2)

let href ~resolve t =
  let { Url.Anchor.page; anchor; _ } = t in

  let target_loc = Path.for_linking page in

  (* If xref_base_uri is defined, do not perform relative URI resolution. *)
  match resolve with
  | Base xref_base_uri -> (
      let page = xref_base_uri ^ String.concat "/" target_loc in
      match anchor with "" -> page | anchor -> page ^ "#" ^ anchor)
  | Current path -> (
      let current_loc = Path.for_linking path in

      let current_from_common_ancestor, target_from_common_ancestor =
        drop_shared_prefix current_loc target_loc
      in

      let relative_target =
        match current_from_common_ancestor with
        | [] ->
            (* We're already on the right page *)
            (* If we're already on the right page, the target from our common
                ancestor can't be anything other than the empty list *)
            assert (target_from_common_ancestor = []);
            []
        | [ _ ] ->
            (* We're already in the right dir *)
            target_from_common_ancestor
        | l ->
            (* We need to go up some dirs *)
            List.map (fun _ -> "..") (List.tl l) @ target_from_common_ancestor
      in
      let remove_index_html l =
        match List.rev l with
        | "index.html" :: rest -> List.rev ("" :: rest)
        | _ -> l
      in
      let relative_target =
        if !semantic_uris then remove_index_html relative_target
        else relative_target
      in
      match (relative_target, anchor) with
      | [], "" -> "#"
      | page, "" -> String.concat "/" page
      | page, anchor -> String.concat "/" page ^ "#" ^ anchor)
