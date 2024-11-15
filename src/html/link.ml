module Url = Odoc_document.Url

type link = Relative of string list * string | Absolute of string

(* Translation from Url.Path *)
module Path = struct
  let for_printing url = List.map snd @@ Url.Path.to_list url

  let segment_to_string (kind, name) =
    Format.asprintf "%a%s" Url.Path.pp_disambiguating_prefix kind name

  let is_leaf_page url = url.Url.Path.kind = `LeafPage

  let remap config f =
    let l = String.concat "/" f in
    let remaps =
      List.filter
        (fun (prefix, _replacement) -> Astring.String.is_prefix ~affix:prefix l)
        (Config.remap config)
    in
    let remaps =
      List.sort
        (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
        remaps
    in
    match remaps with
    | [] -> None
    | (prefix, replacement) :: _ ->
        let len = String.length prefix in
        let l = String.sub l len (String.length l - len) in
        Some (replacement ^ l)

  let get_dir_and_file ~config url =
    let l = Url.Path.to_list url in
    let is_dir =
      if Config.flat config then function `Page -> `Always | _ -> `Never
      else function `LeafPage | `File | `SourcePage -> `Never | _ -> `Always
    in
    let dir, file = Url.Path.split ~is_dir l in
    let dir = List.map segment_to_string dir in
    let file =
      match file with
      | [] -> "index.html"
      | [ (`LeafPage, name) ] -> name ^ ".html"
      | [ (`File, name) ] -> name
      | [ (`SourcePage, name) ] -> name ^ ".html"
      | xs ->
          assert (Config.flat config);
          String.concat "-" (List.map segment_to_string xs) ^ ".html"
    in
    (dir, file)

  let for_linking ~config url =
    let dir, file = get_dir_and_file ~config url in
    match remap config dir with
    | None -> Relative (dir, file)
    | Some x -> Absolute (x ^ "/" ^ file)

  let as_filename ~config (url : Url.Path.t) =
    let dir, file = get_dir_and_file ~config url in
    Fpath.(v @@ String.concat Fpath.dir_sep (dir @ [ file ]))
end

type resolve = Current of Url.Path.t | Base of string

let rec drop_shared_prefix l1 l2 =
  match (l1, l2) with
  | l1 :: l1s, l2 :: l2s when l1 = l2 -> drop_shared_prefix l1s l2s
  | _, _ -> (l1, l2)

let href ~config ~resolve t =
  let { Url.Anchor.page; anchor; _ } = t in
  let add_anchor y = match anchor with "" -> y | anchor -> y ^ "#" ^ anchor in
  let target_loc = Path.for_linking ~config page in

  match target_loc with
  | Absolute y -> add_anchor y
  | Relative (dir, file) -> (
      let target_loc = dir @ [ file ] in
      (* If xref_base_uri is defined, do not perform relative URI resolution. *)
      match resolve with
      | Base xref_base_uri ->
          let page = xref_base_uri ^ String.concat "/" target_loc in
          add_anchor page
      | Current path -> (
          let current_loc =
            let dir, file = Path.get_dir_and_file ~config path in
            dir @ [ file ]
          in

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
                List.map (fun _ -> "..") (List.tl l)
                @ target_from_common_ancestor
          in
          let remove_index_html l =
            match List.rev l with
            | "index.html" :: rest -> List.rev ("" :: rest)
            | _ -> l
          in
          let relative_target =
            if Config.semantic_uris config then
              remove_index_html relative_target
            else relative_target
          in
          match (relative_target, anchor) with
          | [], "" -> "#"
          | page, _ -> add_anchor @@ String.concat "/" page))
