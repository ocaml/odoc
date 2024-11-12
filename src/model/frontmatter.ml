type child = Page of string | Dir of string

type line = Children_order of child Location_.with_location list

type children_order = child Location_.with_location list Location_.with_location

type t = { children_order : children_order option }

let empty = { children_order = None }

let apply fm line =
  match (line.Location_.value, fm) with
  | Children_order children_order, { children_order = None } ->
      { children_order = Some (Location_.same line children_order) }
  | Children_order _, { children_order = Some _ } ->
      (* TODO raise warning about duplicate children field *) fm

let parse_child c =
  if Astring.String.is_suffix ~affix:"/" c then
    let c = String.sub c 0 (String.length c - 1) in
    Dir c
  else Page c

let parse_children_order loc co =
  let rec parse_words acc words =
    match words with
    | [] -> Ok (Location_.at loc (Children_order (List.rev acc)))
    | ({ Location_.value = `Word word; _ } as w) :: tl ->
        parse_words ({ w with value = parse_child word } :: acc) tl
    | { Location_.value = `Space _; _ } :: tl -> parse_words acc tl
    | { location; _ } :: _ ->
        Error
          (Error.make "Only words are accepted when specifying children order"
             location)
  in
  match co with
  | [ { Location_.value = `Paragraph words; _ } ] -> parse_words [] words
  | _ ->
      Error
        (Error.make "Only words are accepted when specifying children order" loc)

let of_lines lines = List.fold_left apply empty lines
