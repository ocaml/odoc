type 'a source_document = 'a source_span list
and 'a source_span = Tagged of 'a * 'a source_document | Leaf of string

let doc_of_poses src posl =
  let l =
    posl
    |> List.sort (fun (_, (l1, e1)) (_, (l2, e2)) ->
           if l1 = l2 then Int.compare e2 e1
             (* If two intervals open at the same time, we open
                first the one that closes last *)
           else Int.compare l1 l2)
  in
  let get_src a b = String.sub src a (b - a) in
  let leaf = function "" -> [] | s -> [ Leaf s ] in
  let min (a : int) b = if a < b then a else b in
  let rec extract from to_ list aux =
    match list with
    | (k, (loc_start, loc_end)) :: q when loc_start < to_ ->
        let loc_end = min loc_end to_ in
        (* In case of inconsistent [a  [b    a] b]
           we do                   [a  [b  b]a] *)
        let initial = leaf (get_src from loc_start) in
        let next, q = extract loc_start loc_end q [] in
        extract loc_end to_ q ([ Tagged (k, List.rev next) ] @ initial @ aux)
    | q -> (leaf (get_src from to_) @ aux, q)
  in
  let doc, _ = extract 0 (String.length src) l [] in
  List.rev doc

module Html = Tyxml.Html

let docs_to_html docs =
  let rec doc_to_html doc =
    match doc with
    | Leaf s -> Html.txt s
    | Tagged (Types.Token tok, docs) ->
        let children = List.map doc_to_html docs in
        Html.span
          ~a:[ Html.a_class [ Syntax_highlighter.tag_of_token tok ] ]
          children
    | Tagged (Types.Line l, docs) ->
        let children = List.map doc_to_html docs in
        Html.span ~a:[ Html.a_id (Printf.sprintf "L%d" l) ] children
  in
  Html.span ~a:[] @@ List.map doc_to_html docs

let doc_of_locs src locs =
  let syntax_locs =
    Syntax_highlighter.syntax_highlighting_locs src
    |> List.rev_map (fun (x, l) -> (Types.Token x, l))
    (* The order won't matter and input can be large *)
  in
  let lines_locs =
    Source_line_splitting.lines_locs src
    |> List.rev_map (fun (x, l) -> (Types.Line x, l))
  in
  let locs = List.rev_append locs syntax_locs in
  let locs = List.rev_append locs lines_locs in
  Html.pre ~a:[] [ Html.code ~a:[] [ docs_to_html (doc_of_poses src locs) ] ]
