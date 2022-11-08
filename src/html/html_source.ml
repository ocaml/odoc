type 'a source_document = 'a source_span list
and 'a source_span = Tagged of 'a * 'a source_document | Leaf of string

let doc_of_poses src posl =
  let l =
    posl
    |> List.sort (fun a b ->
           match (a, b) with
           | ( (_, { Location.loc_start = l1; loc_end = e1; _ }),
               (_, { loc_start = l2; loc_end = e2; _ }) ) ->
               if l1.pos_cnum = l2.pos_cnum then
                 Int.compare e2.pos_cnum e1.pos_cnum
                 (* If two intervals open at the same time, we open
                    first the one that closes last *)
               else Int.compare l1.pos_cnum l2.pos_cnum)
  in
  let get_src a b =
    let a, b = (a.Lexing.pos_cnum, b.Lexing.pos_cnum) in
    String.sub src a (b - a)
  in
  let leaf = function "" -> [] | s -> [ Leaf s ] in
  let ( < ) a b = a.Lexing.pos_cnum < b.Lexing.pos_cnum in
  let min a b = if a < b then a else b in
  let rec extract from to_ list aux =
    match list with
    | (k, { Location.loc_start; loc_end; _ }) :: q when loc_start < to_ ->
        let loc_end = min loc_end to_ in
        (* In case of inconsistent [a  [b    a] b]
           we do                   [a  [b  b]a] *)
        let initial = leaf (get_src from loc_start) in
        let next, q = extract loc_start loc_end q [] in
        extract loc_end to_ q ([ Tagged (k, List.rev next) ] @ initial @ aux)
    | q -> (leaf (get_src from to_) @ aux, q)
  in
  let doc, _ =
    extract
      Lexing.{ pos_bol = 0; pos_cnum = 0; pos_fname = ""; pos_lnum = 1 }
      Lexing.
        {
          pos_bol = 0;
          pos_cnum = String.length src;
          pos_fname = "";
          pos_lnum = 1;
        }
      l []
  in
  List.rev doc

module Html = Tyxml.Html

let docs_to_html docs =
  let rec doc_to_html doc =
    match doc with
    | Leaf s -> Html.span ~a:[] [ Html.txt s ]
    | Tagged (Types.Token tok, docs) ->
        let children = List.map doc_to_html docs in
        Html.span
          ~a:[ Html.a_class [ Syntax_highlighter.tag_of_token tok ] ]
          children
  in
  Html.span ~a:[] @@ List.map doc_to_html docs

let doc_of_locs src locs =
  let locs = locs @ Syntax_highlighter.syntax_highlighting_locs src in
  Html.pre ~a:[] [ Html.code ~a:[] [ docs_to_html (doc_of_poses src locs) ] ]
