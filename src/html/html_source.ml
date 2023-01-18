open Odoc_model.Lang
open Odoc_document.Types
open Tyxml

let html_of_doc docs =
  let open Html in
  let a :
      ( [< Html_types.a_attrib ],
        [< Html_types.span_content_fun ],
        [> Html_types.span ] )
      star =
    Unsafe.node "a"
    (* Makes it possible to use <a> inside span. Although this is not standard (see
        https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Content_categories)
        it is validated by the {{:https://validator.w3.org/nu/#textarea}W3C}. *)
  in
  (* [a] tags should not contain in other [a] tags. If this happens, browsers
     start to be really weird. If PPX do bad things, such a situation could
     happen. We manually avoid this situation. *)
  let rec doc_to_html ~is_in_a doc =
    match doc with
    | Source_page.Plain_code s -> [ txt s ]
    | Tagged_code (info, docs) -> (
        let children = List.concat @@ List.map (doc_to_html ~is_in_a) docs in
        match info with
        | Source_code.Info.Syntax tok ->
            [ span ~a:[ a_class [ tok ] ] children ]
        | Line _ -> children
        | Local_jmp (Occurence { anchor }) ->
            if is_in_a then children
            else
              let children =
                List.concat @@ List.map (doc_to_html ~is_in_a:true) docs
              in
              [ a ~a:[ a_href ("#" ^ anchor) ] children ]
        | Local_jmp (Def lbl) -> [ span ~a:[ a_id lbl ] children ])
  in
  span ~a:[] @@ List.concat @@ List.map (doc_to_html ~is_in_a:false) docs

(** Traverse the doc to find the last [Line] number. *)
let rec count_lines_in_span = function
  | Source_page.Plain_code _ -> 0
  | Tagged_code (Source_code.Info.Line l, docs) -> max (count_lines docs) l
  | Tagged_code (_, docs) -> count_lines docs

and count_lines = function
  | [] -> 0
  | hd :: tl -> max (count_lines_in_span hd) (count_lines tl)

let rec line_numbers acc n =
  let open Html in
  if n < 1 then acc
  else
    let l = string_of_int n in
    let anchor =
      a
        ~a:[ a_id ("L" ^ l); a_class [ "source_line" ]; a_href ("#L" ^ l) ]
        [ txt l ]
    in
    line_numbers (anchor :: txt "\n" :: acc) (n - 1)

let html_of_doc docs =
  let open Html in
  pre
    ~a:[ a_class [ "source_container" ] ]
    [
      code
        ~a:[ a_class [ "source_line_column" ] ]
        (line_numbers [] (count_lines docs));
      code ~a:[ a_class [ "source_code" ] ] [ html_of_doc docs ];
    ]
