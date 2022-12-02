open Odoc_document.Types

let html_of_doc docs =
  let open Tyxml.Html in
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
  let rec doc_to_html doc =
    match doc with
    | Source_page.Plain_code s -> txt s
    | Tagged_code (info, docs) -> (
        let children = List.map doc_to_html docs in
        match info with
        | Token tok -> span ~a:[ a_class [ tok ] ] children
        | Line l ->
            span
              ~a:[ a_id (Printf.sprintf "L%d" l); a_class [ "source_line" ] ]
              children
        | Local_jmp (Occurence lbl) -> a ~a:[ a_href ("#def-" ^ lbl) ] children
        | Local_jmp (Def lbl) -> span ~a:[ a_id ("def-" ^ lbl) ] children)
  in
  span ~a:[] @@ List.map doc_to_html docs

let html_of_doc doc =
  Tyxml.Html.pre ~a:[] [ Tyxml.Html.code ~a:[] [ html_of_doc doc ] ]
