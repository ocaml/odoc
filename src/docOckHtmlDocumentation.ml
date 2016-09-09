open DocOck
open Types
open Tyxml.Html

module Html_tree = DocOckHtmlHtml_tree
module Markup = DocOckHtmlMarkup

let html_dot_magic = List.map ~f:(fun x -> tot @@ toelt x)

module Html_parser = struct
  let mk_attr ((_, local), value) = Xml.string_attrib local value

  let of_string str =
    let source = `String (0, str) in
    let input  = Xmlm.make_input source in
    Xmlm.input_tree input ~data:Xml.pcdata ~el:(fun ((_, name), attrs) subs ->
      let attrs = List.map attrs ~f:mk_attr in
      match subs with
      | [] -> Xml.leaf ~a:attrs name
      | _  -> Xml.node ~a:attrs name subs
    )
    |> tot
end

let apply_style (style : Documentation.style) elt =
  match style with
  | Bold        -> span ~a:[ a_style "font-weight: bold" ] elt
  | Italic      -> span ~a:[ a_style "font-style: italic" ] elt
  | Emphasize   -> span ~a:[ a_style "font-style: italic" ] elt
  | Center      -> span ~a:[ a_style "text-align:center"] elt
  | Left        -> span ~a:[ a_style "text-align:left"] elt
  | Right       -> span ~a:[ a_style "text-align:right"] elt
  | Superscript -> span ~a:[ a_class ["sup"] ] elt
  | Subscript   -> span ~a:[ a_class ["sub"] ] elt
  | Custom str  -> span ~a:[ a_style str ] elt

let ref_to_link (ref : _ Documentation.reference) =
  (* It is wonderful that although each these [r] is a [Reference.t] the phantom
     type parameters are not the same so we can't merge the branches. *)
  match ref with
  | Module r           -> Html_tree.Relative_link.of_reference r
  | ModuleType r       -> Html_tree.Relative_link.of_reference r
  | Type r             -> Html_tree.Relative_link.of_reference r
  | Constructor r      -> Html_tree.Relative_link.of_reference r
  | Field r            -> Html_tree.Relative_link.of_reference r
  | Extension r        -> Html_tree.Relative_link.of_reference r
  | Exception r        -> Html_tree.Relative_link.of_reference r
  | Value r            -> Html_tree.Relative_link.of_reference r
  | Class r            -> Html_tree.Relative_link.of_reference r
  | ClassType r        -> Html_tree.Relative_link.of_reference r
  | Method r           -> Html_tree.Relative_link.of_reference r
  | InstanceVariable r -> Html_tree.Relative_link.of_reference r
  | Element r          -> Html_tree.Relative_link.of_reference r
  | Section r          -> Html_tree.Relative_link.of_reference r
  | Link _
  | Custom (_,_)
    -> [ pcdata "[documentation.handle_ref TODO]" ]


let rec handle_text text =
  let mk_item txt = li (handle_text txt) in
  List.concat @@ List.map text ~f:(function
    | Documentation.Raw str -> [ pcdata str ]
    | Code str -> [ code [ pcdata str ] ]
    | PreCode str -> [ pre [ pcdata str ] ]
    | Verbatim str ->
      (* CR trefis: I don't think this is quite right. *)
      [ pre [ pcdata str ] ]
    | Style (style, txt) -> [ apply_style style (html_dot_magic @@ handle_text txt) ]
    | List elts -> [ul (List.map elts ~f:mk_item)]
    | Enum elts -> [ol (List.map elts ~f:mk_item)]
    | Newline -> [br ()]
    | Title (lvl, _label, txt) ->
      (* CR trefis: don't ignore the label. *)
      let header_fun =
        match lvl with
        | 1 -> h1
        | 2 -> h2
        | 3 -> h3
        | 4 -> h4
        | 5 -> h5
        | _ -> h6
      in
      let header_fun =
        match _label with
        | None -> header_fun ~a:[]
        | Some (Paths.Identifier.Label (_, lbl)) -> header_fun ~a:[ a_id lbl ]
      in
      [ header_fun (html_dot_magic @@ handle_text txt) ]
    | Reference (r,_) -> ref_to_link r
    | Target (Some "html", str) ->
      let html = Html_parser.of_string str in
      [html]
    | Target (_, str) ->
      (* CR trefis: I treated this as verbatim but the manual says it should
          be ignored. So maybe don't generate anything here? *)
      [ pre [ pcdata str ] ]
    | Special _ -> [pcdata "TODO"]
  )

let first_to_html (t : _ Documentation.t) =
  match t with
  | Ok { text = first :: _ ; _ } ->
    div ~a:[ a_class ["doc"] ] (handle_text [first])
  | _ -> p []

let to_html (t : _ Documentation.t) =
  match t with
  | Error _ -> p []
  | Ok body -> div ~a:[ a_class ["doc"] ] (handle_text body.text)

let has_doc (t : _ Types.Documentation.t) =
  match t with
  | Ok body -> body.text <> []
  | Error _ -> false

