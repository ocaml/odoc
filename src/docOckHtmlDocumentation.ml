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

let ref_to_link ~get_package ?text (ref : _ Documentation.reference) =
  (* It is wonderful that although each these [r] is a [Reference.t] the phantom
     type parameters are not the same so we can't merge the branches. *)
  match ref with
  | Module r           -> Html_tree.Relative_link.of_reference ~get_package r
  | ModuleType r       -> Html_tree.Relative_link.of_reference ~get_package r
  | Type r             -> Html_tree.Relative_link.of_reference ~get_package r
  | Constructor r      -> Html_tree.Relative_link.of_reference ~get_package r
  | Field r            -> Html_tree.Relative_link.of_reference ~get_package r
  | Extension r        -> Html_tree.Relative_link.of_reference ~get_package r
  | Exception r        -> Html_tree.Relative_link.of_reference ~get_package r
  | Value r            -> Html_tree.Relative_link.of_reference ~get_package r
  | Class r            -> Html_tree.Relative_link.of_reference ~get_package r
  | ClassType r        -> Html_tree.Relative_link.of_reference ~get_package r
  | Method r           -> Html_tree.Relative_link.of_reference ~get_package r
  | InstanceVariable r -> Html_tree.Relative_link.of_reference ~get_package r
  | Element r          -> Html_tree.Relative_link.of_reference ~get_package r
  | Section r          -> Html_tree.Relative_link.of_reference ~get_package r
  | Link s ->
    let text =
      match text with
      | Some l -> l
      | None -> [ pcdata s ]
    in
    [ a ~a:[ a_href s ] text ]
  | Custom (_,_)
    -> [ pcdata "[documentation.handle_ref TODO]" ]

type kind =
  | Phrasing : Html_types.phrasing elt list -> kind
  | Phrasing_without_interactive :
      Html_types.phrasing_without_interactive elt list -> kind
  | Flow5 : Html_types.flow5 elt list -> kind
  | Flow5_without_interactive :
      Html_types.flow5_without_interactive elt list -> kind
  | Newline : Html_types.phrasing_without_interactive elt list -> kind

let to_phrasing : kind -> Html_types.phrasing elt list = function
  | Phrasing_without_interactive l -> (l :> Html_types.phrasing elt list)
  | Phrasing l -> l
  | Newline [] -> []
  | Newline l -> invalid_arg "to_phrasing"
  | Flow5 l -> invalid_arg "to_phrasing"
  | Flow5_without_interactive l -> invalid_arg "to_phrasing"

let to_flow5 : kind -> Html_types.flow5 elt list = function
  | Phrasing l -> (l :> Html_types.flow5 elt list)
  | Phrasing_without_interactive l -> (l :> Html_types.flow5 elt list)
  | Newline [] -> []
  | Newline _ -> invalid_arg "to_flow5"
  | Flow5 l -> l
  | Flow5_without_interactive l -> (l :> Html_types.flow5 elt list)

let to_flow5_without_interactive
  : kind -> Html_types.flow5_without_interactive elt list =
  function
  | Phrasing_without_interactive l ->
    (l :> Html_types.flow5_without_interactive elt list)
  | Flow5_without_interactive l -> l
  | Newline [] -> []
  | Newline l -> invalid_arg "to_flow5_without_interactive"
  | Flow5 l -> invalid_arg "to_flow5_without_interactive"
  | Phrasing l -> invalid_arg "to_flow5_without_interactive"

let is_interactive = function
  | Phrasing _ -> true
  | Flow5 _ -> true
  | _ -> false

let is_phrasing = function
  | Flow5 _ -> false
  | Flow5_without_interactive _ -> false
  | _ -> true

let rec aggregate ~get_package lst =
  collapse (List.concat @@ List.map lst ~f:(format ~get_package))

and collapse : kind list -> kind list = function
  | [] -> []
  | (Phrasing p1) :: (Phrasing p2) :: rest ->
    collapse (Phrasing (p1 @ p2) :: rest)
  | (Phrasing_without_interactive p1) :: (Phrasing p2) :: rest ->
    let p1 = (p1 :> Html_types.phrasing elt list) in
    collapse (Phrasing (p1 @ p2) :: rest)
  | (Phrasing p1) :: (Phrasing_without_interactive p2) :: rest ->
    let p2 = (p2 :> Html_types.phrasing elt list) in
    collapse (Phrasing (p1 @ p2) :: rest)
  | (Phrasing_without_interactive p1) :: (Phrasing_without_interactive p2)
    :: rest ->
    collapse (Phrasing_without_interactive (p1 @ p2) :: rest)
  | Newline _ :: Newline _ :: rest ->
    collapse (Newline [] :: rest)
  | Flow5 f1 :: Flow5 f2 :: rest ->
    collapse (Flow5 (f1 @ f2) :: rest)
  | (Flow5_without_interactive f1) :: (Flow5 f2) :: rest ->
    let f1 = (f1 :> Html_types.flow5 elt list) in
    collapse (Flow5 (f1 @ f2) :: rest)
  | (Flow5 f1) :: (Flow5_without_interactive f2) :: rest ->
    let f2 = (f2 :> Html_types.flow5 elt list) in
    collapse (Flow5 (f1 @ f2) :: rest)
  | (Flow5_without_interactive f1) :: (Flow5_without_interactive f2)
    :: rest ->
    collapse (Flow5_without_interactive (f1 @ f2) :: rest)
  | other :: rest ->
    other :: collapse rest

and format ~get_package : _ Documentation.text_element -> kind list = function
  | Raw      s ->
    [ Phrasing_without_interactive [ pcdata s ] ]
  | Code     s ->
    [ Phrasing_without_interactive [ code ~a:[ a_class ["code"] ] [ pcdata s ] ] ]
  | Verbatim v ->
    [ Flow5_without_interactive [ pre [ pcdata v ] ] ]
  | PreCode  p ->
    [ Flow5_without_interactive [ pre [ code ~a:[ a_class ["code"] ] [ pcdata p ] ] ] ]
  | Style (style, txt) -> apply_style ~get_package ~style txt
  | List subs  ->
    let subs = List.map subs ~f:(aggregate ~get_package) in
    if List.exists subs ~f:(List.exists ~f:is_interactive) then
      [ Flow5 [ul (
          List.map subs ~f:(fun agg ->
            li (List.concat @@ List.map agg ~f:to_flow5)
          )
        )]
      ]
    else
      [ Flow5_without_interactive [ul (
          List.map subs ~f:(fun agg ->
            li (List.concat @@ List.map agg ~f:to_flow5_without_interactive)
          )
        )]
      ]
  | Enum subs ->
    let subs = List.map subs ~f:(aggregate ~get_package) in
    if List.exists subs ~f:(List.exists ~f:is_interactive) then
      [ Flow5 [ol (
          List.map subs ~f:(fun agg ->
            li (List.concat @@ List.map agg ~f:to_flow5)
          )
        )]
      ]
    else
      [ Flow5_without_interactive [ol (
          List.map subs ~f:(fun agg ->
            li (List.concat @@ List.map agg ~f:to_flow5_without_interactive)
          )
        )]
      ]
  | Newline -> [ Newline [] ]
  | Title (lvl, label, txt) -> make_title ~get_package ~lvl ~label txt
  | Reference (r,text) ->
    begin match text with
    | None -> [ Phrasing (ref_to_link ~get_package r) ]
    | Some text ->
      List.map (aggregate ~get_package text) ~f:(function
        | Newline l -> Newline l
        | Flow5_without_interactive text ->
          Flow5 (ref_to_link ~get_package ~text r)
        | Phrasing_without_interactive text ->
          Phrasing (ref_to_link ~get_package ~text r)
        | _ ->
          (* TODO: better error handling *)
          failwith "It is disallowed to nest references."
      )
    end
  | Target (Some "html", str) ->
    [ Flow5 [Unsafe.data str] ]
  | Target (_, str) ->
    [ Flow5_without_interactive [ pre [pcdata str] ] ]
  | Special _ ->
    [ Phrasing_without_interactive [ pcdata "<TODO: report to odoc devs>" ] ]

and make_title ~get_package ~lvl ~label txt =
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
    match label with
    | None -> header_fun ~a:[]
    | Some (Paths.Identifier.Label (_, lbl)) -> header_fun ~a:[ a_id lbl ]
  in
  let txt = aggregate ~get_package txt in
  let result, should_error =
    (* Best effort, titleize the first part of the subtree if we can. *)
    match txt with
    | [] -> [ Flow5_without_interactive [header_fun []] ], false
    | Phrasing content :: tail ->
      Flow5 [header_fun content] :: tail, tail <> []
    | Phrasing_without_interactive content :: tail ->
      Flow5_without_interactive
        [header_fun (content :> Html_types.phrasing elt list)]
      :: tail, tail <> []
    | _ ->
      txt, false
  in
  if should_error then (
    Printf.eprintf
      "ERROR: only \"simple\" single line content allowed inside {%d }\n%!" lvl
  );
  result

and apply_style ~get_package ~style txt =
  let aggregated = aggregate ~get_package txt in
  let assert_phrasing for_message =
    if not (List.for_all aggregated ~f:is_phrasing) then
      Printf.eprintf "ERROR: only \"simple\" content allowed inside %s\n%!"
        for_message
  in
  match style with
  | Bold ->
    assert_phrasing "{b }";
    List.map aggregated ~f:(function
      | Phrasing p -> Phrasing [b p]
      | Phrasing_without_interactive p -> Phrasing_without_interactive [b p]
      | anything_else -> anything_else
    )
  | Italic ->
    assert_phrasing "{i }";
    List.map aggregated ~f:(function
      | Phrasing p -> Phrasing [i p]
      | Phrasing_without_interactive p -> Phrasing_without_interactive [i p]
      | anything_else -> anything_else
    )
  | Emphasize ->
    assert_phrasing "{e }";
    List.map aggregated ~f:(function
      | Phrasing p -> Phrasing [em p]
      | Phrasing_without_interactive p -> Phrasing_without_interactive [em p]
      | anything_else -> anything_else
    )
  | Center ->
    if List.for_all aggregated ~f:is_phrasing then
      let lst = List.concat @@ List.map aggregated ~f:to_phrasing in
      [ Flow5 [ p ~a:[ a_class ["center"] ] lst ] ]
    else
      let lst = List.concat @@ List.map aggregated ~f:to_flow5 in
      [ Flow5 [ div ~a:[ a_class ["center"] ] lst ] ]
  | Left ->
    if List.for_all aggregated ~f:is_phrasing then
      let lst = List.concat @@ List.map aggregated ~f:to_phrasing in
      [ Flow5 [ p ~a:[ a_class ["left"] ] lst ] ]
    else
      let lst = List.concat @@ List.map aggregated ~f:to_flow5 in
      [ Flow5 [ div ~a:[ a_class ["left"] ] lst ] ]
  | Right ->
    if List.for_all aggregated ~f:is_phrasing then
      let lst = List.concat @@ List.map aggregated ~f:to_phrasing in
      [ Flow5 [ p ~a:[ a_class ["right"] ] lst ] ]
    else
      let lst = List.concat @@ List.map aggregated ~f:to_flow5 in
      [ Flow5 [ div ~a:[ a_class ["right"] ] lst ] ]
  | Superscript ->
    assert_phrasing "{^ }";
    List.map aggregated ~f:(function
      | Phrasing p -> Phrasing [sup p]
      | Phrasing_without_interactive p -> Phrasing_without_interactive [sup p]
      | anything_else -> anything_else
    )
  | Subscript ->
    assert_phrasing "{_ }";
    List.map aggregated ~f:(function
      | Phrasing p -> Phrasing [sup p]
      | Phrasing_without_interactive p -> Phrasing_without_interactive [sub p]
      | anything_else -> anything_else
    )
  | Custom str  ->
    List.map aggregated ~f:(function
      | Newline l -> Newline l
      | Phrasing l -> Phrasing [span ~a:[ a_class [str] ] l]
      | Phrasing_without_interactive l ->
        Phrasing_without_interactive [span ~a:[ a_class [str] ] l]
      | Flow5 l -> Flow5 [div ~a:[ a_class [str] ] l]
      | Flow5_without_interactive l ->
        Flow5_without_interactive [div ~a:[ a_class [str] ] l]
    )

let handle_text ~get_package txt =
  List.concat @@ List.map (aggregate ~get_package txt) ~f:(function
    | Phrasing l -> [p l]
    | Phrasing_without_interactive l -> [p l]
    | Flow5 l -> l
    | Flow5_without_interactive l -> (l :> Html_types.flow5 elt list)
    | Newline _ -> []
  )

let rec list_keep_while ~pred = function
  | x :: xs when pred x -> x :: list_keep_while ~pred xs
  | _ -> []

let prerr_error (err : _ Documentation.Error.t) =
  let print_pos oc { Documentation.Error.Position. line; column } =
    Printf.fprintf oc "line %d, col %d" line column
  in
  let print_loc =
    match err.location with
    | None ->
      begin fun oc () ->
        Printf.fprintf oc "%s, offset: %a to %a" (* Good luck with that. *)
          (Paths.Identifier.name err.origin)
          print_pos err.offset.Documentation.Error.Offset.start
          print_pos err.offset.Documentation.Error.Offset.finish
      end
    | Some { Documentation.Error.Location. filename; start; finish } ->
      begin fun oc () ->
        Printf.fprintf oc "%s, %a to %a" filename print_pos start
          print_pos finish
      end
  in
  Printf.eprintf "Error %a: %s\n%!" print_loc () err.message

let first_to_html ~get_package (t : _ Documentation.t) =
  match t with
  | Ok { text; _ } ->
    let pred = function
      | Documentation.Newline -> false
      | _ -> true
    in
    handle_text ~get_package (list_keep_while ~pred text)
  | Error e -> prerr_error e; []

let to_html ~get_package (t : _ Documentation.t) =
  match t with
  | Error e -> prerr_error e; []
  | Ok body -> handle_text ~get_package body.text

let has_doc (t : _ Types.Documentation.t) =
  match t with
  | Ok body -> body.text <> []
  | Error e -> prerr_error e; false
