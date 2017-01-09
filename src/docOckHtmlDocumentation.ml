open DocOck
open Types
open Tyxml.Html

module Html_tree = DocOckHtmlHtml_tree
module Markup = DocOckHtmlMarkup

let html_dot_magic = List.map ~f:(fun x -> tot @@ toelt x)

let rec list_keep_while ~pred = function
  | x :: xs when pred x -> x :: list_keep_while ~pred xs
  | _ -> []

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

let rec collapse = function
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

module Reference = struct
  module Id = Html_tree.Relative_link.Id

  open DocOck.Paths

  let rec render_resolved : type a. (_, a) Reference.Resolved.t -> string =
    fun r ->
      let open Reference.Resolved in
      match r with
      | Identifier id -> Identifier.name id
      | Module (r, s) -> render_resolved r ^ "." ^ s
      | ModuleType (r, s) -> render_resolved r ^ "." ^ s
      | Type (r, s) -> render_resolved r ^ "." ^ s
      | Constructor (r, s) -> render_resolved r ^ "." ^ s
      | Field (r, s) -> render_resolved r ^ "." ^ s
      | Extension (r, s) -> render_resolved r ^ "." ^ s
      | Exception (r, s) -> render_resolved r ^ "." ^ s
      | Value (r, s) -> render_resolved r ^ "." ^ s
      | Class (r, s) -> render_resolved r ^ "." ^ s
      | ClassType (r, s) -> render_resolved r ^ "." ^ s
      | Method (r, s) ->
        (* CR trefis: do we really want to print anything more than [s] here?  *)
        render_resolved r ^ "." ^ s
      | InstanceVariable (r, s) ->
        (* CR trefis: the following makes no sense to me... *)
        render_resolved r ^ "." ^ s
      | Label (r, s) -> render_resolved r ^ ":" ^ s

  let rec ref_to_string : type a. (_, a) Reference.t -> string = function
    | Reference.Root s -> s
    | Reference.Dot (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Resolved r -> render_resolved r

  let rec to_html : type a. get_package:('b -> string) -> ?text:kind -> stop_before:bool
    -> (_, a) Reference.t -> kind =
    fun ~get_package ?text ~stop_before ref ->
      let span txt =
        let span x =
          span x ~a:[ a_class ["xref-unresolved"]
                    ; a_title (Printf.sprintf "unresolved %S"
                                 (ref_to_string ref))
                    ]
        in
        match txt with
        | Phrasing l -> Phrasing [span l]
        | Phrasing_without_interactive l ->
          Phrasing_without_interactive [span l]
        | otherwise ->
          (* [span] only accepts phrasing content. *)
          Printf.eprintf
            "Unresolved reference %S could not be spaned as the text is too \
             rich\n%!" (ref_to_string ref);
          otherwise
      in
      let open Reference in
      match ref with
      | Root s ->
        begin match text with
        | None -> Phrasing_without_interactive [ pcdata s ]
        | Some s -> span s
        end
      | Dot (parent, s) ->
        begin match text with
        | Some s -> span s
        | None ->
          let tail = [ pcdata ("." ^ s) ] in
          match to_html ~get_package ~stop_before:true parent with
          | Phrasing content -> Phrasing (content @ tail)
          | Phrasing_without_interactive content ->
            Phrasing_without_interactive (content @ tail)
          | Flow5 content -> Flow5 (content @ tail)
          | Flow5_without_interactive content ->
            Flow5_without_interactive (content @ tail)
          | Newline _ -> Phrasing_without_interactive tail
        end
      | Resolved r ->
        let id = Reference.Resolved.identifier r in
        let txt =
          match text with
          | None -> Phrasing_without_interactive [ pcdata (render_resolved r) ]
          | Some s -> s
        in
        begin match Id.href ~get_package ~stop_before id with
        | exception Id.Not_linkable -> txt
        | exception exn ->
          (* FIXME: better error message *)
          Printf.eprintf "Id.href failed: %S\n%!" (Printexc.to_string exn);
          txt
        | href ->
          match txt with
          | Phrasing_without_interactive content ->
            Phrasing [ a ~a:[ a_href href ] content ]
          | Flow5_without_interactive content ->
            Flow5 [ a ~a:[ a_href href ] content ]
          | Newline _ -> Newline []
          | _ ->
            Printf.eprintf "It is disallowed to nest references.\n%!";
            txt
        end

  let link ~get_package ?text (ref : _ Documentation.reference) =
    (* It is wonderful that although each these [r] is a [Reference.t] the phantom
       type parameters are not the same so we can't merge the branches. *)
    match ref with
    | Module r           -> to_html ~get_package ~stop_before:false ?text r
    | ModuleType r       -> to_html ~get_package ~stop_before:false ?text r
    | Type r             -> to_html ~get_package ~stop_before:false ?text r
    | Constructor r      -> to_html ~get_package ~stop_before:false ?text r
    | Field r            -> to_html ~get_package ~stop_before:false ?text r
    | Extension r        -> to_html ~get_package ~stop_before:false ?text r
    | Exception r        -> to_html ~get_package ~stop_before:false ?text r
    | Value r            -> to_html ~get_package ~stop_before:false ?text r
    | Class r            -> to_html ~get_package ~stop_before:false ?text r
    | ClassType r        -> to_html ~get_package ~stop_before:false ?text r
    | Method r           -> to_html ~get_package ~stop_before:false ?text r
    | InstanceVariable r -> to_html ~get_package ~stop_before:false ?text r
    | Element r          -> to_html ~get_package ~stop_before:false ?text r
    | Section r          -> to_html ~get_package ~stop_before:false ?text r
    | Link s ->
      let text =
        match text with
        | Some l -> l
        | None -> Phrasing_without_interactive [ pcdata s ]
      in
      begin match text with
      | Phrasing_without_interactive content ->
        Phrasing [ a ~a:[ a_href s ] content ]
      | Flow5_without_interactive content ->
        Flow5 [ a ~a:[ a_href s ] content ]
      | Newline _ -> Newline []
      | _ ->
        Printf.eprintf "It is disallowed to nest references.\n%!";
        text
      end
    | Custom (_,_)
      -> Phrasing_without_interactive [ pcdata "[documentation.handle_ref TODO]" ]
end

let paragraphise lst =
  List.concat @@ List.map lst ~f:(function
    | Phrasing l -> [p l]
    | Phrasing_without_interactive l -> [p l]
    | Flow5 l -> l
    | Flow5_without_interactive l -> (l :> Html_types.flow5 elt list)
    | Newline _ -> []
  )

let paragraphise_without_interactive lst =
  List.concat @@ List.map lst ~f:(function
    | Phrasing_without_interactive l -> [p l]
    | Flow5_without_interactive l -> (l :> Html_types.flow5 elt list)
    | Newline _ -> []
    | Phrasing l -> invalid_arg "paragraphise_without_interactive"
    | Flow5 l -> invalid_arg "paragraphise_without_interactive"
  )

let make_li_without_interactive = function
  | [ x ] -> li (to_flow5_without_interactive x)
  | lst -> li (paragraphise_without_interactive lst)

let make_li = function
  | [ x ] -> li (to_flow5 x)
  | lst -> li (paragraphise lst)

let whitespace_only s =
  let rec aux i =
    if i < 0 then
      true
    else
      match String.get s i with
      | '\n' | ' ' | '\t' -> aux (i - 1)
      | _ -> false
  in
  aux (String.length s - 1)

let rec aggregate ~get_package lst =
  collapse (List.concat @@ List.map lst ~f:(format ~get_package))

and format ~get_package : _ Documentation.text_element -> kind list = function
  | Raw      s ->
    if whitespace_only s then []
    else [ Phrasing_without_interactive [ pcdata s ] ]
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
      [ Flow5 [ul (List.map subs ~f:make_li)] ]
    else
      [ Flow5_without_interactive
          [ul (List.map subs ~f:make_li_without_interactive)] ]
  | Enum subs ->
    let subs = List.map subs ~f:(aggregate ~get_package) in
    if List.exists subs ~f:(List.exists ~f:is_interactive) then
      [ Flow5 [ol (List.map subs ~f:make_li)] ]
    else
      [ Flow5_without_interactive
          [ol (List.map subs ~f:make_li_without_interactive)] ]
  | Newline -> [ Newline [] ]
  | Title (lvl, label, txt) -> make_title ~get_package ~lvl ~label txt
  | Reference (r,text) ->
    begin match text with
    | None -> [ Reference.link ~get_package r ]
    | Some text ->
      List.map (aggregate ~get_package text)
        ~f:(fun text -> Reference.link ~get_package ~text r)
    end
  | Target (Some "html", str) ->
    [ Flow5 [Unsafe.data str] ]
  | Target (_, str) ->
    [ Flow5_without_interactive [ pre [pcdata str] ] ]
  | Special (Documentation.Modules refs) ->
    let table =
      table ~a:[ a_class ["modules"] ]
        (List.map refs ~f:(module_index_entry ~get_package))
    in
    [ Flow5 [ table ] ]
  | Special (Documentation.Index) ->
    Printf.eprintf "Warning: {!indexlist} is not yet supported by odoc.\n%!";
    []

and module_index_entry ~get_package (reference, preamble) =
  let link = Reference.to_html ~get_package ~stop_before:false reference in
  let doc =
    match aggregate ~get_package preamble with
    | [] -> []
    | p :: _ -> to_flow5 p
  in
  let id, kind =
    let open DocOckHtmlUrl.Module_listing_anchor in
    match from_reference reference with
    | {kind; name} -> Printf.sprintf "listing-%s-%s" kind name, kind
    | exception (Failure s) ->
      Printf.eprintf "ERROR: %s\n%!" s;
      "", ""
  in
  tr ~a:[ a_id id; a_class ["anchored"] ]
    [ td ~a:[ a_class [kind] ] (
        a ~a:[ a_href ("#" ^ id); a_class ["anchor"] ] [] ::
        to_flow5 link
      )
    ; td ~a:[ a_class ["doc"] ] doc
    ]

and make_title ~get_package ~lvl ~label txt =
  let header_fun, attrs =
    match lvl with
    | 1 -> h2, []
    | 2 -> h3, []
    | 3 -> h4, []
    | 4 -> h5, []
    | 5 -> h6, []
    | n ->
      (fun ?a (x : Html_types.phrasing elt list) ->
         div ?a (x :> Html_types.flow5 elt list)),
      [ a_class [Printf.sprintf "h%d" (n + 1)] ]
  in
  let header_fun =
    match label with
    | None -> header_fun ~a:attrs
    | Some (Paths.Identifier.Label (_, lbl)) ->
        fun txt ->
          header_fun ~a:(a_id lbl :: a_class ["anchored"] :: attrs)
            ((a ~a:[ a_href ("#" ^ lbl); a_class ["anchor"] ] []) :: txt)
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
  paragraphise (aggregate ~get_package txt)

let handle_tags ~get_package tags =
  let raw =
    let make_tag ?class_ txt =
      let class_ =
        match class_ with
        | None -> String.uncapitalize txt
        | Some s -> s
      in
      span ~a:[ a_class [ "at-tag"; class_ ] ] [pcdata txt]
    in
    List.map tags ~f:(
      let open Documentation in
      (* TODO: better everything. *)
      function
      | Author  s ->
        [ Phrasing [ make_tag "Author" ; pcdata ": "; pcdata s ] ]
      | Version s ->
        [ Phrasing [ make_tag "Version" ; pcdata ": "; pcdata s ] ]
      | See (see, txt) ->
        let prefix = [ make_tag "See"; pcdata " " ] in
        let see =
          match see with
          | Url s -> a ~a:[ a_href s ] [ pcdata s ]
          | File s -> pcdata s
          | Doc s -> pcdata s
        in
        let aggregated = aggregate ~get_package txt in
        collapse (Phrasing (prefix @ [see]) :: aggregated)
      | Since s ->
        [ Phrasing [ make_tag "Since" ; pcdata ": "; pcdata s ] ]
      | Before (s, txt) ->
        let prefix = [ make_tag "Before" ; pcdata " "; pcdata s; pcdata "." ] in
        collapse (Phrasing prefix :: aggregate ~get_package txt)
      | Deprecated txt ->
        let prefix = [ make_tag "Deprecated" ; pcdata " " ] in
        collapse (Phrasing prefix :: aggregate ~get_package txt)
      | Param (name, txt) ->
        let p =
          [ make_tag "Parameter"
          ; pcdata " "
          ; Markup.module_path [name] (* Meh. *)
          ; pcdata ": "
          ]
        in
        collapse (Phrasing p :: aggregate ~get_package txt)
      | Raise (name, txt) ->
        let p =
          [ make_tag ~class_:"raise" "Raises"
          ; pcdata " "
          ; Markup.module_path [name] (* Meh. *)
          ; pcdata ": "
          ]
        in
        collapse (Phrasing p :: aggregate ~get_package txt)
      | Return txt ->
        let prefix = [ make_tag ~class_:"return" "Returns" ; pcdata " " ] in
        collapse (Phrasing prefix :: aggregate ~get_package txt)
      | Tag (s, txt) ->
        let prefix = [ make_tag ~class_:s ("@" ^ s) ; pcdata " " ] in
        collapse (Phrasing prefix :: aggregate ~get_package txt)
      | Inline -> []
    )
  in
  let cleaned = List.filter (function [] -> false | _ -> true) raw in
  match cleaned with
  | [] -> []
  | lst -> [ ul ~a:[ a_class ["at-tag"] ] (List.map lst ~f:make_li) ]

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
    begin match handle_text ~get_package text with
    | [] -> []
    | x :: _ -> [x]
    end
  | Error e -> prerr_error e; []

let to_html ?wrap ~get_package (t : _ Documentation.t) =
  match t with
  | Error e -> prerr_error e; []
  | Ok body ->
    match wrap with
    | None ->
      let doc = handle_text ~get_package body.text in
      let tags = handle_tags ~get_package body.tags in
      doc @ tags
    | Some () ->
      let open Documentation in
      let open_ = "(** " in
      let close = " *)" in
      match body.text, body.tags with
      | [], [] -> []
      | _, [] -> handle_text ~get_package (Raw open_ :: body.text @ [Raw close])
      | [], _ -> p [pcdata open_] :: handle_tags ~get_package body.tags @ [p [pcdata close]]
      | _, _ ->
        handle_text ~get_package (Raw open_ :: body.text) @
        handle_tags ~get_package body.tags @
        [p [pcdata close]]

let has_doc (t : _ Types.Documentation.t) =
  match t with
  | Ok body -> body.text <> [] || body.tags <> []
  | Error e -> prerr_error e; false
