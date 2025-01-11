open Odoc_parser

type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

module Location_to_sexp = struct
  let point : Loc.point -> sexp =
   fun { line; column } ->
    List [ Atom (string_of_int line); Atom (string_of_int column) ]

  let span : Loc.span -> sexp =
   fun { file; start; end_ } -> List [ Atom file; point start; point end_ ]

  let at : ('a -> sexp) -> 'a Loc.with_location -> sexp =
   fun f { location; value } -> List [ span location; f value ]
end

module Ast_to_sexp = struct
  (* let at = Location_to_sexp.at *)
  type at = { at : 'a. ('a -> sexp) -> 'a Loc.with_location -> sexp }

  let loc_at = { at = Location_to_sexp.at }
  let str s = Atom s
  let opt f s = match s with Some s -> List [ f s ] | None -> List []

  let style : Ast.style -> sexp = function
    | `Bold -> Atom "bold"
    | `Italic -> Atom "italic"
    | `Emphasis -> Atom "emphasis"
    | `Superscript -> Atom "superscript"
    | `Subscript -> Atom "subscript"

  let alignment : Ast.alignment option -> sexp = function
    | Some `Left -> Atom "left"
    | Some `Center -> Atom "center"
    | Some `Right -> Atom "right"
    | None -> Atom "default"

  let reference_kind : Ast.reference_kind -> sexp = function
    | `Simple -> Atom "simple"
    | `With_text -> Atom "with_text"

  let rec inline_element at : Ast.inline_element -> sexp = function
    | `Space _ -> Atom "space"
    | `Word w -> List [ Atom "word"; Atom w ]
    | `Code_span c -> List [ Atom "code_span"; Atom c ]
    | `Raw_markup (target, s) ->
        List [ Atom "raw_markup"; opt str target; Atom s ]
    | `Math_span s -> List [ Atom "math_span"; Atom s ]
    | `Styled (s, es) ->
        List [ style s; List (List.map (at.at (inline_element at)) es) ]
    | `Reference (kind, r, es) ->
        List
          [
            reference_kind kind;
            at.at str r;
            List (List.map (at.at (inline_element at)) es);
          ]
    | `Link (u, es) ->
        List [ str u; List (List.map (at.at (inline_element at)) es) ]

  let code_block_lang at { Ast.language; tags } =
    List [ at.at str language; opt (at.at str) tags ]

  let media : Ast.media -> sexp = function
    | `Image -> Atom "image"
    | `Video -> Atom "video"
    | `Audio -> Atom "audio"

  let media_href = function
    | `Reference href -> List [ Atom "Reference"; Atom href ]
    | `Link href -> List [ Atom "Link"; Atom href ]

  let rec nestable_block_element at : Ast.nestable_block_element -> sexp =
    function
    | `Paragraph es ->
        List
          [ Atom "paragraph"; List (List.map (at.at (inline_element at)) es) ]
    | `Math_block s -> List [ Atom "math_block"; Atom s ]
    | `Code_block { Ast.meta = None; content; output = None; _ } ->
        List [ Atom "code_block"; at.at str content ]
    | `Code_block { meta = Some meta; content; output = None; _ } ->
        List [ Atom "code_block"; code_block_lang at meta; at.at str content ]
    | `Code_block { meta = Some meta; content; output = Some output; _ } ->
        List
          [
            Atom "code_block";
            code_block_lang at meta;
            at.at str content;
            List
              (List.map (fun v -> nestable_block_element at v.Loc.value) output);
          ]
    | `Code_block { meta = None; content = _; output = Some _output; _ } ->
        List [ Atom "code_block_err" ]
    | `Verbatim t -> List [ Atom "verbatim"; Atom t ]
    | `Modules ps -> List [ Atom "modules"; List (List.map (at.at str) ps) ]
    | `List (kind, weight, items) ->
        let kind =
          match kind with `Unordered -> "unordered" | `Ordered -> "ordered"
        in
        let weight =
          match weight with `Light -> "light" | `Heavy -> "heavy"
        in
        let items =
          items
          |> List.map (fun item ->
                 List (List.map (at.at (nestable_block_element at)) item))
          |> fun items -> List items
        in
        List [ Atom kind; Atom weight; items ]
    | `Table ((grid, align), s) ->
        let syntax = function `Light -> "light" | `Heavy -> "heavy" in
        let kind = function `Header -> "header" | `Data -> "data" in
        let map name x f = List [ Atom name; List (List.map f x) ] in
        let alignment =
          match align with
          | None -> List [ Atom "align"; Atom "no alignment" ]
          | Some align -> map "align" align @@ alignment
        in
        List
          [
            Atom "table";
            List [ Atom "syntax"; Atom (syntax s) ];
            ( map "grid" grid @@ fun row ->
              map "row" row @@ fun (cell, k) ->
              map (kind k) cell @@ at.at (nestable_block_element at) );
            alignment;
          ]
    | `Media (kind, href, c, m) ->
        List [ reference_kind kind; at.at media_href href; Atom c; media m ]

  let tag at : Ast.tag -> sexp = function
    | `Author s -> List [ Atom "@author"; Atom s ]
    | `Deprecated es ->
        List
          (Atom "@deprecated" :: List.map (at.at (nestable_block_element at)) es)
    | `Param (s, es) ->
        List
          ([ Atom "@param"; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Raise (s, es) ->
        List
          ([ Atom "@raise"; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Return es ->
        List (Atom "@return" :: List.map (at.at (nestable_block_element at)) es)
    | `See (kind, s, es) ->
        let kind =
          match kind with
          | `Url -> "url"
          | `File -> "file"
          | `Document -> "document"
        in
        List
          ([ Atom "@see"; Atom kind; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Since s -> List [ Atom "@since"; Atom s ]
    | `Before (s, es) ->
        List
          ([ Atom "@before"; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Version s -> List [ Atom "@version"; Atom s ]
    | `Canonical p -> List [ Atom "@canonical"; at.at str p ]
    | `Inline -> Atom "@inline"
    | `Open -> Atom "@open"
    | `Closed -> Atom "@closed"
    | `Hidden -> Atom "@hidden"
    | _ -> failwith "TODO"

  let block_element at : Ast.block_element -> sexp = function
    | #Ast.nestable_block_element as e -> nestable_block_element at e
    | `Heading (level, label, es) ->
        let label = List [ Atom "label"; opt str label ] in
        let level = string_of_int level in
        List
          [ Atom level; label; List (List.map (at.at (inline_element at)) es) ]
    | `Tag t -> tag at t

  let docs at : Ast.t -> sexp =
   fun f -> List (List.map (at.at (block_element at)) f)
end
