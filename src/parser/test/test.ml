open Odoc_parser

type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

module Location_to_sexp = struct
  let point : Loc.point -> sexp =
   fun { line; column } ->
    List [ Atom (string_of_int line); Atom (string_of_int column) ]

  let span : Loc.span -> sexp =
   fun { file; start; end_ } -> List [ Atom file; point start; point end_ ]

  let at : ('a Loc.with_location -> sexp) -> 'a Loc.with_location -> sexp =
   fun f ({ location; _ } as v) -> List [ span location; f v ]
end

let error err = Atom (Odoc_parser.Warning.to_string err)

module Ast_to_sexp = struct
  (* let at = Location_to_sexp.at *)
  type at = {
    at : 'a. ('a Loc.with_location -> sexp) -> 'a Loc.with_location -> sexp;
  }

  let loc_at = { at = Location_to_sexp.at }
  let str s = Atom s
  let str_at s = Atom s.Loc.value
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

  let media : Ast.media -> sexp = function
    | `Image -> Atom "image"
    | `Video -> Atom "video"
    | `Audio -> Atom "audio"

  let rec inline_element at : Ast.inline_element Loc.with_location -> sexp =
   fun v ->
    match v.value with
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
            at.at str_at r;
            List (List.map (at.at (inline_element at)) es);
          ]
    | `Link (u, es) ->
        List [ str u; List (List.map (at.at (inline_element at)) es) ]

  let code_block_tags at tags =
    let code_block_tag t =
      match t with
      | `Tag s -> List [ Atom "tag"; at.at str_at s ]
      | `Binding (key, value) ->
          List [ Atom "binding"; at.at str_at key; at.at str_at value ]
    in
    List (List.map code_block_tag tags)

  let code_block_lang at { Ast.language; tags } =
    List [ at.at str_at language; code_block_tags at tags ]

  let media_href =
   fun v ->
    match v.Loc.value with
    | `Reference href -> List [ Atom "Reference"; Atom href ]
    | `Link href -> List [ Atom "Link"; Atom href ]

  let rec nestable_block_element at :
      Ast.nestable_block_element Loc.with_location -> sexp =
   fun v ->
    match v.value with
    | `Paragraph es ->
        List
          [ Atom "paragraph"; List (List.map (at.at (inline_element at)) es) ]
    | `Math_block s -> List [ Atom "math_block"; Atom s ]
    | `Code_block { Ast.meta = None; content; output = None; _ } ->
        let trimmed_content, warnings =
          Odoc_parser.codeblock_content v.location content.value
        in
        let warnings =
          if warnings = [] then []
          else [ List (Atom "Warnings" :: List.map error warnings) ]
        in
        let content = Loc.at content.location trimmed_content in
        List ([ Atom "code_block"; at.at str_at content ] @ warnings)
    | `Code_block { meta = Some meta; content; output = None; _ } ->
        let trimmed_content, warnings =
          Odoc_parser.codeblock_content v.location content.value
        in
        let warnings =
          if warnings = [] then []
          else [ List (Atom "Warnings" :: List.map error warnings) ]
        in
        let content = Loc.at content.location trimmed_content in
        List
          ([ Atom "code_block"; code_block_lang at meta; at.at str_at content ]
          @ warnings)
    | `Code_block
        {
          meta = Some meta;
          content;
          output = Some output (* ; outer_location *);
          _;
        } ->
        let trimmed_content, warnings =
          Odoc_parser.codeblock_content v.location content.value
        in
        let warnings =
          if warnings = [] then []
          else [ List (Atom "Warnings" :: List.map error warnings) ]
        in
        let content = Loc.at content.location trimmed_content in
        List
          ([
             Atom "code_block";
             code_block_lang at meta;
             at.at str_at content;
             List (List.map (nestable_block_element at) output);
           ]
          @ warnings)
    | `Code_block { meta = None; content = _; output = Some _output; _ } ->
        List [ Atom "code_block_err" ]
    | `Verbatim t ->
        let trimmed_content, warnings =
          Odoc_parser.verbatim_content v.location t
        in
        let warnings =
          if warnings = [] then []
          else [ List (Atom "Warnings" :: List.map error warnings) ]
        in
        List ([ Atom "verbatim"; Atom trimmed_content ] @ warnings)
    | `Modules ps -> List [ Atom "modules"; List (List.map (at.at str_at) ps) ]
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
    | `Children_order es ->
        List
          (Atom "@children_order"
          :: List.map (at.at (nestable_block_element at)) es)
    | `Toc_status es ->
        List
          (Atom "@toc_status" :: List.map (at.at (nestable_block_element at)) es)
    | `Order_category es ->
        List
          (Atom "@order_category"
          :: List.map (at.at (nestable_block_element at)) es)
    | `Short_title es ->
        List
          (Atom "@short_title"
          :: List.map (at.at (nestable_block_element at)) es)
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
    | `Canonical p -> List [ Atom "@canonical"; at.at str_at p ]
    | `Inline -> Atom "@inline"
    | `Open -> Atom "@open"
    | `Closed -> Atom "@closed"
    | `Hidden -> Atom "@hidden"

  let block_element at : Ast.block_element Loc.with_location -> sexp = function
    | { value = #Ast.nestable_block_element; _ } as e ->
        nestable_block_element at e
    | { value = `Heading (level, label, es); _ } ->
        let label = List [ Atom "label"; opt str label ] in
        let level = string_of_int level in
        List
          [ Atom level; label; List (List.map (at.at (inline_element at)) es) ]
    | { value = `Tag t; _ } -> tag at t

  let docs at : Ast.t -> sexp =
   fun f -> List (List.map (at.at (block_element at)) f)
end

let parser_output formatter v =
  let ast, warnings = Odoc_parser.(ast v, warnings v) in
  let value = Ast_to_sexp.(docs loc_at ast) in
  let warnings = List (List.map error warnings) in
  let output =
    List [ List [ Atom "output"; value ]; List [ Atom "warnings"; warnings ] ]
  in
  Sexplib0.Sexp.pp_hum formatter output;
  Format.pp_print_flush formatter ()

let test ?(location = { Loc.line = 1; column = 0 }) str =
  let location =
    Expect_test_helper.lexing_position ~line:location.line
      ~column:location.column ()
  in
  let ast = Odoc_parser.parse_comment ~location ~text:str in
  Expect_test_helper.expect ~input:str (fun f -> parser_output f ast)

[@@@ocaml.warning "-32"]

let trivial () =
  let module Trivial = struct
    let empty = test ""

    let space = test " "

    let two_spaces = test "  "

    let tab = test "\t"

    let mixed_space = test " \t \t"

    let newline = test "\n"

    let blank_line = test "\n\n"

    let cf_lf = test "\r\n"
  end in
  ()

let one_paragraph () =
  let module One_paragraph = struct
    let word = test "foo"

    let two_words = test "foo bar"

    let two_words = test "foo bar"

    let two_spaces = test "foo  bar"

    let mixed_space = test "foo \t \t bar"

    let two_lines = test "foo\n"

    let two_lines_cr_lf = test "foo\r\nbar"

    let leading_space = test " foo"

    let trailing_space = test "foo "

    let leading_space_on_line = test "foo\n bar"

    let trailing_space_on_line = test "foo \nbar"

    let leading_tab_on_line = test "foo\n\tbar"

    let trailing_tab_on_line = test "foo\t\nbar"

    let email = test "foo@bar.com"
  end in
  ()

let two_paragraphs () =
  let module Two_paragraphs = struct
    let basic = test "foo\n\nbar"

    let leading_space = test "foo \n\nbar"

    let trailing_space = test "foo\n\n bar"

    let cr_lf = test "foo\r\n\r\nbar"

    let mixed_cr_lf = test "foo\n\r\nbar"
  end in
  ()

let plus_minus_bar_words () =
  let module Plus_minus_bar_words = struct
    let minus_in_word = test "foo-bar"

    let minus_as_word = test "foo -"

    let plus_in_word = test "foo+bar"

    let plus_as_word = test "foo +"

    let bar_in_word = test "foo|bar"

    let escaped_bar_in_word = test "foo\\|bar"

    let bar_as_word = test "foo |"

    let negative_number = test "-3.14 -1337"

    let n_em_dash = test "-- ---"

    let minus_at = test "-@"

    let at_minus = test "-@-"

    let option = test "--option"
  end in
  ()

let escape_sequence () =
  let module Escape_sequence = struct
    let left_brace = test "\\{"

    let left_brace_in_word = test "foo\\{bar"

    let right_brace = test "\\}"

    let right_brace_in_word = test "foo\\{bar"

    let left_bracket = test "\\["

    let left_bracket_in_word = test "foo\\[bar"

    let right_bracket = test "\\]"

    let right_bracket_in_word = test "foo\\]bar"

    let at = test "@"

    let not_a_tag = test "\\@author"

    let at_in_word = test "foo\\@bar"

    let trailing_backslash = test "foo\\"

    let none_escape = test "foo\\bar"

    let backslash_not_escaped = test "foo\\\\{bar"

    let single_backslash = test "\\"

    let escape_minus = test "\\{- foo"

    let escape_plus = test "\\{+ foo"

    let minus_escape = test "-\\{"

    let plus_escape = test "+\\{"

    let escape_at = test "\\{@author"

    let two = test "\\{\\}"
  end in
  ()

let code_span () =
  let module Code_span = struct
    let basic = test "[foo]"

    let empty = test "[]"

    let list = test "[[]]"
    (* TODO The next two error messages are particularly unintuitive. *)

    let unbalanced_list = test "[[]"

    let no_markup = test "[{b"

    let few_escapes = test "[\\{]"

    let escaped_right_bracket = test "[\\]]"

    let escaped_left_bracket = test "[\\[]"

    let whitespace_preserved = test "[ foo bar ]"

    let no_new_lines = test "[foo\nbar]"

    let cr_lf_preserved = test "[foo\r\nbar]"

    let no_double_new_line = test "[foo\r\n\r\nbar]"

    let no_double_crlf = test "[foo\r\n\r\nbar]"

    let not_merged = test "[foo][bar]"

    let explicit_space = test "[foo] [bar]"

    let untermindated = test "[foo"
  end in
  ()

let bold () =
  let module Bold = struct
    let basic = test "{b foo}"

    let extra_leading_whitespace = test "{b  \t foo}"

    let leading_newline = test "{b\nfoo}"

    let leading_cr_lf = test "{b\r\nfoo}"

    let leading_newline_and_whitespace = test "{b\n foo}"

    let no_leading_whitespace = test "{bfoo}"

    let trailing_whitespace = test "{b foo }"

    let trailing_newline = test "{b foo\n}"

    let trailing_cr_lf = test "{b foo\r\n}"

    let two_words = test "{b foo bar}"

    let not_merged = test "{b foo}{b bar}"

    let nested = test "{b foo{b bar}}"

    let newline = test "{b foo\nbar}"

    let cr_lf = test "{b foo\r\nbar}"

    let minus = test "{b -}"

    let minus_list_item = test "{b foo\n - bar}"

    let plus_list_item = test "{b foo\n + bar}"

    let immediate_minus_list_item = test "{b\n- foo}"

    let immediate_plus_list_item = test "{b\n+ foo}"

    let blank_line = test "{b foo\n\nbar}"

    let immediate_blank_line = test "{b"

    let end_of_comment = test "{b foo"

    let nested_code_block = test "{b {[foo]}"

    let degenerate = test "{b}"

    let empty = test "{b }"
  end in
  ()

let italic () =
  let module Italic = struct
    let basic = test "{i foo}"

    let extra_leading_whitespace = test "{i  \t foo}"

    let leading_newline = test "{i\nfoo}"

    let leading_newline_and_whitespace = test "{i\n foo}"
  end in
  ()

let emphasis () =
  let module Emphasis = struct
    let basic = test "{e foo}"

    let extra_leading_whitespace = test "{e  \t foo}"

    let leading_newline = test "{e\nfoo}"

    let leading_newline_and_whitespace = test "{e\n foo}"
  end in
  ()

let superscript () =
  let module Superscript = struct
    let basic = test "{^ foo}"

    let extra_leading_whitespace = test "{^  \t foo}"

    let leading_newline = test "{^\nfoo}"

    let leading_cr_lf = test "{^\r\nfoo}"

    let leading_newline_and_whitespace = test "{^\n foo}"

    let no_whitespace = test "{^foo}"

    let degenerate = test "{^}"

    let empty = test "{^ }"
  end in
  ()

let subscript () =
  let module Subscript = struct
    let basic = test "{_ foo}"

    let extra_leading_whitespace = test "{_  \t foo}"

    let leading_newline = test "{_\nfoo}"

    let leading_newline_and_whitespace = test "{_\n foo}"

    let no_whitespace = test "{_foo}"

    let v_verbose = test "{_uv}"
  end in
  ()

let simple_reference () =
  let module Simple_reference = struct
    let basic = test "{!foo}"

    let leading_whitespace = test "{! foo}"

    let trailing_whitespace = test "{!foo }"

    let adjacent_word_leading = test "bar{!foo}"

    let explicit_leading_space = test "bar {!foo}"

    let adjacent_word_trailing = test "{!foo}bar"

    let explicit_trailing_space = test "{!foo} bar"

    let kind = test "{!val:foo}"

    let empty = test "{!}"

    let whitespace_only = test "{! }"

    let internal_whitespace = test "{!( * )}"

    (* TODO Limiting the character combinations allowed will make it easier to
       catch expressions accidentally written inside references. This can also
       be caught by a good resolver and resolver error messages. *)
    (* t "expression" *)
    let unterminated = test "{!foo"

    let empty_kind = test "{!:foo}"

    let whitespace_kind = test "{! :foo}"

    let with_kind_but_empty = test "{!val:}"

    let with_kind_but_whitespace = test "{!val: }"

    let leading_whitespace_in_kind = test "{! val:foo}"

    let internal_whitespace_in_kind = test "{!va l:foo}"

    let internal_whitespace_in_referent = test "{!val:( * )}"

    let two_colons = test "{!val:foo:bar}"

    let space_before_colon = test "{!val :foo}"

    let space_after_colon = test "{!val: foo}"

    let unterminated_after_kind = test "{!val:foo"

    let operator = test "{!(>>=)}"

    let operator_with_dash = test "{!(@->)}"

    let operator_with_dot = test "{!(*.)}"

    let operator_with_colon = test "{!(>::)}"

    let operator_with_curly_braces = test "{!(.*{})}"

    let quotes_with_dash = test "{!\"my-name\"}"

    let quotes_with_curly_braces = test "{!\"}\"}"

    let operator_with_curly_braces = test "{!( } )}"

    let operator_unbalanced = test "{!(.*()}"

    let operator_eof = test "{!(.*()"
  end in
  ()

let reference_with_text () =
  let module Reference_with_text = struct
    let basic = test "{{!foo} bar}"

    let degenerate = test "{{!foo}}"

    let empty = test "{{!foo} }"

    let nested_markup = test "{{!foo} {b bar}}"

    let in_markup = test "{e {{!foo} bar}}"

    let no_separating_space = test "{{!foo}bar}"

    let kind = test "{{!val:foo} bar}"

    let nested_reference = test "{{!foo} {!bar}}"

    let nested_empty = test "{{!foo} {{!bar}}}"

    let nested_through_emphasis = test "{{!foo} {e {{!bar} baz}}}"

    let simple_through_emphasis = test "{{!foo} {e {!bar}}}"

    let empty_target = test "{{!} foo}"

    let whitespace_only_in_target = test "{{! } foo}"

    let internal_whitespace = test "{{!( * )} baz}"

    let unterminated = test "{{!foo"

    let unterminated_content = test "{{!foo} bar"
  end in
  ()

let medias () =
  let module Medias = struct
    let basic_simple =
      test
        "{image!foo}\n\n\
         {audio!foo}\n\n\
         {video!foo}\n\n\
         {image:foo}\n\n\
         {audio:foo}\n\n\
         {video:foo}"

    let basic =
      test
        "{{image!foo}bar}\n\n\
         {{audio!foo}bar}\n\n\
         {{video!foo}bar}\n\n\
         {{image:foo}bar}\n\n\
         {{audio:foo}bar}\n\n\
         {{video:foo}bar}"

    let empty = test "{{image!foo}}"

    let whitespace = test "{{image!foo}   }"

    let trimming = test "{{image!foo}    hello     }"

    let nested_markup_is_uninterpreted = test "{{image!foo}{b bar}}"

    let in_markup = test "{ul {li {{image!foo}bar}}}"

    let unterminated_image = test "{{image!foo"

    let unterminated_image_simple = test "{image!foo"

    let unterminated_video = test "{{video!foo"

    let unterminated_video_simple = test "{video!foo"

    let unterminated_audio = test "{{audio!foo"

    let unterminated_audio_simple = test "{audio!foo"

    let unterminated_content = test "{{image!foo} bar"

    let newline_in_content = test "{{image!foo} bar \n baz}"
  end in
  ()

let link () =
  let module Link = struct
    let basic = test "{{:foo} bar}"

    let nested_markup = test "{{:foo} {b bar}}"

    let in_markup = test "{e {{:foo} bar}}"

    let no_separating_space = test "{{:foo}bar}"

    let nested_link = test "{{:foo} {{:bar} baz}}"

    let nested_through_emphasis = test "{{:foo} {e {{:bar} baz}}}"

    let reference_through_emphasis = test "{{:foo} {e {!bar}}}"

    let nested_in_reference = test "{{!foo} {e {{:bar} baz}}}"

    let empty_target = test "{{:} foo}"

    let whitespace_only_in_target = test "{{: } foo}"

    let empty = test "{{:foo}}"

    let internal_whitespace = test "{{:foo bar} baz}"

    let unterminated = test "{{:foo"

    let single_braces = test "{:foo}"

    let unterminated_single_braces = test "{:foo"

    let empty_single_braces = test "{:}"

    let single_braces_whitespace_only = test "{: }"
  end in
  ()

let module_list () =
  let module Module_list = struct
    let basic = test "{!modules:Foo}"

    let two = test "{!modules:Foo Bar}"

    let extra_whitespace = test "{!modules: Foo  Bar }"

    let newline = test "{!modules:Foo\nBar}"

    let cr_lf = test "{!modules:Foo\r\nBar}"

    let empty = test "{!modules:}"

    let whitespace_only = test "{!modules: }"

    let unterminated = test "{!modules:"

    let in_paragraph = test "foo {!modules:Foo}"

    let followed_by_word = test "{!modules:Foo} foo"

    let in_list = test "- {!modules:Foo}"
  end in
  ()

let code_block () =
  let module Code_block = struct
    let basic = test "{[foo]}"

    let empty = test "{[]}"

    let whitespace_only = test "{[ ]}"

    let blank_line_only = test "{[\n  \n]}"

    let whitespace = test "{[foo bar]}"

    let newline = test "{[foo\nbar]}"

    let cr_lf = test "{[foo\r\nbar]}"

    let blank_line = test "{[foo\n\nbar]}"

    let leading_whitespace = test "{[ foo]}"

    let leading_whitespace_two = test "{[ foo\n bar]}"

    let leading_whitespace_two_cr_lf = test "{[ foo\r\n bar]}"

    let leading_whitespace_two_different_indent = test "{[ foo\n   bar]}"

    let leading_whitespace_two_different_indent_rev = test "{[   foo\n bar]}"

    let leading_whitespace_two_different_indent_reloc =
      test "{[ foo\n      bar]}"

    let leading_whitespace_with_empty_line = test "{[ foo\n\n bar]}"

    let leading_whitespace_with_whitespace_line_short =
      test "{[  foo\n \n  bar]}"

    let leading_whitespace_with_whitespace_line_long =
      test "{[ foo\n   \n bar]}"

    let leading_whitespace_leading_newline = test "{[\n  foo\n  bar\n]}"

    let leading_tab = test "{[\tfoo]}"

    let leading_tab_two = test "{[\tfoo\n\tbar]}"

    let leading_tab_two_different_indent = test "{[\tfoo\n\t\tbar]}"

    let leading_newline = test "{[\nfoo]}"

    let leading_cr_lf = test "{[\r\nfoo]}"

    let leading_newlines = test "{[\n\nfoo]}"

    let leading_newline_with_space = test "{[\n foo]}"

    let leading_newline_with_trash = test "{[ \nfoo]}"

    let nested_opener = test "{[{[]}"

    let nested_closer = test "{[foo]}]}"

    let nested_bracket = test "{[]]}"

    let two_nested_brackets = test "{[]]]}"

    let nested_brackets_in_text = test "{[foo]]bar]}"

    let trailing_whitespace = test "{[foo ]}"

    let trailing_tab = test "{[foo\t]}"

    let trailing_newline = test "{[foo\n]}"

    let trailing_cr_lf = test "{[foo\r\n]}"

    let trailing_newlines = test "{[foo\n\n]}"

    let preceded_by_whitespace = test "{[foo]}"

    let followed_by_whitespace = test "{[foo]}"

    let two_on_one_line = test "{[foo]} {[bar]}"

    let two = test "{[foo]}\n{[bar]}"

    let two_with_blank_line = test "{[foo]}\n\n{[bar]}"

    let followed_by_words = test "{[foo]} bar"

    let preceded_by_words = test "foo {[bar]}"

    let preceded_by_paragraph = test "foo\n{[bar]}"

    let followed_by_paragraph = test "{[foo]}\nbar"

    let unterminated = test "{[foo"

    let unterminated_bracket = test "{[foo]"

    let trailing_cr = test "{[foo\r]}"

    let comment = test "{[(* foo *)\nlet bar = ()]}"

    let docstring = test "{[(** foo *)\nlet bar = ()]}"

    let docstring_with_code_block = test "{[(** {[foo]} *)\nlet bar = ()]}"

    let code_block_with_meta =
      test "{@ocaml env=f1 version>=4.06 [code goes here]}"

    let code_block_with_output = test "{delim@ocaml[foo]delim[output {b foo}]}"

    let delimited_code_block_with_meta_and_output =
      test "{delim@ocaml env=f1 version>=4.06 [foo]delim[output {b foo}]}"

    (* Code block contains ']['. *)
    let code_block_with_output_without_delim = test "{[foo][output {b foo}]}"

    (* Code block contains ']['. *)
    let code_block_with_output_and_lang_without_delim =
      test "{@ocaml[foo][output {b foo}]}"

    let code_block_with_output_unexpected_delim =
      test "{[foo]unexpected[output {b foo}]}"

    let code_block_with_output_lang_unexpected_delim =
      test "{@ocaml[foo]unexpected[output {b foo}]}"

    let code_block_with_output_wrong_delim =
      test "{delim@ocaml[foo]wrong[output {b foo}]delim}"

    let code_block_empty_meta = test "{@[code goes here]}"

    let unterminated_code_block_with_meta = test "{@meta[foo"

    let unterminated_code_block_with_meta = test "{@met"

    let newlines_after_langtag = test "{@ocaml\n[ code ]}"

    let newlines_after_meta = test "{@ocaml kind=toplevel\n[ code ]}"

    let spaces_after_meta = test "{@ocaml kind=toplevel [ code ]}"

    let spaces_and_newline_after_meta =
      test "{@ocaml kind=toplevel \n  [ code ]}"

    let newlines_inside_meta = test "{@ocaml kind=toplevel\nenv=e1[ code ]}"

    let newlines_between_meta = test "{@ocaml\nkind=toplevel[ code ]}"

    let empty_key = test "{@ocaml =foo [ code ]}"

    let no_escape_without_quotes = test {|{@ocaml \n\t\b=hello [ code ]}|}

    let escape_within_quotes = test {|{@ocaml "\065"=hello [ code ]}|}

    let langtag_non_word = test "{@ocaml,top[ code ]}"

    let delimited_code_block =
      test "{delim@ocaml[ all{}[2[{{]doo}}]]'''(* ]} ]delim}"

    let code_block_with_output =
      test
        {|{delim@ocaml[ let x = ]delim[ {err@mdx-error[ here's the error ]} ]err}
        ]delim}|}

    let delimited_code_block_with_output = test "{delim@ocaml[ foo ]delim[ ]}"

    (** {3 Code block indentation}

        Code blocks strip some whitespace from the content of a multiline code
        block. We test that here, as well as warnings related to that.

        Let's test warnings first. When the indentation is less than the
        identation of the opening bracket, we warn. *)

    let too_few_indentation = test {|
   {[
  foo
 ]}|}

    let multiline_without_newline = test {|
   {[ foo
 ]}|}

    let everything_is_wrong = test {|
   {[ foo
  bar ]}|}

    let all_good_multiline = test {|
   {[
   foo
 ]}|}

    let all_good_single_line = test {| {[ foo ]} |}

    (** Let's now test that the correct amount of whitespace is removed. *)

    let descending_stair = test {|
   {[
   foo
     bar
       baz
 ]}|}

    let ascending_stair = test {|
   {[
       baz
     bar
   foo
 ]}|}

    let indented_after_opening_fence =
      test {|
   {[
       baz
     bar
       foo
 ]}|}

    let indentation_warning_case = test {|
   {[
    baz
  bar
    foo
 ]}|}

    (** {3 Metadata tags}

        Code blocks have a metadata field, with bindings and simple tags. *)

    let lots_of_tags =
      test
        {|{@ocaml env=f1 version=4.06 "tag with several words" "binding with"=singleword also="other case" "everything has"="multiple words" [foo]}|}

    let lots_of_tags_with_newlines =
      test
        {|{@ocaml
         env=f1
         version=4.06
         single_tag
         "tag with several words"
         "binding with"=singleword
         also="other case"
         "everything has"="multiple words"
         [foo]}|}

    let escaping1 = test {|{@ocaml "\""="\"" [foo]}|}

    let escaping2 = test {|{@ocaml \"=\" [foo]}|}

    let two_slashes_are_required = test {|{@ocaml "\\" [foo]}|}

    let escaped_char_are_allowed_but_warn =
      test {|
     {@ocaml "\a\b\c" [foo]}|}

    let escaped_char_are_allowed_but_warn2 =
      test {|
     {@ocaml "\a\b\c"="\x\y\z" [foo]}|}
  end in
  ()

let verbatim () =
  let module Verbatim = struct
    let basic = test "{v foo v}"

    let empty = test "{v v}"

    let degenerate = test "{vv}"

    let whitespace_only = test "{v  v}"

    let blank_line_only = test "{v\n  \nv}"

    let no_leading_whitespace = test "{vfoo v}"

    let no_trailing_whitespace = test "{v foov}"

    let multiple_leading_whitespace = test "{v  foo v}"

    let multiple_trailing_whitespace = test "{v foo  v}"

    let leading_tab = test "{v\tfoo v}"

    let leading_newline = test "{v\nfoo v}"

    let leading_cr_lf = test "{v\r\nfoo v}"

    let trailing_tab = test "{v foo\tv}"

    let trailing_newline = test "{v foo\nv}"

    let trailing_cr_lf = test "{v foo\r\nv}"

    let internal_whitespace = test "{v foo bar v}"

    let newline = test "{v foo\nbar v}"

    let cr_lf = test "{v foo\r\nbar v}"

    let blank_line = test "{v foo\n\nbar v}"

    let leading_newlines = test "{v\n\nfoo v}"

    let leading_newline_with_space = test "{v\n foo v}"

    let leading_newline_with_trash = test "{v \nfoo v}"

    let nested_opener = test "{v {v v}"

    let nested_closer = test "{v foo v} v}"

    let nested_closer_with_word = test "{v {dev} v}"

    let nested_v = test "{v v v}"

    let two_nested_vs = test "{v vv v}"

    let nested_v_at_end = test "{v vv}"

    let two_nested_vs_at_end = test "{v vvv}"

    let nested_vs_in_text = test "{v foovvbar v}"

    let trailing_newlines = test "{v foo\n\nv}"

    let preceded_by_whitespace = test "{v foo v}"

    let followed_by_whitespace = test "{v foo v}"

    let two_on_one_line = test "{v foo v} {v bar v}"

    let two = test "{v foo v}\n{v bar v}"

    let two_with_blank_line = test "{v foo v}\n\n{v bar v}"

    let followed_by_words = test "{v foo v} bar"

    let preceded_by_words = test "foo {v bar v}"

    let preceded_by_paragraph = test "foo\n{v bar v}"

    let followed_by_paragraph = test "{v foo v}\nbar"

    let unterminated = test "{v foo"

    let unterminated_v = test "{v foo v"

    let unterminated_empty = test "{v"

    let unterminated_whitespace = test "{v"

    let unterminated_whitespace_2 = test "{v"

    let trailing_cr = test "{v foo\rv}"

    (** {3 Verbatim indentation}

        Verbatims work as code blocks. We test that here. *)

    let too_few_indentation = test {|
   {v
  foo
 v}|}

    let multiline_without_newline = test {|
   {v foo
 v}|}

    let everything_is_wrong = test {|
   {[ foo
  bar ]}|}

    let all_good_multiline = test {|
   {v
   foo
 v}|}

    let all_good_single_line = test {| {v foo v} |}

    (** Let's now test that the correct amount of whitespace is removed. *)

    let descending_stair = test {|
   {v
   foo
     bar
       baz
 v}|}

    let ascending_stair = test {|
   {v
       baz
     bar
   foo
 v}|}

    let indented_after_opening_fence =
      test {|
   {v
       baz
     bar
       foo
 v}|}

    let indentation_warning_case = test {|
   {v
    baz
  bar
    foo
 v}|}
  end in
  ()

let shorthand_list () =
  let module Shorthand_list = struct
    let basic = test "- foo"

    let multiple_items = test "- foo\n- bar"

    let two_lists = test "- foo\n\n- bar"

    let ordered = test "+ foo"

    let leading_whitespace = test "- foo"

    let trailing_whitespace = test "- foo"

    let bullet_in_line = test "- foo - bar"

    let bullet_in_line_immediately = test "- - foo"

    let code_block = test "- {[foo]}"

    let verbatim = test "- {v foo v}"

    let multiple_blocks = test "- foo\n{[bar]}"

    let followed_by_code_block = test "- foo\n\n{[bar]}"

    let different_kinds = test "- foo\n+ bar"

    let no_content = test "-"

    let immediate_newline = test "-\nfoo"

    let immediate_blank_line = test "-\n\nfoo"

    let immediate_markup = test "-{b foo}"

    let after_code_block = test "{[foo]} - bar"
  end in
  ()

let explicit_list () =
  let module Explicit_list = struct
    let basic = test "{ul {li foo}}"

    let ordered = test "{ol {li foo}}"

    let two_items = test "{ul {li foo} {li bar}}"

    let items_on_separate_lines = test "{ul {li foo}\n{li bar}}"

    let blank_line = test "{ul {li foo}\n\n{li bar}}"

    let blank_line_in_item = test "{ul {li foo\n\nbar}}"

    let junk = test "{ul foo}"

    let junk_with_no_whitespace = test "{ulfoo}"

    let empty = test "{ul}"

    let unterminated_list = test "{ul"

    let no_whitespace = test "{ul{li foo}}"

    let whitespace_at_end_of_item = test "{ul {li foo\n\n\n}}"

    let unterminated_li_syntax = test "{ul {li foo"

    let unterminated_left_curly_brace = test "{ul {- foo"

    let empty_li_styntax = test "{ul {li }}"

    let empty_left_curly_brace = test "{ul {- }}"

    let li_syntax_without_whitespace = test "{ul {lifoo}}"

    let li_syntax_followed_by_newline = test "{ul {li\nfoo}}"

    let li_syntax_followed_by_cr_lf = test "{ul {li\r\nfoo}}"

    let li_syntax_followed_by_blank_line = test "{ul {li\n\nfoo}}"

    let left_curly_brace_without_whitespace = test "{ul {-foo}}"

    let mixed_list_items = test "{ul {li foo} {- bar}}"

    let nested = test "{ul {li {ul {li foo}}}}"

    let shorthand_in_explicit = test "{ul {li - foo\n- bar}}"

    let explicit_in_shorthand = test "- {ul {li foo}}"

    let bare_li_syntax = test "{li foo}"

    let bare_left_curly_brace = test "{- foo"

    let after_code_block = test "{[foo]} {ul {li bar}}"
  end in
  ()

let deprecated () =
  let module Deprecated = struct
    let basic = test "@deprecated"

    let words = test "@deprecated foo bar"

    let multiline = test "@deprecated foo\nbar"

    let paragraphs = test "@deprecated foo\n\nbar"

    let whitespace_only = test "@deprecated"

    let immediate_newline = test "@deprecated\nfoo"

    let immediate_cr_lf = test "@deprecated\r\nfoo"

    let immediate_blank_line = test "@deprecated\n\nfoo"

    let extra_whitespace = test "@deprecated  foo"

    let followed_by_deprecated = test "@deprecated foo\n@deprecated bar"

    let followed_by_deprecated_cr_lf = test "@deprecated foo\r\n@deprecated bar"

    let nested_in_self = test "@deprecated foo @deprecated bar"

    let nested_in_self_at_start = test "@deprecated @deprecated foo"

    let preceded_by_paragraph = test "foo\n@deprecated"

    let preceded_by_shorthand_list = test "- foo\n@deprecated"

    let with_shorthand_list = test "@deprecated - foo"

    let with_shorthand_list_double_item = test "@deprecated - foo\n- bar"

    let double_implicitly_ended =
      test "@deprecated - foo\n- bar\n\nNew paragraph"

    let with_shorthand_list_after_newline = test "@deprecated\n- foo"

    let prefix = test "@deprecatedfoo"

    let after_code_block = test "{[foo]} @deprecated"

    let followed_by_section = test "@deprecated foo\n{2 Bar}"
  end in
  ()

let param () =
  let module Param = struct
    let basic = test "@param foo"

    let bare = test "@param"

    let bare_with_whitespace = test "@param"

    let immediate_newline = test "@param\nfoo"

    let followed_by_whitespace = test "@param foo"

    let extra_whitespace = test "@param  foo"

    let words = test "@param foo bar baz"

    let multiline = test "@param foo\nbar\nbaz"

    let paragraphs = test "@param foo bar\n\nbaz"

    let two = test "@param foo\n@param bar"

    let nested = test "@param foo @param bar"

    let preceded_by_paragraph = test "foo\n@param bar"

    let prefix = test "@paramfoo"

    let after_code_block = test "{[foo]} @param foo"
  end in
  ()

let raise () =
  let module Raise = struct
    let basic = test "@raise Foo"

    let bare = test "@raise"

    let words = test "@raise foo bar baz"

    let prefix = test "@raisefoo"
  end in
  ()

let return () =
  let module Return = struct
    let basic = test "@return"

    let words = test "@return foo bar"

    let prefix = test "@returnfoo"
  end in
  ()

let see () =
  let module See = struct
    let url = test "@see <foo>"

    let file = test "@see 'foo'"

    let document = test "@see \"foo\""

    let bare = test "@see"

    let unterminated_url = test "@see <foo"

    let unterminated_file = test "@see 'foo"

    let unterminated_document = test "@see foo"

    let no_space = test "@see<foo>"

    let words = test "@see <foo> bar"

    let prefix = test "@seefoo"

    let after_code_block = test "{[foo]} @see <foo>"

    let url_attempted_nested_closer = test "@see <foo>bar>"

    let file_attempted_nested_closer = test "@see 'foo'bar'"

    let document_attempted_nested_closer = test "@see \"foo\"bar\""
  end in
  ()

let since () =
  let module Since = struct
    let basic = test "@since foo"

    let bare = test "@since"

    let prefix = test "@sincefoo"

    let with_whitespace = test "@since foo bar"

    let leading_whitespace = test "@since  foo"

    let trailing_whitespace = test "@since foo"

    let whitespace_only = test "@since"
  end in
  ()

let before () =
  let module Before = struct
    let basic = test "@before Foo"

    let bare = test "@before"

    let words = test "@before foo bar baz"

    let prefix = test "@beforefoo"
  end in
  ()

let version () =
  let module Version = struct
    let basic = test "@version foo"

    let bare = test "@version"

    let prefix = test "@versionfoo"

    let with_whitespace = test "@version foo bar"

    let leading_whitespace = test "@version  foo"

    let trailing_whitespace = test "@version foo"

    let whitespace_only = test "@version"
  end in
  ()

let canonical () =
  let module Canonical = struct
    let basic = test "@canonical Foo"

    let empty = test "@canonical"

    let whitespace_only = test "@canonical"

    let extra_whitespace = test "@canonical  Foo"

    let prefix = test "@canonicalfoo"

    (* TODO This should probably be an error of some kind, as Foo Bar is not a
           valid module path. *)
    let with_whitespace = test "@canonical Foo Bar"
  end in
  ()

let inline () =
  let module Inline = struct
    let basic = test "@inline"

    let prefix = test "@inlinefoo"

    let extra_whitespace = test "@inline"

    let followed_by_junk = test "@inline foo"

    let followed_by_paragraph = test "@inline\nfoo"

    let followed_by_tag = test "@inline\n@deprecated"

    let with_list = test "@inline - foo"
  end in
  ()

let open_ () =
  let module Open = struct
    let basic = test "@open"

    let prefix = test "@openfoo"

    let extra_whitespace = test "@open"

    let followed_by_junk = test "@open foo"

    let followed_by_paragraph = test "@open\nfoo"

    let followed_by_tag = test "@open\n@deprecated"

    let with_list = test "@open - foo"
  end in
  ()

let closed () =
  let module Closed = struct
    let basic = test "@closed"

    let prefix = test "@closedfoo"

    let extra_whitespace = test "@closed"

    let followed_by_junk = test "@closed foo"

    let followed_by_paragraph = test "@closed\nfoo"

    let followed_by_tag = test "@closed\n@deprecated"

    let with_list = test "@closed - foo"
  end in
  ()

let hidden () =
  let module Hidden = struct
    let basic = test "@hidden"

    let prefix = test "@hiddenfoo"

    let extra_whitespace = test "@hidden"

    let followed_by_junk = test "@hidden foo"

    let followed_by_paragraph = test "@hidden\nfoo"

    let followed_by_tag = test "@hidden\n@deprecated"

    let with_list = test "@hidden - foo"
  end in
  ()

let bad_markup () =
  let module Bad_markup = struct
    let left_brace = test "{"

    let left_brace_with_letter = test "{g"

    let left_brace_with_letters = test "{gg"

    let empty_braces = test "{}"

    let left_space = test "{ foo}"

    let left_spaces = test "{  foo}"

    let left_space_eof = test "{"

    let braces_instead_of_brackets = test "{foo}"

    let right_brace = test "}"

    let right_brace_in_paragraph = test "foo}"

    let multiple_right_brace = test "foo } bar } baz"

    let right_brace_in_list_item = test "- foo}"

    let right_brace_in_code_span = test "[foo}]"

    let right_brace_in_code_block = test "{[foo}]}"

    let right_brace_in_verbatim_text = test "{v foo} v}"

    let right_brace_in_author = test "@author Foo}"

    let right_brace_in_deprecated = test "@deprecated }"

    let right_bracket = test "]"

    let right_bracket_in_paragraph = test "foo]"

    let right_bracket_in_shorthand_list = test "- foo]"

    let right_bracket_in_code_span = test "[]]"

    let right_bracket_in_style = test "{b]}"

    let right_bracket_in_verbatim = test "{v ] v}"

    let right_bracket_in_list = test "{ul ]}"

    let right_bracket_in_list_item = test "{ul {li ]}}"

    let right_bracket_in_heading = test "{2 ]}"

    let right_bracket_in_author = test "@author Foo]"

    let at = test "@"

    let cr = test ""
  end in
  ()

let utf_8 () =
  let module Utf_8 = struct
    let lambda = test "\xce\xbb"

    let words = test "\xce\xbb \xce\xbb"

    let no_validation = test "Î"

    let escapes = test "\xce\xbb\\}"

    let newline = test "\xce\xbb \n \xce\xbb"

    let paragraphs = test "\xce\xbb \n\n \xce\xbb"

    let code_span = test "[\xce\xbb]"

    let minus = test "\xce\xbb-\xce\xbb"

    let shorthand_list = test "- \xce\xbb"

    let styled = test "{b \xce\xbb}"

    let reference_target = test "{!\xce\xbb}"

    let code_block = test "{[\xce\xbb]}"

    let verbatim = test "{v \xce\xbb v}"

    let label = test "{2:\xce\xbb Bar}"

    let author = test "@author \xce\xbb"

    let param = test "@param \xce\xbb"

    let raise = test "@raise \xce\xbb"

    let see = test "@see <\xce\xbb>"

    let since = test "@since \xce\xbb"

    let before = test "@before \xce\xbb"

    let version = test "@version \xce\xbb"

    let right_brace = test "\xce\xbb}"
  end in
  ()

let comment_location () =
  let module Comment_location = struct
    let error_on_first_line = test "@foo"

    let error_on_second_line = test "  \n  @foo"
  end in
  ()

let unsupported () =
  let module Unsupported = struct
    (* test "index list"
       "{!indexlist}"
       (Ok []); *)

    let left_alignment = test "{L foo}"

    let center_alignment = test "{C foo}"

    let right_alignment = test "{R foo}"

    let custom_style = test "{c foo}"

    let custom_tag = test "@custom"

    let custom_reference_kind = test "{!custom:foo}"

    let html_tag = test "<b>foo</b>"
  end in
  ()

let locations () =
  let module Locations = struct
    (* test "index list"
       "{!indexlist}"
       (Ok []); *)

    let lexing_pos_to_sexp : Lexing.position -> sexp =
     fun v ->
      List
        [
          List [ Atom "pos_fname"; Atom v.pos_fname ];
          List [ Atom "pos_bol"; Atom (string_of_int v.pos_bol) ];
          List [ Atom "pos_lnum"; Atom (string_of_int v.pos_lnum) ];
          List [ Atom "pos_cnum"; Atom (string_of_int v.pos_cnum) ];
        ]

    let parser_output formatter pv =
      let ast, warnings = Odoc_parser.(ast pv, warnings pv) in
      let at conv v =
        let { Loc.start; end_; _ } = Loc.location v in
        let v' = v |> conv in
        let start' =
          Odoc_parser.position_of_point pv start |> lexing_pos_to_sexp
        in
        let start'' = Location_to_sexp.point start in
        let end' =
          Odoc_parser.position_of_point pv end_ |> lexing_pos_to_sexp
        in
        let end'' = Location_to_sexp.point end_ in
        List
          [
            List [ Atom "start"; start' ];
            List [ Atom "start_loc"; start'' ];
            List [ Atom "end"; end' ];
            List [ Atom "end_loc"; end'' ];
            List [ Atom "value"; v' ];
          ]
      in
      let sexp = Ast_to_sexp.(docs { at } ast) in
      let warnings = List (List.map error warnings) in
      let output =
        List
          [ List [ Atom "output"; sexp ]; List [ Atom "warnings"; warnings ] ]
      in
      Sexplib0.Sexp.pp_hum formatter output;
      Format.pp_print_flush formatter ()

    let test
        ?(location =
          Lexing.{ pos_bol = 0; pos_cnum = 0; pos_lnum = 1; pos_fname = "none" })
        text =
      let ast = Odoc_parser.parse_comment ~location ~text in
      Expect_test_helper.expect ~input:text (fun f -> parser_output f ast)

    let non_offset_location = test "one\n two\n  three"

    let offset_location =
      test
        ~location:
          Lexing.
            { pos_bol = 10; pos_cnum = 20; pos_lnum = 2; pos_fname = "none" }
        "one\n two\n  three"
  end in
  ()

let math () =
  let module Math = struct
    let block = test "{math \\sum_{i=0}^n x^i%}"

    let complex_block =
      test
        {|{math
      \alpha(x)=\left\{
                \begin{array}{ll}                 % beginning of the array
                  x \% 4\\                        % some variable modulo 4
                  \frac{1}{1+e^{-kx}}\\           % something else
                  \frac{e^x-e^{-x}}{e^x+e^{-x}}   % another action
                \end{array}                       % end of the array
              \right.
      }|}

    let inline = test "{m x + 4}"

    let inline_nested = test "{m \\sub_{i=0}^n x^i}"

    let inline_false_nesting = test "{m \\{ \\mathbb{only_left}}"

    let inline_false_terminator = test "{m \\mathbb{only_left}\\}}"
  end in
  ()
