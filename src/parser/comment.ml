(* This module is a recursive descent parser for the ocamldoc syntax. The parser
   consumes a token stream of type [Token.t Stream.t], provided by the lexer,
   and produces a comment AST of the type defined in [Model.Comment] – see
   [src/model/comment.ml].

   The AST has two main levels: inline elements, which can appear inside
   paragraphs, and are spaced horizontally when presented, and block elements,
   such as paragraphs and lists, which are spaced vertically when presented.
   Block elements contain inline elements, but not vice versa.

   Corresponding to this, the parser has three "main" functions:

   - [delimited_non_link_inline_element_list] parses a run of inline elements
     that is delimited by curly brace markup ([{...}]).
   - [paragraph] parses a run of inline elements that make up a paragraph, and
     is not explicitly delimited with curly braces.
   - [block_element_list] parses a sequence of block elements. A comment is a
     sequence of block elements, so [block_element_list] is the top-level
     parser. It is also used for list item and tag content.

   The parser raises exceptions with constructor [Helpers.Parse_error]. These
   contain raw byte offsets as locations. These are caught in module [Parser_],
   and translated to line/column locations. *)



(* {2 Input} *)

(* Tokens paired with their start and end byte offsets.

   This type constructor is mainly used as [Token.t stream_head]. However, in
   places where only a subset of tokens is allowed, it is used with more
   restrictive types, such as [[ `Space | `Single_newline ] stream_head]. *)
type 'token stream_head = (int * int) * 'token

(* What the parser needs from the outside world. A value of type [input] is
   passed around between all the parsing functions.

   - [token_stream] is the stream of tokens emitted by the lexer.
   - [parent_of_sections] is the definition or page containing the comment being
     parsed. It is used to fully qualify section labels.

  In addition to a value of type [input], some parsing functions also take a
  value of type ['a stream_head], for some ['a] that is narrower than [Token.t].
  This is done when the stream head has already been examined by the caller, and
  it allows a precise and limited set of cases in the function. *)
type input = {
  permissive : bool;
  sections : [ `Allow_all_sections | `No_titles_allowed | `No_sections ];
  offset_to_location : int -> Model.Location_.point;
  token_stream : (Token.t stream_head) Stream.t;
  parent_of_sections : Model.Paths.Identifier.label_parent;
  accumulated_warnings : (Helpers.raw_parse_error list) ref;
}

let junk input =
  Stream.junk input.token_stream

let peek input =
  match Stream.peek input.token_stream with
  | Some token -> token
  | None -> assert false
  (* The last token in the stream is always [`End], and it is never consumed by
     the parser, so the [None] case is impossible. *)

let npeek n input =
  Stream.npeek n input.token_stream

let warning input error =
  if input.permissive then
    input.accumulated_warnings := error::!(input.accumulated_warnings)
  else
    raise (Helpers.Parse_error error)

type 'a with_location = 'a Model.Location_.with_location

let at_token input (start_offset, end_offset) value : _ with_location =
  {
    location = {
      start = input.offset_to_location start_offset;
      end_ = input.offset_to_location end_offset;
    };
    value;
  }

let token_span input (start_offset, _) (_, end_offset) value : _ with_location =
  {
    location = {
      start = input.offset_to_location start_offset;
      end_ = input.offset_to_location end_offset;
    };
    value;
  }



module Grammar = Model.Comment
module Raise = Helpers



(* {2 Non-link inline elements} *)

(* Consumes tokens that make up a single word – a sequence of consecutive
   [`Word _], [`Minus], and [`Plus] tokens.

   This function is only called when the first token in the token stream is
   known to be [`Word _], [`Minus], or [`Plus]. In case the first token is
   [`Minus] or [`Plus], the caller has already decided that the token is not the
   beginning of a shorthand list item (i.e., it is not the first non-whitespace
   token on its line).

   There are two reasons consecutive tokens need to be combined into a single
   word:

   - [`Minus] and [`Plus] are part of words when they are not list bullets, but
     that determination is made by the parser. The lexer blindly emits them as
     [`Minus] and [`Plus], so a word like "cool-headed" becomes the token stream
     [`Word "cool"; `Minus; `Word "headed"].
   - For convenience in the lexer, escape sequences are emitted as separate
     tokens, so "brace \{" becomes [`Word "brace"; `Word "{"].

   This parser stops on the first non-word token, and does not consume it. *)
let word
    : input -> (int * int) ->
        Grammar.non_link_inline_element with_location =
    fun input start_location ->
  let rec consume_word_tokens end_location acc =
    match peek input with
    | l, `Word w ->
      junk input;
      consume_word_tokens l (acc ^ w)

    | l, `Minus ->
      junk input;
      consume_word_tokens l (acc ^ "-")

    | l, `Plus ->
      junk input;
      consume_word_tokens l (acc ^ "+")

    | _ ->
      token_span input start_location end_location (`Word acc)
  in
  consume_word_tokens start_location ""

(* Consumes tokens that make up a single non-link inline element:

   - a horizontal space ([`Space], significant in inline elements),
   - a word (see [word]),
   - a code span ([...], [`Code_span _]), or
   - styled text ({e ...}).

   The latter requires a recursive call to
   [delimited_non_link_inline_element_list], defined below.

   This should be part of [delimited_non_link_inline_element_list]; however, it
   is also called by function [paragraph], which uses it to parse the non-link
   subset of general inline elements. As a result, it is factored out, and made
   mutually-recursive with [delimited_non_link_inline_element_list].

   This is called only when it is known that the first token in the list is the
   beginning of a non-link inline element. In the case of [`Minus] and [`Plus],
   that means the caller has determined that they are not a list bullet (i.e.,
   not the first non-whitespace tokens on their line).

   This function consumes exactly the tokens that make up the element. *)
let rec non_link_inline_element
    : [> ] stream_head -> input ->
        Grammar.non_link_inline_element with_location =
    fun stream_head input ->

  match stream_head with
  | l, `Space ->
    junk input;
    at_token input l `Space

  | l, `Word _
  | l, `Minus
  | l, `Plus ->
    word input l

  | l, `Code_span c ->
    junk input;
    at_token input l (`Code_span c)

  | l, (`Begin_style s as parent_markup) ->
    junk input;
    let requires_leading_whitespace =
      match s with
      | `Superscript | `Subscript -> false
      | _ -> true
    in
    let content, end_location =
      delimited_non_link_inline_element_list
        ~parent_markup
        ~parent_markup_location:l
        ~requires_leading_whitespace
        input
    in
    if content = [] then
      Raise.cannot_be_empty l ~what:(Token.describe parent_markup);
    token_span input l end_location (`Styled (s, content))

(* Consumes tokens that make up a sequence of non-link inline elements. See
   function [non_link_inline_element] for a list of what those are.

   It turns out, as an accident of the comment grammar, that every sequence of
   non-link inline elements is nested in markup that can only be properly ended
   with a '}', a [`Right_brace] token. This parser consumes that token also.

   The sequences are also preceded by some markup like '{b'. Some of these
   markup tokens require whitespace immediately after the token, and others not.
   The caller indicates which way that is through the
   [~requires_leading_whitespace] argument.

   Whitespace is significant in non-link inline element lists. In particular,
   "foo [bar]" is represented as [`Word "foo"; `Space; `Code_span "bar"], while
   "foo[bar]" is [`Word "foo"; `Code_span "bar"]. It doesn't matter how much
   whitespace is there, just whether it is present or not. Single newlines and
   horizontal space in any amount are allowed. Blank lines are not, as these are
   separators for {e block} elements.

   The first and last elements emitted will not be [`Space], i.e. [`Space]
   appears only between other non-link inline elements.

   The [~parent_markup] and [~parent_markup_location] arguments are used for
   generating error messages. *)
and delimited_non_link_inline_element_list
    : parent_markup:[< Token.t ] ->
      parent_markup_location:(int * int) ->
      requires_leading_whitespace:bool ->
      input ->
        (Grammar.non_link_inline_element with_location) list * (int * int) =
    fun
      ~parent_markup
      ~parent_markup_location
      ~requires_leading_whitespace
      input ->

  (* [~at_start_of_line] is used to interpret [`Minus] and [`Plus]. These are
     word tokens if not the first non-whitespace tokens on their line. Then,
     they are allowed in a non-link element list. *)
  let rec consume_non_link_inline_elements
      : at_start_of_line:bool ->
        (Grammar.non_link_inline_element with_location) list ->
          (Grammar.non_link_inline_element with_location) list * (int * int) =
      fun ~at_start_of_line acc ->

    match peek input with
    | l, `Right_brace ->
      junk input;
      List.rev acc, l

    (* The [`Space] token is not space at the beginning or end of line, because
       that is combined into [`Single_newline] or [`Blank_line] tokens. It is
       also not at the beginning of markup (after e.g. '{b'), because that is
       handled separately before calling
       [consume_non_link_inline_elements], and not immediately before '}',
       because that is combined into the [`Right_brace] token by the lexer. So,
       it is an internal space, and we want to add it to the non-link inline
       element list. *)
    | _, `Space
    | _, `Word _
    | _, `Code_span _
    | _, `Begin_style _ as stream_head ->
      let acc = (non_link_inline_element stream_head input)::acc in
      consume_non_link_inline_elements ~at_start_of_line:false acc

    | l, `Single_newline ->
      junk input;
      let element = at_token input l `Space in
      consume_non_link_inline_elements ~at_start_of_line:true (element::acc)

    | l, (`Minus | `Plus as bullet) as stream_head ->
      if not at_start_of_line then
        let acc = (non_link_inline_element stream_head input)::acc in
        consume_non_link_inline_elements ~at_start_of_line:false acc
      else
        let suggestion =
          Printf.sprintf
            "move %s so it isn't the first thing on the line"
            (Token.print bullet)
        in
        Raise.not_allowed
          l
          ~what:(Token.describe bullet)
          ~in_what:(Token.describe parent_markup)
          ~suggestion

    | l, token ->
      Raise.not_allowed
        l ~what:(Token.describe token) ~in_what:(Token.describe parent_markup)
  in

  match peek input with
  | _, `Space ->
    junk input;
    consume_non_link_inline_elements ~at_start_of_line:false []
    (* [~at_start_of_line] is [false] here because the preceding token was some
       some markup like '{b', and we didn't move to the next line, so the next
       token will not be the first non-whitespace token on its line. *)

  | _, `Single_newline ->
    junk input;
    consume_non_link_inline_elements ~at_start_of_line:true []

  | l, `Blank_line ->
    (* In case the markup is immediately followed by a blank line, the error
       message printed by the catch-all case below can be confusing, as it will
       suggest that the markup must be followed by a newline (which it is). It
       just must not be followed by two newlines. To explain that clearly,
       handle that case specifically. *)
    Raise.not_allowed
      l
      ~what:(Token.describe `Blank_line)
      ~in_what:(Token.describe parent_markup)

  | l, `Right_brace ->
    junk input;
    [], l

  | _ ->
    if requires_leading_whitespace then
      Raise.must_be_followed_by_whitespace
        parent_markup_location ~what:(Token.print parent_markup)
    else
      consume_non_link_inline_elements ~at_start_of_line:false []



(* {2 Paragraphs} *)

(* Convenient abbreviation for use in patterns. *)
type token_that_begins_a_paragraph_line = [
  | `Word of string
  | `Code_span of string
  | `Begin_style of Model.Comment.style
  | `Simple_reference of string
  | `Begin_reference_with_replacement_text of string
  | `Begin_link_with_replacement_text of string
]

(* Check that the token constructors above actually are all in [Token.t]. *)
let _check_subset : token_that_begins_a_paragraph_line -> Token.t =
  fun t -> (t :> Token.t)

(* Consumes tokens that make up a paragraph.

   A paragraph is a sequence of general inline elements. These include all the
   non-link inline elements (see above), and also cross-references and external
   links.

   Paragraphs end at a blank line, or when another block element begins on a new
   line. Other block elements are verbatim text, code blocks, section headings,
   and lists.

   Because of the significance of newlines, paragraphs are parsed line-by-line.
   The function [paragraph] is called only when the current token is the first
   non-whitespace token on its line, and begins an inline element. [paragraph]
   then parses a line of inline elements. Afterwards, it looks ahead to the next
   line. If that line also begins with an inline element, it parses that line,
   and so on. *)
let paragraph : input -> Grammar.nestable_block_element = fun input ->

  (* Parses a single line of a paragraph, consisting of inline elements. The
     only valid ways to end a paragraph line are with [`End], [`Single_newline],
     [`Blank_line], and [`Right_brace]. Everything else either belongs in the
     paragraph, or signifies an attempt to begin a block element inside a
     paragraph row, which is an error. These errors are currently caught
     elsewhere; the paragraph parser just stops. *)
  let rec paragraph_line
      : Grammar.inline_element list -> Grammar.inline_element list =
      fun acc ->
    match peek input with
    | _, `Space
    | _, `Minus
    | _, `Plus
    | _, `Word _
    | _, `Code_span _
    | _, `Begin_style _ as stream_head ->
      let element = non_link_inline_element stream_head input in
      let element = element.value in
      let acc = (element :> Grammar.inline_element)::acc in
      paragraph_line acc

    | _, `Simple_reference r ->
      junk input;
      let acc = (`Reference (Helpers.read_reference r, []))::acc in
      paragraph_line acc

    | l, (`Begin_reference_with_replacement_text r as parent_markup) ->
      junk input;
      let content, _ =
        delimited_non_link_inline_element_list
          ~parent_markup
          ~parent_markup_location:l
          ~requires_leading_whitespace:false
          input
      in
      if content = [] then
        Raise.cannot_be_empty l ~what:(Token.describe parent_markup);
      let acc = (`Reference (Helpers.read_reference r, content))::acc in
      paragraph_line acc

    | l, (`Begin_link_with_replacement_text u as parent_markup) ->
      junk input;
      let content, _ =
        delimited_non_link_inline_element_list
          ~parent_markup
          ~parent_markup_location:l
          ~requires_leading_whitespace:false
          input
      in
      let acc = (`Link (u, content))::acc in
      paragraph_line acc

    | _ ->
      acc
  in

  (* After each row is parsed, decides whether to parse more rows. *)
  let rec additional_rows
      : Grammar.inline_element list -> Grammar.inline_element list =
      fun acc ->
    match npeek 2 input with
    | (_, `Single_newline)::(_, #token_that_begins_a_paragraph_line)::_ ->
      junk input;
      let acc = `Space::acc in
      let acc = paragraph_line acc in
      additional_rows acc

    | _ ->
      List.rev acc
  in

  let elements_from_first_row = paragraph_line [] in
  `Paragraph (additional_rows elements_from_first_row)



(* {2 Block elements} *)

(* {3 Helper types} *)

(* The interpretation of tokens in the block parser depends on where on a line
   each token appears. The five possible "locations" are:

   - [`At_start_of_line], when only whitespace has been read on the current
     line.
   - [`After_tag], when a valid tag token, such as [@deprecated], has been read,
     and only whitespace has been read since.
   - [`After_shorthand_bullet], when a valid shorthand list item bullet, such as
     [-], has been read, and only whitespace has been read since.
   - [`After_explicit_list_bullet], when a valid explicit bullet, such as [{li],
     has been read, and only whitespace has been read since.
   - [`After_text], when any other valid non-whitespace token has already been
     read on the current line.

   Here are some examples of how this affects the interpretation of tokens:

   - A paragraph can start anywhere except [`After_text] (two paragraphs cannot
     be on the same line, but paragraphs can be nested in just about anything).
   - [`Minus] is interpreted as a list item bullet [`At_start_of_line],
     [`After_tag], and [`After_explicit_list_bullet].
   - Tags are only allowed [`At_start_of_line].

  To track the location accurately, the functions that make up the block parser
  pass explicit [where_in_line] values around and return them.

  In a few cases, [where_in_line] can be inferred from what helper was called.
  For example, the [paragraph] parser always stops on the same line as the last
  significant token that is in the paragraph it consumed, so the location must
  be [`After_text]. *)
type where_in_line = [
  | `At_start_of_line
  | `After_tag
  | `After_shorthand_bullet
  | `After_explicit_list_bullet
  | `After_text
]

(* The block parsing loop, function [block_element_list], stops when it
   encounters certain tokens.

   When it is called for the whole comment, or for in explicit list item
   ([{li foo}]), it can only stop on end of input or a right brace.

   When it is called inside a shorthand list item ([- foo]), it stops on end of
   input, right brace, a blank line (indicating end of shorthand list), plus or
   minus (indicating the start of the next liste item), or a section heading or
   tag, which cannot be nested in list markup.

   The block parser [block_element_list] explicitly returns the token that
   stopped it, with a type more precise than [Token.t stream_head]: if it was
   called for the whole comment or an explicit list item, the stop token will
   have type [stops_at_delimiters stream_head], and if it was called for a
   shorthand list item, the stop token will have type
   [implicit_stop stream_head]. This allows the calling parsers to write precise
   cases for exactly the tokens that might be at the front of the stream after
   the block parser returns. *)
type stops_at_delimiters = [
  | `End
  | `Right_brace
]

type stopped_implicitly = [
  | `End
  | `Blank_line
  | `Right_brace
  | `Minus
  | `Plus
  | Token.section_heading
  | Token.tag
]

(* Ensure that the above two types are really subsets of [Token.t]. *)
let _check_subset : stops_at_delimiters -> Token.t = fun t -> (t :> Token.t)
let _check_subset : stopped_implicitly -> Token.t = fun t -> (t :> Token.t)

(* The different contexts in which the block parser [block_element_list] can be
   called. The block parser's behavior depends somewhat on the context. For
   example, while paragraphs are allowed anywhere, shorthand lists are not
   allowed immediately inside other shorthand lists, while tags are not allowed
   anywhere except at the comment top level.

   Besides telling the block parser how to behave, each context also carries two
   types, which determine the return type of the block parser:

   - The type of blocks the parser returns. Note that [nestable_block_element]
     is included in [block_element]. However, the extra block kinds in
     [block_element] are only allowed at the comment top level.
   - The type of token that the block parser stops at. See discussion above. *)
type ('block, 'stops_at_which_tokens) context =
  | Top_level :
      (Grammar.block_element, stops_at_delimiters) context
  | In_shorthand_list :
      (Grammar.nestable_block_element, stopped_implicitly) context
  | In_explicit_list :
      (Grammar.nestable_block_element, stops_at_delimiters) context
  | In_tag :
      (Grammar.nestable_block_element, Token.t) context

(* This is a no-op. It is needed to prove to the type system that nestable block
   elements are acceptable block elements in all contexts. *)
let accepted_in_all_contexts
    : type block stops_at_which_tokens.
      (block, stops_at_which_tokens) context ->
      Grammar.nestable_block_element ->
        block =
    fun context block ->
  match context with
  | Top_level -> (block :> Grammar.block_element)
  | In_shorthand_list -> block
  | In_explicit_list -> block
  | In_tag -> block

(* {3 The block element loop} *)

(* {2 Block element lists} *)

(* Consumes tokens making up a sequence of block elements. These are:

   - paragraphs,
   - code blocks,
   - verbatim text blocks,
   - lists, and
   - section headings. *)
let rec block_element_list
    : type block stops_at_which_tokens.
      (block, stops_at_which_tokens) context ->
      parent_markup:[< Token.t | `Comment ] ->
      input ->
        block list * stops_at_which_tokens stream_head * where_in_line =
    fun context ~parent_markup input ->

  let rec consume_block_elements
      : parsed_a_title:bool ->
        parsed_a_tag:bool ->
        where_in_line ->
        block list ->
          block list * stops_at_which_tokens stream_head * where_in_line =
      fun ~parsed_a_title ~parsed_a_tag where_in_line acc ->

    let describe token =
      match token with
      | #token_that_begins_a_paragraph_line -> "paragraph"
      | _ -> Token.describe token
    in

    let raise_if_after_text (l, token) =
      if where_in_line = `After_text then
        Raise.must_begin_on_its_own_line l ~what:(describe token)
    in

    let raise_if_after_tags (l, token) =
      if parsed_a_tag then
        let suggestion =
          Printf.sprintf
            "move %s before any tags" (Token.describe token)
        in
        Raise.not_allowed
          l ~what:(describe token) ~in_what:"the tags section" ~suggestion
    in

    let raise_because_not_at_top_level (l, token) =
      let suggestion =
        Printf.sprintf
          "move %s outside of any other markup" (Token.print token)
      in
      Raise.not_allowed
        l
        ~what:(Token.describe token)
        ~in_what:(Token.describe parent_markup)
        ~suggestion
    in



    match peek input with
    (* Terminators: the two tokens that terminate anything. *)
    | _, `End
    | _, `Right_brace as stream_head ->
      (* This little absurdity is needed to satisfy the type system. Without it,
         OCaml is unable to prove that [stream_head] has the right type for all
         possible values of [context]. *)
      begin match context with
      | Top_level ->
        List.rev acc, stream_head, where_in_line
      | In_shorthand_list ->
        List.rev acc, stream_head, where_in_line
      | In_explicit_list ->
        List.rev acc, stream_head, where_in_line
      | In_tag ->
        List.rev acc, stream_head, where_in_line
      end



    (* Whitespace. This can terminate some kinds of block elements. It is also
       necessary to track it to interpret [`Minus] and [`Plus] correctly, as
       well as to ensure that all block elements begin on their own line. *)
    | _, `Space ->
      junk input;
      consume_block_elements ~parsed_a_title ~parsed_a_tag where_in_line acc

    | _, `Single_newline ->
      junk input;
      consume_block_elements ~parsed_a_title ~parsed_a_tag `At_start_of_line acc

    | _, `Blank_line as stream_head ->
      begin match context with
      (* Blank lines terminate shorthand lists ([- foo]). They also terminate
         paragraphs, but the paragraph parser is aware of that internally. *)
      | In_shorthand_list ->
        List.rev acc, stream_head, where_in_line
      (* Otherwise, blank lines are pretty much like single newlines. *)
      | _ ->
        junk input;
        consume_block_elements
          ~parsed_a_title ~parsed_a_tag `At_start_of_line acc
      end



    (* Explicit list items ([{li ...}] and [{- ...}]) can never appear directly
       in block content. They can only appear inside [{ul ...}] and [{ol ...}].
       So, catch those. *)
    | l, (`Begin_list_item _ as token) ->
      let suggestion =
        Printf.sprintf
          "move %s into %s, or use %s"
          (Token.print token)
          (Token.describe (`Begin_list `Unordered))
          (Token.describe (`Minus))
      in
      Raise.not_allowed
        l
        ~what:(Token.describe token)
        ~in_what:(Token.describe parent_markup)
        ~suggestion



    (* Tags. These can appear at the top level only. Also, once one tag is seen,
       the only top-level elements allowed are more tags. *)
    | l, (`Tag tag as token) as stream_head ->
      begin match context with
      (* Tags cannot make sense in an explicit list ([{ul {li ...}}]). *)
      | In_explicit_list ->
        raise_because_not_at_top_level stream_head
      (* If a tag starts at the beginning of a line, it terminates the preceding
         tag and/or the current shorthand list. In this case, return to the
         caller, and let the caller decide how to interpret the tag token. *)
      | In_shorthand_list ->
        if where_in_line = `At_start_of_line then
          List.rev acc, stream_head, where_in_line
        else
          raise_because_not_at_top_level stream_head
      | In_tag ->
        if where_in_line = `At_start_of_line then
          List.rev acc, stream_head, where_in_line
        else
          raise_because_not_at_top_level stream_head

      (* If this is the top-level call to [block_element_list], parse the
         tag. *)
      | Top_level ->
        if where_in_line <> `At_start_of_line then
          Raise.must_begin_on_its_own_line l ~what:(Token.describe token);
        junk input;

        begin match tag with
        | `Author s | `Since s | `Version s | `Canonical s as tag ->
          let s = String.trim s in
          if s = "" then
            Raise.cannot_be_empty l ~what:(Token.describe token);
          let tag =
            match tag with
            | `Author _ -> `Author s
            | `Since _ -> `Since s
            | `Version _ -> `Version s
            | `Canonical _ ->
              let path = Helpers.read_path_longident s in
              let module_ = Helpers.read_mod_longident s in
              `Canonical (path, module_)
          in
          let acc = (`Tag tag)::acc in
          consume_block_elements
            ~parsed_a_title ~parsed_a_tag:true `After_text acc

        | `Deprecated | `Return as tag ->
          let content, _stream_head, where_in_line =
            block_element_list In_tag ~parent_markup:token input in
          let tag =
            match tag with
            | `Deprecated -> `Deprecated content
            | `Return -> `Return content
          in
          let acc = (`Tag tag)::acc in
          consume_block_elements
            ~parsed_a_title ~parsed_a_tag:true where_in_line acc

        | `Param _ | `Raise _ | `Before _ as tag ->
          let content, _stream_head, where_in_line =
            block_element_list In_tag ~parent_markup:token input in
          let tag =
            match tag with
            | `Param s -> `Param (s, content)
            | `Raise s -> `Raise (s, content)
            | `Before s -> `Before (s, content)
          in
          let acc = (`Tag tag)::acc in
          consume_block_elements
            ~parsed_a_title ~parsed_a_tag:true where_in_line acc

        | `See (kind, target) ->
          let content, _stream_head, where_in_line =
            block_element_list In_tag ~parent_markup:token input in
          let acc = (`Tag (`See (kind, target, content)))::acc in
          consume_block_elements
            ~parsed_a_title ~parsed_a_tag:true where_in_line acc
        end
      end



    | _, #token_that_begins_a_paragraph_line as stream_head ->
      raise_if_after_tags stream_head;
      raise_if_after_text stream_head;
      let block = paragraph input in
      let block = accepted_in_all_contexts context block in
      let acc = block::acc in
      consume_block_elements ~parsed_a_title ~parsed_a_tag `After_text acc

    | l, ((`Code_block s | `Verbatim s) as token) as stream_head ->
      raise_if_after_tags stream_head;
      raise_if_after_text stream_head;
      if s = "" then
        Raise.cannot_be_empty l ~what:(Token.describe token);
      junk input;
      let block =
        match token with
        | `Code_block _ -> `Code_block s
        | `Verbatim _ -> `Verbatim s
      in
      let block = accepted_in_all_contexts context block in
      let acc = block::acc in
      consume_block_elements ~parsed_a_title ~parsed_a_tag `After_text acc

    | l, (`Modules s as token) as stream_head ->
      raise_if_after_tags stream_head;
      raise_if_after_text stream_head;

      junk input;

      (* TODO Use some library for a splitting function, or move this out into a
         Util module. *)
      let split_string delimiters s =
        let rec scan_delimiters acc index =
          if index >= String.length s then
            List.rev acc
          else
            if String.contains delimiters s.[index] then
              scan_delimiters acc (index + 1)
            else
              scan_word acc index (index + 1)

        and scan_word acc start_index index =
          if index >= String.length s then
            let word = String.sub s start_index (index - start_index) in
            List.rev (word::acc)
          else
            if String.contains delimiters s.[index] then
              let word = String.sub s start_index (index - start_index) in
              scan_delimiters (word::acc) (index + 1)
            else
              scan_word acc start_index (index + 1)

        in

        scan_delimiters [] 0
      in

      let modules =
        split_string " \t\r\n" s
        |> List.map Helpers.read_mod_longident
      in

      if modules = [] then
        Raise.cannot_be_empty l ~what:(Token.describe token);

      let block = accepted_in_all_contexts context (`Modules modules) in
      let acc = block::acc in
      consume_block_elements ~parsed_a_title ~parsed_a_tag `After_text acc



    | l, (`Begin_list kind as token) as stream_head ->
      raise_if_after_tags stream_head;
      raise_if_after_text stream_head;
      junk input;
      let items = explicit_list_items ~parent_markup:token input in
      if items = [] then
        Raise.cannot_be_empty l ~what:(Token.describe token);
      let block = `List (kind, items) in
      let block = accepted_in_all_contexts context block in
      let acc = block::acc in
      consume_block_elements ~parsed_a_title ~parsed_a_tag `After_text acc



    | l, (`Minus | `Plus as token) as stream_head ->
      raise_if_after_tags stream_head;
      begin match where_in_line with
      | `After_text | `After_shorthand_bullet ->
        Raise.must_begin_on_its_own_line l ~what:(Token.describe token);
      | _ ->
        ()
      end;
      begin match context with
      | In_shorthand_list ->
        List.rev acc, stream_head, where_in_line
      | _ ->
        let items, where_in_line =
          shorthand_list_items stream_head where_in_line input in
        let kind =
          match token with
          | `Minus -> `Unordered
          | `Plus -> `Ordered
        in
        let block = `List (kind, items) in
        let block = accepted_in_all_contexts context block in
        let acc = block::acc in
        consume_block_elements ~parsed_a_title ~parsed_a_tag where_in_line acc
      end



    | l, (`Begin_section_heading (level, label) as token) as stream_head ->
      raise_if_after_tags stream_head;

      begin match context with
      | In_shorthand_list ->
        if where_in_line = `At_start_of_line then
          List.rev acc, stream_head, where_in_line
        else
          raise_because_not_at_top_level stream_head
      | In_explicit_list ->
        raise_because_not_at_top_level stream_head
      | In_tag ->
        raise_because_not_at_top_level stream_head

      | Top_level ->
        if where_in_line <> `At_start_of_line then
          Raise.must_begin_on_its_own_line l ~what:(Token.describe token);

        let parsed_a_title, create_heading =
          match input.sections, level with
          | `No_sections, _ ->
            let message =
              {
                Helpers.start_offset = fst l;
                end_offset = snd l;
                text = "sections not allowed in this comment";
              }
            in
            warning input message;
            parsed_a_title,
            fun _label content ->
              accepted_in_all_contexts
                context (`Paragraph [`Styled (`Bold, content)])

          | `Allow_all_sections, 1 ->
            if parsed_a_title then begin
              let message =
                {
                  Helpers.start_offset = fst l;
                  end_offset = snd l;
                  text = "only one title-level heading is allowed";
                }
              in
              raise_notrace (Helpers.Parse_error message)
            end;
            true,
            fun label content -> `Heading (`Title, label, content)

          | _, _ ->
            let level =
              match level with
              | 2 -> `Section
              | 3 -> `Subsection
              | 4 -> `Subsubsection
              | _ ->
                let message =
                  {
                    Helpers.start_offset = fst l;
                    end_offset = snd l;
                    text =
                      Printf.sprintf "'%i': bad section level (2-4 allowed)"
                        level
                  }
                in
                warning input message;
                if level < 2 then
                  `Section
                else
                  `Subsubsection
            in
            parsed_a_title,
            fun label content -> `Heading (level, label, content)
        in

        junk input;

        let content, _ =
          delimited_non_link_inline_element_list
            ~parent_markup:token
            ~parent_markup_location:l
            ~requires_leading_whitespace:true
            input
        in
        if content = [] then
          Raise.cannot_be_empty l ~what:(Token.describe token);

        let label =
          match label with
          | None -> None
          | Some label ->
            Some
              (Model.Paths.Identifier.Label (input.parent_of_sections, label))
        in

        let heading = create_heading label content in
        let acc = heading::acc in
        consume_block_elements ~parsed_a_title  ~parsed_a_tag `After_text acc
      end
  in

  let where_in_line =
    match context with
    | Top_level -> `At_start_of_line
    | In_shorthand_list -> `After_shorthand_bullet
    | In_explicit_list -> `After_explicit_list_bullet
    | In_tag -> `After_tag
  in

  consume_block_elements
    ~parsed_a_title:false ~parsed_a_tag:false where_in_line []

(* {3 Lists} *)

(* Consumes a sequence of implicit list items. Each one consists of a [`Minus]
   or [`Plus] token, followed by block elements until:

   - a blank line, or
   - a list bullet of the opposite kind (e.g. [`Plus] for a [`Minus] list).

   This function is called when the next token is known to be [`Minus] or
   [`Plus]. It consumes that token, and calls the block element parser (see
   above). That parser returns to [implicit_list_items] only on [`Blank_line],
   [`End], [`Minus] or [`Plus] at the start of a line, or [`Right_brace]. *)
and shorthand_list_items
    : [ `Minus | `Plus ] stream_head ->
      where_in_line ->
      input ->
        (Grammar.nestable_block_element list) list * where_in_line =
    fun ((_, bullet_token) as stream_head) where_in_line input ->

  let rec consume_list_items
      : [> ] stream_head ->
        where_in_line ->
        (Grammar.nestable_block_element list) list ->
          (Grammar.nestable_block_element list) list * where_in_line =
      fun stream_head where_in_line acc ->

    match stream_head with
    | _, `End
    | _, `Right_brace
    | _, `Blank_line
    | _, `Tag _
    | _, `Begin_section_heading _ ->
      List.rev acc, where_in_line

    | l, (`Minus | `Plus as bullet) ->
      if bullet = bullet_token then begin
        junk input;

        let content, stream_head, where_in_line =
          block_element_list In_shorthand_list ~parent_markup:bullet input in
        if content = [] then
          Raise.cannot_be_empty l ~what:(Token.describe bullet);

        let acc = content::acc in
        consume_list_items stream_head where_in_line acc
      end
      else
        List.rev acc, where_in_line
  in

  consume_list_items
    (stream_head :> stopped_implicitly stream_head) where_in_line []

(* Consumes a sequence of explicit list items (starting with '{li ...}' and
   '{-...}', which are represented by [`Begin_list_item _] tokens).

   This function is called immediately after '{ul' or '{ol' ([`Begin_list _]) is
   read. The only "valid" way to exit is by reading a [`Right_brace] token,
   which is consumed.

   Whitespace inside the list, but outside list items, is not significant – this
   parsing function consumes all of it. Otherwise, only list item start tokens
   are accepted. Everything else is an error. *)
and explicit_list_items
    : parent_markup:[< Token.t ] ->
      input ->
        (Grammar.nestable_block_element list) list =
    fun ~parent_markup input ->

  let rec consume_list_items
      : (Grammar.nestable_block_element list) list ->
          (Grammar.nestable_block_element list) list =
      fun acc ->

    match peek input with
    | l, `End ->
      Raise.not_allowed
        l ~what:(Token.describe `End) ~in_what:(Token.describe parent_markup)

    | _, `Right_brace ->
      junk input;
      List.rev acc

    | _, `Space
    | _, `Single_newline
    | _, `Blank_line ->
      junk input;
      consume_list_items acc

    | l, (`Begin_list_item kind as token) ->
      junk input;

      (* '{li', represented by [`Begin_list_item `Li], must be followed by
         whitespace. *)
      if kind = `Li then begin
        match peek input with
        | _, (`Space | `Single_newline | `Blank_line | `Right_brace) ->
          ()
          (* The presence of [`Right_brace] above requires some explanation:

             - It is better to be silent about missing whitespace if the next
               token is [`Right_brace], because the error about an empty list
               item will be generated below, and that error is more important to
               the user.
             - The [`Right_brace] token also happens to include all whitespace
               before it, as a convenience for the rest of the parser. As a
               result, not ignoring it could be wrong: there could in fact be
               whitespace in the concrete syntax immediately after '{li', just
               it is not represented as [`Space], [`Single_newline], or
               [`Blank_line]. *)
        | _ ->
          Raise.must_be_followed_by_whitespace l ~what:(Token.print token)
      end;

      let content, stream_head, _where_in_line =
        block_element_list In_explicit_list ~parent_markup:token input in

      if content = [] then
        Raise.cannot_be_empty l ~what:(Token.describe token);

      begin match stream_head with
      | _, `Right_brace ->
        junk input
      | l', `End ->
        Raise.not_allowed
          l' ~what:(Token.describe `End) ~in_what:(Token.describe token)
      end;

      let acc = content::acc in
      consume_list_items acc

    | l, token ->
      let suggestion =
        match token with
        | `Begin_section_heading _ | `Tag _ ->
          Printf.sprintf "move %s outside the list" (Token.describe token)
        | _ ->
          Printf.sprintf
            "move %s into a list item, %s or %s"
            (Token.describe token)
            (Token.print (`Begin_list_item `Li))
            (Token.print (`Begin_list_item `Dash))
      in
      Raise.not_allowed
        l
        ~what:(Token.describe token)
        ~in_what:(Token.describe parent_markup)
        ~suggestion
  in

  consume_list_items []



(* {2 Entry point} *)

let comment
    ~permissive
    sections
    ~parent_of_sections
    ~offset_to_location
    ~token_stream
    ~accumulated_warnings =

  let input =
    {
      permissive;
      sections;
      offset_to_location;
      token_stream;
      parent_of_sections;
      accumulated_warnings
    }
  in

  let elements, stream_head, _where_in_line =
    block_element_list Top_level ~parent_markup:`Comment input in

  match stream_head with
  | _, `End ->
    elements

  | l, `Right_brace ->
    raise_notrace (Helpers.Parse_error {
      start_offset = fst l;
      end_offset = snd l;
      text = "unpaired '}' (end of markup)"
    })
