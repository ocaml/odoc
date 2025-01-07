%{
  open Writer.Prelude
  let ( let+ ) = Fun.flip Writer.map
  let wrap_location : Lexing.position * Lexing.position -> 'a -> 'a Loc.with_location =
  fun pos value ->
    let location = Loc.of_position pos in
    { location; value }

  let not_empty : 'a list -> bool = function _ :: _ -> true | _ -> false
  let has_content : string -> bool = fun s -> String.length s > 0

  type align_error =
    | Invalid_align (* An invalid align cell *)
    | Not_align (* Not an align cell *)

  (* This could be made a bit more specific by allowing Space elements only at
    the beginning and end *)
  let valid_elements (cell : Ast.inline_element list) : string option =
    let rec go acc = function
      | `Word _ :: _ when Option.is_some acc -> None
      | `Word word :: rest -> go (Some word) rest
      | `Space _ :: rest -> go acc rest
      | _ :: _ -> None
      | [] -> acc
    in
    go None cell

  let valid_word word =
    match String.length word with
    | 0 -> Ok None
    | 1 -> (
        match word.[0] with
        | ':' -> Ok (Some `Center)
        | '-' -> Ok None
        | _ -> Error Not_align)
    | len ->
        if String.for_all (Char.equal '-') (String.sub word 1 (len - 2)) then
          match (word.[0], word.[pred len]) with
          | ':', '-' -> Ok (Some `Left)
          | '-', ':' -> Ok (Some `Right)
          | ':', ':' -> Ok (Some `Center)
          | '-', '-' -> Ok None
          | _ -> Error Invalid_align
        else Error Not_align

  let valid_align_cell (cell : Ast.inline_element Loc.with_location list) =
    match List.map Loc.value cell |> valid_elements with
    | Some word -> valid_word word
    | None -> Error Not_align

  let sequence : ('elt, 'err) result list -> ('elt list, 'err) result =
    fun list ->
      let rec go acc : ('elt, 'err) result list -> ('elt list, 'err) result =
        function
        | Ok x :: xs -> go (x :: acc) xs
        | Error err :: _ -> Error err
        | [] -> Ok (List.rev acc)
      in
      go [] list

  (* NOTE: (@FayCarsons)
    When we get something that doesn't look like an align at all, we check to see if we've gotten
    any valid aligns, if so we assume that the cell being considered is supposed to be an align and treat it as an error,
    otherwise we assume the row is not supposed to be an align row *)
  let valid_align_row (row : Ast.inline_element Loc.with_location list list) :
      (Ast.alignment option list, align_error) result =
    let align, not_align =
      List.map valid_align_cell row
      |> List.partition (function
          | Ok _ | Error Invalid_align -> true
          | _ -> false)
    in
    match (align, not_align) with
    | _ :: _, _ :: _ -> Error Invalid_align
    | _ :: _, [] -> sequence align
    | _ -> Error Not_align

  let to_paragraph :
      Ast.inline_element Loc.with_location list ->
      Ast.nestable_block_element Loc.with_location =
  fun words ->
    let span = Loc.span @@ List.map Loc.location words in
    Loc.at span (`Paragraph words)

  (* Merges inline elements within a cell into a single paragraph element, and tags cells w/ tag *)
  let merged_tagged_row tag : 'a Loc.with_location list list -> 'b =
    List.map (fun elts -> ([ to_paragraph elts ], tag))
  let as_data = merged_tagged_row `Data
  let as_header = merged_tagged_row `Header

  let is_valid_align row = Result.is_ok @@ valid_align_row row

(*

  - If the first row is the alignment row then the rest should be data 
  - Otherwise the first should be the headers, the second align, and the rest data 
  - If there's only one row and it's not the align row, then it's data 
*)

  let construct_table : 
      span:Loc.span -> 
      Ast.inline_element Loc.with_location list list list ->
      Ast.nestable_block_element Loc.with_location = 
    fun ~span grid ->
      match grid with
      | [only_row] -> (
        match valid_align_row only_row with
        | Ok align -> 
          Loc.at span @@ `Table (([[]], Some align), `Light)
        | _ ->
          Loc.at span @@ `Table (([as_data only_row], None), `Light))
      | align :: data when is_valid_align align -> 
        let align = Result.get_ok @@ valid_align_row align in
        Loc.at span @@ `Table ( (List.map as_data data, Some align) , `Light)
      | header :: align :: data when is_valid_align align ->
        let align = Result.get_ok @@ valid_align_row align in
        Loc.at span @@ `Table ((as_header header :: List.map as_data data, Some align), `Light)
      | data -> Loc.at span @@ `Table ((List.map as_data data, None), `Light)

  let unclosed_table
      ?(data :
        Ast.inline_element Loc.with_location list list list Writer.t option)
      ~span
      warning : Ast.nestable_block_element Loc.with_location Writer.t =
    let node =
      match data with
      | Some data ->
          Writer.map
            (fun data -> Loc.at span @@ `Table ((List.map as_data data, None), `Light))
            data
      | None -> 
        let inner = Loc.at span @@ `Table (([], None), `Light) in
        return inner
    in
    Writer.warning warning node

  let media_kind_of_target =
    let open Tokens in
    function Audio -> `Audio | Video -> `Video | Image -> `Image

  let href_of_media =
    let open Tokens in
    function Reference name -> `Reference name | Link uri -> `Link uri

  let split_simple_media Loc.{ location; value = media, target } =
    (Loc.at location media, target)

  let split_replacement_media Loc.{ location; value = media, target, content } =
    (Loc.at location media, target, content)

  let trim_start = function
    | Loc.{value = `Space _; _ } :: xs -> xs
    | xs -> xs

  let paragraph : Ast.inline_element Loc.with_location list -> Ast.nestable_block_element Loc.with_location =
    fun elts ->
      let elts = trim_start elts in
      let span = Loc.span @@ List.map Loc.location elts in
      Loc.at span @@ `Paragraph elts

  let rec inline_element_inner : Ast.inline_element -> string = function 
    | `Space s -> s 
    | `Word s -> s
    | `Styled (_, s) -> children s 
    | `Code_span s -> s
    | `Raw_markup (_, s) -> s
    | `Reference (_, _, s) -> children s
    | `Link (_, s) -> children s
    | `Math_span s -> s
  and children s = 
    List.fold_left 
      (fun acc elt -> acc ^ inline_element_inner (Loc.value elt)) 
      "" 
      s

let legal_module_list : Ast.inline_element Loc.with_location list -> bool = 
  fun xs -> 
    not_empty xs 
    && List.for_all 
      (function 
        | `Word _ | `Space _ -> true 
        | _ -> false) 
      @@ List.map Loc.value xs
%}

%token RIGHT_BRACE "{"
%token RIGHT_CODE_DELIMITER "{["

%token <string> Blank_line
%token <string> Single_newline
%token <string> Space " "

%token <string> Word (* Any space-delmited text *)

%token MINUS "-" 
%token PLUS "+"

%token <Tokens.style> Style "{i" (* or '{b' etc *)

(* or '{C' or '{R', but this syntax has been deprecated and is only kept around so legacy codebases don't break :p *)
%token <Tokens.alignment> Paragraph_style "{L" 

%token MODULES "{!modules:"

%token <Tokens.math> Math_span "{m"
%token <Tokens.math> Math_block "{math"

%token <string option * string> Raw_markup "{%%}"

%token <Tokens.code_block> Code_block "{[]}" (* Self-contained code-block *)
%token <Tokens.code_block> Code_block_with_output "{[][" (* Code block that expects some block elements *)
%token <string> Code_span "[]" 

%token <Tokens.list_kind> List "{ol" (* or '{ul' *)
%token LI "{li" 
%token DASH "{-"

%token TABLE_LIGHT "{t"
%token TABLE_HEAVY "{table"
%token TABLE_ROW "{tr"
%token BAR "|"

%token <Ast.table_cell_kind> Table_cell "{td" (* or '{th' for header *)

(* Where N is an integer *)
%token <int * string option> Section_heading "{N:"

(* Tags *)
%token <string> Author "@author"
%token DEPRECATED "@deprecated"
%token <string> Param "@param"
%token <string> Raise "@raise(s)"
%token RETURN "@return"
%token <Tokens.internal_reference * string> See "@see"
%token <string> Since "@since"
%token <string> Before "@before"
%token <string> Version "@version"
%token <string> Canonical "@canonical"
%token INLINE "@inline" 
%token OPEN "@open" 
%token CLOSED "@closed"
%token HIDDEN "@hidden"

%token <string> Simple_ref "{!" 
%token <string> Ref_with_replacement "{{!" 
%token <string> Simple_link "{:"
%token <string> Link_with_replacement "{{:"
%token <Tokens.media * Tokens.media_target> Media "{(format)!" 
%token <Tokens.media * Tokens.media_target * string> Media_with_replacement "{(format):"  (* where 'format' is audio, video, image *)
%token <string> Verbatim "{v"

%token END

%type <Ast.tag Loc.with_location Writer.t> tag_bare
%type <Ast.tag Loc.with_location Writer.t> tag_with_content
%type <Ast.tag Loc.with_location Writer.t> tag

%start <Ast.t Writer.t> main 

%%

(* UTILITIES *)

(* Utility which wraps the return value of a rule in `Loc.with_location` *)
let locatedM(rule) == inner = rule; { Writer.map (wrap_location $sloc)  inner }
let located(rule) == inner = rule; { wrap_location $sloc inner }
let with_position(rule) == inner = rule; { (inner, $sloc) } 
let position(rule) == _ = rule; { $sloc }
let sequence(rule) == xs = list(rule); { Writer.sequence xs }
let sequence_nonempty(rule) == xs = nonempty_list(rule); { Writer.sequence xs }

let delimited_location(opening, rule, closing) := startpos = located(opening); inner = rule; endpos = located(closing); {
  let span = Loc.delimited startpos endpos in
  Loc.at span inner
} 

let separated_nonempty_sequence(sep, rule) := xs = separated_nonempty_list(sep, rule); { Writer.sequence xs }
let separated_sequence(sep, rule) := xs = separated_list(sep, rule); { Writer.sequence xs } 

(* WHITESPACE *)

let horizontal_whitespace := 
  | ~ = Space; <`Space>

let whitespace := 
  | ~ = horizontal_whitespace; <>
  | ~ = Single_newline; <`Space>

let any_whitespace := 
  | ~ = whitespace; <>
  | ~ = Blank_line; <`Space>

let line_break := 
  | ~ = Single_newline; <>
  | ~ = Blank_line; <>

(* ENTRY *)

let main :=  
  | any_whitespace?; ~ = sequence_nonempty(toplevel); END; <>
  | any_whitespace?; END; { return [] }

let toplevel :=
  | block = nestable_block_element; { (block :> Ast.block_element Loc.with_location Writer.t) }
  | t = tag; { Writer.map (fun loc -> Loc.{ loc with value = `Tag loc.value }) t }
  | ~ = section_heading; <>
  | ~ = toplevel_error; <>

let toplevel_error :=
  | errloc = position(PLUS); whitespace?; { 
    let span = Loc.of_position errloc in
    let warning = 
      let what = Tokens.describe PLUS in 
      Writer.Warning (Parse_error.bad_markup what span) 
    in 
    let as_text = Loc.at span @@ `Word "{" in
    let node = (Loc.same as_text @@ `Paragraph [ as_text ]) in
    Writer.with_warning node warning 
  }
  | errloc = position(MINUS); whitespace?; { 
    let span = Loc.of_position errloc in
    let warning = 
      let what = Tokens.describe MINUS in 
      Writer.Warning (Parse_error.bad_markup what span) 
    in 
    let as_text = Loc.at span @@ `Word "{" in
    let node = (Loc.same as_text @@ `Paragraph [ as_text ]) in
    Writer.with_warning node warning 
  }
  | errloc = position(BAR); whitespace?; { 
    let span = Loc.of_position errloc in
    let warning = 
      let what = Tokens.describe BAR in 
      Writer.Warning (Parse_error.bad_markup what span) 
    in 
    let as_text = Loc.at span @@ `Word "{" in
    let node = (Loc.same as_text @@ `Paragraph [ as_text ]) in
    Writer.with_warning node warning 
  }
  | errloc = position(RIGHT_BRACE); whitespace?; { 
    let span = Loc.of_position errloc in
    let warning = 
      let what = Tokens.describe RIGHT_BRACE in 
      Writer.Warning (Parse_error.bad_markup what span) 
    in 
    let as_text = Loc.at span @@ `Word "{" in
    let node = (Loc.same as_text @@ `Paragraph [ as_text ]) in
    Writer.with_warning node warning 
  }
  | errloc = position(RIGHT_CODE_DELIMITER); { 
    let span = Loc.of_position errloc in
    let warning = 
      let what = Tokens.describe RIGHT_BRACE in 
      Writer.Warning (Parse_error.bad_markup what span) 
    in 
    let as_text = Loc.at span @@ `Word "{" in
    let node = Loc.same as_text @@ `Paragraph [ as_text ] in
    Writer.with_warning node warning 
  }
  | list = list_light; {
    let* list = list in
    let warning = 
      let Loc.{ value; location } = list in
      let what = Tokens.describe @@
        match value with
        | `List (`Ordered, `Light, _) -> Tokens.PLUS
        | `List (`Unordered, `Light, _) -> Tokens.MINUS
        | _ -> assert false (* Unreachable *)
      in 
      Parse_error.should_begin_on_its_own_line ~what location
    in 
    Writer.with_warning (list :> Ast.block_element Loc.with_location) (Writer.Warning warning)
  }
  | illegal_elt = tag; {
      Writer.bind 
        illegal_elt
        (fun illegal_elt -> 
          let should_begin_on_its_own_line = Writer.Warning (
            let what = Tokens.describe_tag illegal_elt.Loc.value in
            Parse_error.should_begin_on_its_own_line ~what illegal_elt.Loc.location) 
          in 
          let illegal_elt = Loc.map (fun t -> `Tag t) illegal_elt in
          let inner = (illegal_elt :> Ast.block_element  Loc.with_location) in
          Writer.with_warning inner should_begin_on_its_own_line) 
    }

(* SECTION HEADING *)

let section_heading := 
  | content = located(Section_heading); children = sequence_nonempty(inline_element(whitespace)); endpos = located(RIGHT_BRACE); {
    let span = Loc.delimited content endpos in
    let (num, title) = content.Loc.value in
    Writer.map (fun c -> Loc.at span @@ `Heading (num, title, trim_start c)) children
  }
  | content = located(Section_heading); endpos = located(RIGHT_BRACE); { 
    let span = Loc.delimited content endpos 
    and (num, title) = content.Loc.value in
    let should_not_be_empty = 
      let what = Tokens.describe @@ Section_heading (num, title) in
      Writer.Warning (Parse_error.should_not_be_empty ~what span)
    in
    let node = Loc.at span @@ `Heading (num, title, []) in
    Writer.with_warning node should_not_be_empty
  }
  | content = with_position(Section_heading); end_pos = position(error); {
    let (num, title), start_pos = content in
    let span = Loc.span @@ List.map Loc.of_position [start_pos; end_pos] in
    let start_pos = fst start_pos and end_pos = snd end_pos in
    let illegal = Writer.InputNeeded (fun input ->
      let err = Loc.extract ~input ~start_pos ~end_pos in
      let in_what = Tokens.describe @@ Section_heading (num, title) in
      Parse_error.illegal ~in_what err span) 
    in
    let inner = Loc.at span @@ `Heading (num, title, []) in
    Writer.with_warning inner illegal
  }
  
(* TAGS *)

let tag := 
  | with_content = tag_with_content; line_break?; { with_content }
  | bare = tag_bare; line_break?; { bare }

let tag_with_content := 
  | ~ = before; <>
  | ~ = raise; <>
  | ~ = see; <>
  | ~ = param; <>
  | ~ = deprecated; <>
  | ~ = return; <>

let return := 
  | startpos = located(RETURN); horizontal_whitespace; children = located(sequence_nonempty(nestable_block_element)); {
    let span = Loc.delimited startpos children in
    Writer.map (fun c -> Loc.at span @@ `Return c) children.Loc.value 
  }
  | pos = located(RETURN); horizontal_whitespace?; {
    return (Loc.same pos @@ `Return [])
  }

let deprecated := 
  | startpos = located(DEPRECATED); horizontal_whitespace; children = located(sequence_nonempty(nestable_block_element)); {
    let span = Loc.delimited startpos children in
    Writer.map (fun c -> Loc.at span @@ `Deprecated c) children.Loc.value 
  }
  | pos = located(DEPRECATED); horizontal_whitespace?;
    { return @@ { pos with Loc.value = `Deprecated [] } }
  | pos = located(DEPRECATED); horizontal_whitespace?; errloc = located(RIGHT_BRACE); {
    let warning = 
      Writer.Warning (Parse_error.unpaired_right_brace @@ errloc.Loc.location)
    in
    Writer.with_warning ({ pos with Loc.value = `Deprecated [] }) warning
  }

let before := 
  | content = located(Before); horizontal_whitespace; children = sequence_nonempty(nestable_block_element); {
    let* children = children in
    let span = Loc.span @@ content.Loc.location :: List.map Loc.location children in
    let inner = Loc.at span @@ `Before (content.Loc.value, children) in
    return inner
  }
  | version = located(Before); {
    return { version with value = `Before (version.Loc.value, []) }
  }

let raise := 
  | exn = located(Raise); horizontal_whitespace; children = located(sequence_nonempty(nestable_block_element)); { 
    let span = Loc.delimited exn children in
    let exn = exn.Loc.value in
    let* children = children.Loc.value in
    let inner = Loc.at span @@ `Raise (exn, children) in
    return inner
  }
  | exn = located(Raise); horizontal_whitespace?; { 
    return { exn with Loc.value = `Raise (exn.Loc.value, []) }
  }

let see := 
  | content = located(See); horizontal_whitespace; children = located(sequence_nonempty(nestable_block_element)); {
    let span = Loc.delimited content children in
    let (kind, href) = content.Loc.value in
    let* children = children.Loc.value in
    let inner = Loc.at span @@ `See (Tokens.to_ast_ref kind, href, children) in
    return inner
  }
  | content = located(See); horizontal_whitespace?; {
    let (kind, href) = content.Loc.value in
    return { content with value = `See (Tokens.to_ast_ref kind, href, []) }
  }

let param := 
  | content = located(Param); horizontal_whitespace; children = sequence_nonempty(nestable_block_element); { 
    let* children = children in
    let span = Loc.span @@ content.Loc.location :: List.map Loc.location children in
    let ident = content.Loc.value in
    let inner = Loc.at span @@ `Param (ident, children) in
    return inner
  }
  | content = located(Param); horizontal_whitespace?; { 
    return { content with Loc.value = `Param (content.Loc.value, []) }
  }

let tag_bare :=
  | content = located(Version); horizontal_whitespace?;
    {
      let Loc.{ value = version; location } = content in
      let what = Tokens.describe (Version version) in
      let warning = 
        Writer.Warning (Parse_error.should_not_be_empty ~what location)
      in
      Writer.ensure has_content warning (return version) 
      |> Writer.map (fun v -> { content with value = `Version v }) 
    }
  | content = located(Since); 
    {
      let Loc.{ value = version; location } = content in
      let what = Tokens.describe (Since version) in
      let warning = 
        Writer.Warning (Parse_error.should_not_be_empty ~what location)
      in
      Writer.ensure has_content warning (return version) 
      |> Writer.map (fun v -> { content with value = `Since v }) 
    }
  | impl = located(Canonical); 
    {
      let what = Tokens.describe @@ Canonical "" in
      let warning = 
        let span = Loc.of_position $sloc in
        Writer.Warning (Parse_error.should_not_be_empty ~what span)
      in
      Writer.ensure (Loc.is has_content) warning @@ return impl 
      |> Writer.map (fun v -> Loc.at impl.Loc.location @@ `Canonical v) 
    }
  | content = located(Author); 
    {
      let Loc.{ value; location } = content in
      let what = Tokens.describe @@ Author value in
      let warning = 
        Writer.Warning (Parse_error.should_not_be_empty ~what location)
      in
      Writer.ensure has_content warning @@ return value
      |> Writer.map (fun a -> { content with value = `Author a }) 
    }
  | pos = position(OPEN); whitespace?; { let loc = Loc.of_position pos in return @@ Loc.at loc `Open }
  | pos = position(INLINE); whitespace?; { let loc = Loc.of_position pos in return @@ Loc.at loc `Inline }
  | pos = position(CLOSED); whitespace?; { let loc = Loc.of_position pos in return @@ Loc.at loc `Closed }
  | pos = position(HIDDEN); whitespace?; { let loc = Loc.of_position pos in return @@ Loc.at loc `Hidden }

(* INLINE ELEMENTS *)

let inline_element(ws) := 
  | ~ = inline_element_without_whitespace; <>
  | s = located(ws); { return s } 

let inline_element_without_whitespace :=
  (* Single token inline elements which are mostly handled in the lexer *)
  | c = located(Code_span); {
    return @@ Loc.map (fun c -> `Code_span c) c
  }
  | m = located(Raw_markup); { return @@ Loc.map (fun m -> `Raw_markup m) m }
  | w = located(Word); { return @@ Loc.map (fun w -> `Word w) w }
  | m = located(Math_span); { 
    let Loc.{ value = Tokens.{ start; content }; location } = m in
    let span = { location with start } in
    return @@ Loc.at span (`Math_span content)
  }
  (* More complex/recursive inline elements should have their own rule *)
  | ~ = style; <>
  | ~ = reference; <>
  | ~ = link; <>

let style := 
  | style = located(Style); children = sequence(inline_element(whitespace)); endpos = located(RIGHT_BRACE); { 
    let span = Loc.delimited style endpos in
    let style = style.Loc.value in
    let warning = 
      let what = Tokens.describe @@ Style style in
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    Writer.ensure not_empty warning children
    |> Writer.map (fun c -> Loc.at span @@ `Styled (Tokens.to_ast_style style, trim_start c)) 
  }
  | style = located(Style); endpos = located(RIGHT_BRACE); {
    let span = Loc.delimited style endpos in
    let style = style.Loc.value in
    let warning = 
      let what = Tokens.describe @@ Style style in
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    let inner = Loc.at span @@ `Styled (Tokens.to_ast_style style, []) in
    Writer.with_warning inner warning
  }
  | style = located(Style); endpos = located(RIGHT_CODE_DELIMITER); {
    let span = Loc.delimited style endpos in
    let style = style.Loc.value in
    let style_desc = Tokens.describe @@ Style style in
    let not_allowed = 
      let what = Tokens.describe RIGHT_CODE_DELIMITER in
      Writer.Warning (Parse_error.not_allowed ~what ~in_what:style_desc span)
    in
    let should_not_be_empty = 
      Writer.Warning (Parse_error.should_not_be_empty ~what:style_desc span) 
    in
    let inner = Loc.at span @@ `Styled (Tokens.to_ast_style style, []) in
    return inner 
    |> Writer.warning not_allowed
    |> Writer.warning should_not_be_empty
  }
  | style = located(Style); endpos = located(END); {
    let span = Loc.delimited style endpos in
    let style = style.Loc.value in
    let warning = 
      let span = Loc.of_position $sloc in
      let in_what = Tokens.describe @@ Style style in
      Writer.Warning (Parse_error.end_not_allowed ~in_what span) 
    in
    let inner = Loc.at span @@ `Styled (Tokens.to_ast_style style, []) in
    Writer.with_warning inner warning
  }

(* LINKS + REFS *)

let reference := 
  | ref_body = located(Simple_ref); children = sequence(inline_element(whitespace)); {
    let+ children = children in
    let startpos = Loc.nudge_start (-2) ref_body.Loc.location in
    let span = Loc.span @@ startpos :: List.map Loc.location children in
    let ref_body = Loc.at startpos ref_body.Loc.value in
    Loc.at span @@ `Reference (`Simple, ref_body, trim_start children)
  }
  | ref_body = located(Ref_with_replacement); children = sequence_nonempty(inline_element(whitespace)); endpos = located(RIGHT_BRACE); { 
    let+ children = children in 
    let startpos = Loc.nudge_map_start (-3) ref_body in
    let ref_body = Loc.same startpos ref_body.Loc.value in
    let span = Loc.delimited startpos endpos in
    Loc.at span @@ `Reference (`With_text, ref_body, trim_start children)
  }
  | ref_body = located(Ref_with_replacement); endpos = located(RIGHT_BRACE); {
    let startpos = Loc.nudge_map_start (-3) ref_body in
    let span = Loc.delimited startpos endpos in
    let node = Loc.at span @@ `Reference (`With_text, ref_body, []) in
    let warning = 
      let what = Tokens.describe @@ Ref_with_replacement (Loc.value ref_body) in
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    Writer.with_warning node warning
  }
  | ref_body = located(Ref_with_replacement); children = sequence_nonempty(inline_element(whitespace))?; endpos = located(END); {
    let Loc.{value = ref_body_value; location} = ref_body in
    let startpos = Loc.nudge_start (-3) location in
    let ref_body : string Loc.with_location = Loc.at startpos ref_body_value in
    let span = Loc.delimited ref_body endpos in
    let not_allowed = 
      let in_what = Tokens.describe (Ref_with_replacement ref_body_value) in
      Writer.Warning (Parse_error.end_not_allowed ~in_what span)
    in
    let* children = Option.value ~default:(return []) children in
    let node = Loc.at span @@ `Reference (`With_text, ref_body, children) in
    Writer.with_warning node not_allowed
  }

let link := 
  | link_body = located(Simple_link); endpos = located(RIGHT_BRACE); 
    { 
      let span = Loc.delimited (Loc.nudge_map_start (-2) link_body) endpos in
      let link_body = link_body.Loc.value in
      let node = Loc.at span @@ `Link (link_body, []) in
      let url = String.trim link_body in
      if "" = url then 
        let what = Tokens.describe @@ Simple_link link_body in
        let warning = 
          Writer.Warning (Parse_error.should_not_be_empty ~what span)
        in
        Writer.with_warning node warning
      else
        return node
    }
  | link_body = located(Link_with_replacement); children = sequence_nonempty(inline_element(whitespace)); endpos = located(RIGHT_BRACE); 
    { 
      let* c = children in 
      let span = Loc.delimited link_body endpos in
      let link_body = link_body.Loc.value in
      let node = Loc.at span @@ `Link (link_body, c) in
      if "" = link_body then
        let what = Tokens.describe @@ Link_with_replacement link_body in
        let warning =  
          Writer.Warning (Parse_error.should_not_be_empty ~what span)
        in
        Writer.with_warning node warning
      else 
        return node
    }
  | link_body = located(Link_with_replacement); endpos = located(RIGHT_BRACE);
    {
      let span = 
        Loc.delimited link_body endpos 
      in
      let link_body = link_body.Loc.value in
      let node = Loc.at span @@ `Link (link_body, []) in
      let what = Tokens.describe @@ Link_with_replacement link_body in
      let warning =
        Writer.Warning (Parse_error.should_not_be_empty ~what span) 
      in
      Writer.with_warning node warning
    }

(* LIST *)

let list_light_item_unordered := 
  | MINUS; horizontal_whitespace; ~ = nestable_block_element; <>
  | horizontal_whitespace; MINUS; item = nestable_block_element; { 
    let warning = 
      let span = Loc.of_position $sloc in
      Writer.Warning (Parse_error.should_begin_on_its_own_line ~what:(Tokens.describe MINUS) span)
    in
    Writer.warning warning item 
  }

let list_light_item_ordered := 
  | PLUS; horizontal_whitespace; ~ = nestable_block_element; <>
  | horizontal_whitespace; PLUS; item = nestable_block_element; { 
    let warning = 
      let span = Loc.of_position $sloc in
      Writer.Warning (Parse_error.should_begin_on_its_own_line ~what:(Tokens.describe PLUS) span)
    in
    Writer.warning warning item 
  }

let list_light := 
  | children = separated_nonempty_sequence(Single_newline, list_light_item_unordered); {
    let* children = children in
    let span = Loc.span @@ List.map Loc.location children in
    let inner = Loc.at span @@ `List (`Unordered, `Light, [ children ]) in
    return inner
  }
  | children = separated_nonempty_sequence(Single_newline, list_light_item_ordered); {
    let* children = children in
    let span = Loc.span @@ List.map Loc.location children in
    let inner = Loc.at span @@ `List (`Ordered, `Light, [ children ]) in
    return inner
  }

let item_heavy :=
    | startpos = located(LI); whitespace; items = sequence(nestable_block_element); endpos = located(RIGHT_BRACE); whitespace?; {
      let span = Loc.delimited startpos endpos in
      let warning = 
        Writer.Warning (Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span) 
      in
      Writer.ensure not_empty warning items 
    }
    | startpos = located(LI); items = sequence(nestable_block_element); endpos = located(RIGHT_BRACE); whitespace?; {
      let span = Loc.delimited startpos endpos in
      let should_be_followed_by_whitespace = 
        Writer.Warning (Parse_error.should_be_followed_by_whitespace ~what:(Tokens.describe LI) span)
      in
      let should_not_be_empty = 
        Writer.Warning (Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span) 
      in
      Writer.ensure not_empty should_not_be_empty items 
      |> Writer.warning should_be_followed_by_whitespace 
    }
    | startpos = located(DASH); whitespace?; items = sequence(nestable_block_element); endpos = located(RIGHT_BRACE); whitespace?; {
      let warning = 
        let span = Loc.delimited startpos endpos in
        Writer.Warning (Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span) 
      in
      Writer.ensure not_empty warning items
    }
    | startpos = located(DASH); whitespace?; items = sequence_nonempty(nestable_block_element)?; endpos = located(END); {
      let end_not_allowed = 
        Writer.Warning (Parse_error.end_not_allowed ~in_what:(Tokens.describe DASH) endpos.Loc.location)
      in
      match items with 
      | Some writer ->
        Writer.warning end_not_allowed writer
      | None ->
        let span = Loc.delimited startpos endpos in
        let should_not_be_empty = 
          Writer.Warning (Parse_error.should_not_be_empty ~what:(Tokens.describe DASH) span) 
        in
        Writer.with_warning [] should_not_be_empty |> Writer.warning end_not_allowed
    }

let list_heavy := 
    | list_kind = located(List); whitespace?; items = sequence_nonempty(item_heavy); endpos = located(RIGHT_BRACE); { 
      let span = Loc.delimited list_kind endpos in
      let* items : Ast.nestable_block_element Loc.with_location list list = items in
      let inner = Loc.at span @@ `List (Tokens.ast_list_kind list_kind.Loc.value, `Heavy, items) in
      return inner
    }
    | list_kind = located(List); endpos = located(RIGHT_BRACE); { 
      let span = Loc.delimited list_kind endpos in
      let warning = 
        let what = Tokens.describe @@ List list_kind.Loc.value in
        Writer.Warning (Parse_error.should_not_be_empty ~what span) 
      in
      let node = Loc.at span @@ `List (Tokens.ast_list_kind list_kind.Loc.value, `Heavy, []) in
      Writer.with_warning node warning
    }
    | list_kind = located(List); whitespace?; items = sequence_nonempty(item_heavy); errloc = position(error); {
      let span = Loc.(span [list_kind.location; Loc.of_position errloc]) in
      let warning = fun input ->
        let (start_pos, end_pos) = errloc in 
        let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
        let in_what = Tokens.describe @@ List list_kind.Loc.value in
        Parse_error.illegal ~in_what illegal_input span 
      in 
      let* items : Ast.nestable_block_element Loc.with_location list list = Writer.warning (Writer.InputNeeded warning) items in
      let inner = Loc.at span @@ `List (Tokens.ast_list_kind list_kind.Loc.value, `Heavy, items) in
      return inner 
    }
    | list_kind = located(List); whitespace?; errloc = position(error); {
      let span = Loc.(span [list_kind.location; Loc.of_position errloc]) in
      let warning = fun input ->
        let (start_pos, end_pos) = errloc in 
        let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
        let in_what = Tokens.describe (List list_kind.Loc.value) in
        Parse_error.illegal ~in_what illegal_input span 
      in
      let inner = Loc.at span @@ `List (Tokens.ast_list_kind list_kind.Loc.value, `Heavy, []) in
      Writer.with_warning inner (Writer.InputNeeded warning)
    }

let odoc_list := 
  | ~ = list_light; <>
  | ~ = list_heavy; <>

(* TABLES *)

let cell_heavy := 
  | cell_kind = Table_cell; whitespace?; children = sequence_nonempty(nestable_block_element); RIGHT_BRACE; whitespace?;
    { Writer.map (fun c -> (c, cell_kind)) children }
  | cell_kind = Table_cell; RIGHT_BRACE; whitespace?;
    { return ([], cell_kind) }
  | cell_kind = Table_cell; children = sequence_nonempty(nestable_block_element)?; errloc = position(error);
    {
      let warning = fun input ->
        let (start_pos, end_pos) as loc = errloc in 
        let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
        let span = Loc.of_position loc in
        let in_what = Tokens.describe @@ Table_cell cell_kind in
        Parse_error.illegal ~in_what illegal_input span 
      in 
      Option.value ~default:(return []) children
      |> Writer.map (fun children -> (children, cell_kind))
      |> Writer.warning (Writer.InputNeeded warning) 
    }

let row_heavy := 
  | TABLE_ROW; whitespace?; ~ = sequence_nonempty(cell_heavy); RIGHT_BRACE; whitespace?; <>
  | TABLE_ROW; whitespace?; RIGHT_BRACE; whitespace?; { return [] }
  | TABLE_ROW; children = sequence_nonempty(cell_heavy)?; errloc = position(error);
    {
      let warning = fun input ->
        let (start_pos, end_pos) as loc = errloc in 
        let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
        let span = Loc.of_position loc in
        let in_what = Tokens.describe TABLE_ROW in
        Parse_error.illegal ~in_what illegal_input span 
      in 
      Option.value ~default:(return []) children
      |> Writer.warning (Writer.InputNeeded warning) 
    }

let table_heavy :=
  | grid = delimited_location(TABLE_HEAVY, whitespace?; sequence_nonempty(row_heavy), RIGHT_BRACE); {
    Writer.map (Loc.map (fun grid -> `Table ((grid, None), `Heavy))) (Writer.sequence_loc grid)
  }
  | startpos = located(TABLE_HEAVY); endpos = located(RIGHT_BRACE); { 
    let span = Loc.(span [startpos.location; endpos.location]) in
    let inner = Loc.at span @@ `Table (([], None), `Heavy) in
    return inner
  }
  | startpos = located(TABLE_HEAVY); whitespace?; grid = sequence_nonempty(row_heavy)?; errloc = position(error);
    {
      let warning = fun input ->
        let (start_pos, end_pos) as loc = errloc in 
        let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
        let span = Loc.of_position loc in
        let in_what = Tokens.describe TABLE_HEAVY in
        Parse_error.illegal ~in_what illegal_input span 
      in 
      let span = Loc.(span [startpos.location; (Loc.of_position errloc)]) in
      Option.value ~default:(return []) grid
      |> Writer.map (fun grid -> Loc.at span @@ `Table ((grid, None), `Heavy))
      |> Writer.warning (Writer.InputNeeded warning) 
    }

(* LIGHT TABLE *)

let table_light_legal_elt := 
  | s = located(horizontal_whitespace); { return s } 
  | ~ = inline_element_without_whitespace; <>

let cell_content_light := ~ = sequence_nonempty(table_light_legal_elt); <>
let cell := 
  | ~ = cell_content_light; <>
  | ~ = cell_content_light; BAR; <>

let cells := BAR?; ~ = sequence_nonempty(cell); <>

let row_light :=
  | ~ = cells; <>
  | ~ = cells; Single_newline; <>

let rows_light := ~ = sequence_nonempty(row_light); <>

let table_start_light := startpos = located(TABLE_LIGHT); whitespace?; { startpos }
let table_light :=
    | startpos = table_start_light; data = rows_light; endpos = located(RIGHT_BRACE); { 
      let span = Loc.delimited startpos endpos in
      Writer.map (construct_table ~span) data 
    }
    | startpos = table_start_light; endpos = located(RIGHT_BRACE); { 
      let span = Loc.delimited startpos endpos in
      let inner = Loc.at span @@ `Table (([[]], None), `Light) in
      return inner
    }
    | startpos = table_start_light; data = rows_light; endpos = located(END); {
      let in_what = Tokens.describe TABLE_LIGHT in
      let warning = 
        let span = Loc.of_position $sloc in
        Writer.Warning (Parse_error.end_not_allowed ~in_what span)
      in
      let span = Loc.delimited startpos endpos in
      unclosed_table ~span ~data warning
    }
    | startpos = located(TABLE_LIGHT); any_whitespace?; endpos = located(END); 
      {
        let in_what = Tokens.describe TABLE_LIGHT in
        let warning = 
          let span = Loc.of_position $sloc in
          Writer.Warning (Parse_error.end_not_allowed ~in_what span) 
        in
        let span = Loc.delimited startpos endpos in
        unclosed_table ~span warning
      }

let table := 
  | ~ = table_heavy; <>
  | ~ = table_light; <>

(* MEDIA *)

(* TODO: Naming needs some work here, multiple fields labeled as `kind` which
   doesn't make sense *)
let media := 
  | media = located(Media); whitespace*; 
    { 
      let (located_media_kind, media_href) = split_simple_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      let kind = media_kind_of_target media_href in
      let inner = Loc.(at media.location @@ `Media (`Simple, wrapped_located_kind, "", kind)) in
      return inner   
    }
  | media = located(Media_with_replacement); whitespace*;
    { 
      let (located_media_kind, media_href, content) = split_replacement_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      let kind = media_kind_of_target media_href in
      let inner = Loc.(at media.location @@ `Media (`With_text, wrapped_located_kind, content, kind)) in
      return inner  
    }

(* TOP-LEVEL ELEMENTS *)

let nestable_block_element := ~ = nestable_block_element_inner; any_whitespace?; <>
let nestable_block_element_inner :=
  | ~ = verbatim; <>
  | ~ = code_block; <> 
  | ~ = odoc_list; <>
  | ~ = table; <> 
  | ~ = media; <>
  | ~ = math_block; <>
  | ~ = paragraph; <>
  | ~ = modules; <> 
  | ~ = paragraph_style; <>

let paragraph_style := content = located(Paragraph_style); ws = paragraph; endpos = located(RIGHT_BRACE); { 
  let span = Loc.delimited content endpos in
  let warning = 
    let what = Tokens.describe @@ Paragraph_style content.Loc.value in
    Writer.Warning (Parse_error.markup_should_not_be_used span ~what)
  in 
  Writer.warning warning ws
}

let verbatim := verbatim = located(Verbatim); { 
  let Loc.{ value; location } = verbatim in
  let what = Tokens.describe @@ Verbatim value in
  let warning =  
    let span = Loc.of_position $sloc in
    Writer.Warning (Parse_error.should_not_be_empty ~what span) 
  in
  let location = Loc.nudge_start (-String.length "{v ") location in
  let verbatim = Loc.at location @@ `Verbatim value in
  Writer.ensure has_content warning (return value) 
  |> Writer.map (Fun.const verbatim)
}

let paragraph := items = sequence_nonempty(inline_element(horizontal_whitespace)); Single_newline?; {
  Writer.map paragraph items
}

let code_block := 
  | content = located(Code_block); {
    return @@ Loc.map (fun Tokens.{metadata; delimiter; content} -> 
      let meta = Option.map (fun Tokens.{language_tag; tags} -> Ast.{ language = language_tag; tags }) metadata in
      `Code_block Ast.{ meta; delimiter; content; output = None }
    ) content
  }
  | content = located(Code_block_with_output); output = sequence_nonempty(nestable_block_element); RIGHT_CODE_DELIMITER; {
    let* output = output in
    return @@ Loc.map (fun Tokens.{metadata; delimiter; content} -> 
      let meta = Option.map (fun Tokens.{language_tag; tags} -> Ast.{ language = language_tag; tags }) metadata in
      `Code_block Ast.{ meta; delimiter; content; output = Some output }
    ) content
  }

let math_block := inner = located(Math_block); {
  let Loc.{ value ; location } = inner in
  let Tokens.{start; content} = value in
  let span = { location with start } in
  let what = Tokens.describe @@ Math_block value in
  let warning = 
    Writer.Warning (Parse_error.should_not_be_empty ~what span) 
  in 
  Writer.ensure has_content warning (return content) 
  |> Writer.map (fun m -> Loc.at span @@ `Math_block m)
}

let modules := startpos = located(MODULES); modules = sequence(inline_element(whitespace)); endpos = located(RIGHT_BRACE); {
    let in_what = Tokens.describe MODULES in
    let* modules = modules in
    let not_allowed =  
      let span = Loc.span @@ List.map Loc.location modules in
      let first_offending = 
        List.find_opt 
          (function 
            | `Word _ | `Space _ -> false 
            | _ -> true) 
          (List.map Loc.value modules : Ast.inline_element list) 
      in
      let what = Option.map Tokens.describe_inline first_offending |> Option.value ~default:String.empty in
      Writer.Warning (Parse_error.not_allowed ~what ~in_what span) 
    in
    let is_empty = 
      let span = Loc.span @@ List.map Loc.location modules in
      let what = Tokens.describe MODULES in
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    let span = Loc.(span [startpos.location; endpos.location]) in
    let inner = Loc.at span @@ `Modules (List.map (Loc.map inline_element_inner) modules) in
    (* Test the content for errors *)
    let* _ = List.fold_left 
        (fun writer (f, w) -> Writer.ensure f w writer) 
        (return modules) 
        [
          (* predicate + error pairs *)
          (not_empty, is_empty); 
          (legal_module_list, not_allowed)
        ]
    in
    return inner
  }
  | startpos = located(MODULES); modules = sequence(inline_element(whitespace)); endpos = located(END); {
    let in_what = Tokens.describe MODULES in
    let* modules = modules in
    let span = Loc.span @@ List.map Loc.location modules in
    let not_allowed = 
      let first_offending = 
        List.find_opt 
          (function 
            | `Word _ | `Space _ -> false 
            | _ -> true) 
          (List.map Loc.value modules : Ast.inline_element list) 
      in
      let what = Option.map Tokens.describe_inline first_offending |> Option.value ~default:String.empty in
      Writer.Warning (Parse_error.not_allowed ~what ~in_what span) 
    in
    let unexpected_end = 
      Writer.Warning (Parse_error.end_not_allowed ~in_what:(Tokens.describe MODULES) span)
    in
    let span = Loc.(span [startpos.location; endpos.location]) in
    let inner = Loc.at span @@ `Modules (List.map (Loc.map inline_element_inner) modules) in
    return inner 
    |> Writer.warning not_allowed
    |> Writer.warning unexpected_end
  }
