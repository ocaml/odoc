%{
  open Parser_aux
  open Writer.Prelude
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

(* or '{C' or '{R', but this syntax has been deprecated *)
%token <Tokens.alignment Tokens.with_start_point> Paragraph_style "{L" 

%token MODULES "{!modules:"

%token <string Tokens.with_start_point> Math_span "{m"
%token <string Tokens.with_start_point> Math_block "{math"

%token <string Tokens.with_start_point> Verbatim "{v"

%token <(string option * string) Tokens.with_start_point> Raw_markup "{%%}"

%token <Tokens.code_block Tokens.with_start_point> Code_block "{[]}" (* Self-contained code-block *)
%token <Tokens.code_block Tokens.with_start_point> Code_block_with_output "{[][" (* Code block that expects some block elements *)
%token <string Tokens.with_start_point> Code_span "[]" 

%token <Tokens.list_kind> List "{ol" (* or '{ul' *)
%token LI "{li" 
%token DASH "{-"

%token TABLE_LIGHT "{t"
%token TABLE_HEAVY "{table"
%token TABLE_ROW "{tr"
%token BAR "|"

%token <Ast.table_cell_kind> Table_cell "{td" (* or '{th' for header *)

(* Where N is an integer *)
%token <(int * string option) Tokens.with_start_point> Section_heading "{N:"

(* Tags *)
%token <Tokens.tag> Tag
%token <Tokens.tag_with_content> Tag_with_content

(* Links and references *)
%token <string Loc.with_location Tokens.with_start_point> Simple_ref "{!" 
%token <string Loc.with_location Tokens.with_start_point> Ref_with_replacement "{{!" 
%token <string Tokens.with_start_point> Simple_link "{:"
%token <string Tokens.with_start_point> Link_with_replacement "{{:"
%token <(Tokens.media * Tokens.media_target) Tokens.with_start_point> Media "{(format)!" 
%token <(Tokens.media * Tokens.media_target * string) Tokens.with_start_point> Media_with_replacement "{(format):"  (* where 'format' is audio, video, image *)

%token END

%type <Ast.tag Loc.with_location Writer.t> tag_bare
%type <Ast.tag Loc.with_location Writer.t> tag_with_content
%type <Ast.tag Loc.with_location Writer.t> tag

%on_error_reduce nestable_block_element(paragraph)
%on_error_reduce tag_with_content
%on_error_reduce section_heading

%start <Ast.t Writer.t> main 


%%
(* ENTRY *)

(* Consume any leading whitespace *)
let main :=  
  | any_whitespace*; ~ = sequence_nonempty(toplevel); END; <>
  | any_whitespace*; END; { return [] }

let toplevel :=
  | block = nestable_block_element(paragraph); any_whitespace*; { (block :> Ast.block_element Loc.with_location Writer.t) }
  | t = tag; line_break?; { Writer.map ~f:(fun loc -> Loc.{ loc with value = `Tag loc.value }) t }
  | ~ = section_heading; line_break*; <>
  | ~ = toplevel_error; line_break*; <>

(* Tokens which cannot begin any block element *)
let toplevel_error :=
  (* Stray heavy list items, `{li` or `{-` *)
  | err = located(item_open); children = sequence_nonempty(inline_element(horizontal_whitespace)); endpos = located(RIGHT_BRACE)?; {
    let default = 
      Writer.get children 
      |> List.rev 
      |> List.hd 
      |> Loc.map (Fun.const ()) 
    in
    let endloc = Option.value ~default endpos in
    let span = Loc.delimited err endloc in
    let not_allowed = Writer.Warning (
        let what = Tokens.describe err.Loc.value in
        let in_what = "top-level text" in
        let suggestion = 
          Printf.sprintf 
            "Move %s into %s or %s" 
            what 
            (Tokens.describe @@ List Ordered) 
            (Tokens.describe @@ List Unordered) 
        in
        Parse_error.not_allowed ~what ~in_what ~suggestion span) 
    in
    let unclosed = Writer.Warning (
      Parse_error.unclosed_bracket ~bracket:(Tokens.print err.Loc.value) span) 
    in
    Writer.bind children ~f:(fun c -> 
      let inner = { (paragraph c :> Ast.block_element Loc.with_location) with location = span } in
      let m = Writer.return_warning inner not_allowed in
      if Option.is_some endpos then m 
      else 
        Writer.warning unclosed m)
  }
  | errloc = position(RIGHT_BRACE); whitespace?; { 
    let span = Loc.of_position errloc in
    let warning = 
      let what = Tokens.describe RIGHT_BRACE in 
      Writer.Warning (Parse_error.bad_markup what span) 
    in 
    let as_text = Loc.at span @@ `Word "}" in
    let node = (Loc.same as_text @@ `Paragraph [ as_text ]) in
    Writer.return_warning node warning 
  }
  | errloc = position(RIGHT_CODE_DELIMITER); { 
    let span = Loc.of_position errloc in
    let warning = 
      let what = Tokens.describe RIGHT_BRACE in 
      Writer.Warning (Parse_error.bad_markup what span) 
    in 
    let as_text = Loc.at span @@ `Word "{" in
    let node = Loc.same as_text @@ `Paragraph [ as_text ] in
    Writer.return_warning node warning 
  }

(* SECTION HEADING *)

let section_heading := 
  | content = Section_heading; children = sequence_nonempty(inline_element(whitespace)); endpos = located(RIGHT_BRACE); {
    let Tokens.{ inner = (num, title); start } = content in
    let span = { endpos.Loc.location with start } in
    Writer.map ~f:(fun c -> Loc.at span @@ `Heading (num, title, c)) children
  }
  | content = Section_heading; endpos = located(RIGHT_BRACE); { 
    let Tokens.{ inner = (num, title); start } = content in
    let span = { endpos.Loc.location with start } in
    let should_not_be_empty = 
      let what = Tokens.describe @@ Section_heading content in
      Writer.Warning (Parse_error.should_not_be_empty ~what span)
    in
    let node = Loc.at span @@ `Heading (num, title, []) in
    Writer.return_warning node should_not_be_empty
  }
  | content = with_position(Section_heading); end_pos = position(error); {
    let Tokens.{ inner; start }, start_pos = content in
    let (num, title) = inner in
    let end_ = Loc.make_point @@ snd end_pos in
    let file = (fst end_pos).Lexing.pos_fname in
    let span = Loc.{ file; start; end_; } in
    let start_pos = fst start_pos and end_pos = snd end_pos in
    let illegal = Writer.InputNeeded (fun input ->
      let err = Loc.extract ~input ~start_pos ~end_pos in
      let in_what = Tokens.describe @@ Section_heading (fst content) in
      Parse_error.illegal ~in_what err span) 
    in
    let inner = Loc.at span @@ `Heading (num, title, []) in
    Writer.return_warning inner illegal
  }
  
(* TAGS *)

let tag := 
  | with_content = tag_with_content; { with_content }
  | bare = tag_bare; { bare }

let tag_with_content := tag = located(Tag_with_content); children = sequence(nestable_block_element(paragraph)); {
    Writer.map children ~f:(fun children -> 
      let Loc.{ value; location } = tag in
      let start = Tokens.tag_with_content_start_point value |> Option.map (fun start -> { location with start }) |> Option.value ~default:location in
      let span = Loc.span @@ start :: List.map Loc.location children in
      Loc.at span @@ Tokens.tag_with_content children value)
  }
  | tag = located(Tag_with_content); horizontal_whitespace?; {
    return @@ { tag with Loc.value = Tokens.tag_with_content [] tag.Loc.value }
  }
  (* NOTE: (@FayCarsons) Right now this is the only way to accept a newline 
     after a tag_with_content, adding an optional newline causes unsolvable 
     reduce conflicts. 
     Maybe if the line break/whitespace handling for nestable block element were
     refactored, we could remove this *)
  | tag = located(Tag_with_content); Single_newline; children = sequence(nestable_block_element(paragraph)); {
    Writer.map children ~f:(fun children -> 
      let Loc.{ value; location } = tag in
      let start = Tokens.tag_with_content_start_point value |> Option.map (fun start -> { location with start }) |> Option.value ~default:location in
      let span = Loc.span @@ start :: List.map Loc.location children in
      Loc.at span @@ Tokens.tag_with_content children value)
  }

let tag_bare := tag = located(Tag); horizontal_whitespace?; {
  return @@ Loc.map (Fun.const @@ Tokens.tag_bare tag) tag
}

(* INLINE ELEMENTS *)

let inline_element(ws) := 
  | ~ = inline_element_without_whitespace; <>
  | s = located(ws); { return s } 

let inline_element_without_whitespace :=
  (* Single token inline elements which are mostly handled in the lexer *)
  | content = located(Code_span); {
    let Loc.{ value = Tokens.{ inner; start }; location } = content in
    return @@ Loc.at { location with start } (`Code_span inner)
  }
  | m = located(Raw_markup); { 
    let Loc.{ value = Tokens.{ inner; start }; location } = m in
    return @@ Loc.at { location with start } (`Raw_markup inner) 
  }
  | w = located(Word); { return @@ Loc.map (fun w -> `Word w) w }
  | m = located(Math_span); { 
    let Loc.{ value = Tokens.{ start; inner }; location } = m in
    return @@ Loc.at { location with start } (`Math_span inner)
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
    |> Writer.map ~f:(fun c -> Loc.at span @@ `Styled (Tokens.to_ast_style style, trim_start c)) 
  }
  | style = located(Style); endpos = located(RIGHT_BRACE); {
    let span = Loc.delimited style endpos in
    let style = style.Loc.value in
    let warning = 
      let what = Tokens.describe @@ Style style in
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    let inner = Loc.at span @@ `Styled (Tokens.to_ast_style style, []) in
    Writer.return_warning inner warning
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
  | style = located(Style); errloc = position(error); {
    let span = Loc.span [style.Loc.location; Loc.of_position errloc] in
    let illegal = Writer.InputNeeded (fun input ->
      let in_what = Tokens.describe @@ Style style.Loc.value in
      let (start_pos, end_pos) = errloc in
      let illegal_section = Loc.extract ~input ~start_pos ~end_pos in
      Parse_error.illegal ~in_what illegal_section span) 
    in
    let inner = Loc.at span @@ `Styled (Tokens.to_ast_style style.Loc.value, []) in
    Writer.return_warning inner illegal
  }
  | style = located(Style); endpos = located(END); {
    let span = Loc.delimited style endpos in
    let style = style.Loc.value in
    let warning = 
      let in_what = Tokens.describe @@ Style style in
      Writer.Warning (Parse_error.end_not_allowed ~in_what span) 
    in
    let inner = Loc.at span @@ `Styled (Tokens.to_ast_style style, []) in
    Writer.return_warning inner warning
  }

(* LINKS + REFS *)

(* TODO: See comment @ lexer.mll:205 *)
let reference := 
  | ref_body = located(Simple_ref); {
    let Loc.{ value = Tokens.{ inner; start }; location } = ref_body in
    let span = { location with start } in
    return @@ Loc.at span @@ `Reference (`Simple, inner, [])
  }
  | ref_body = Ref_with_replacement; children = sequence_nonempty(inline_element(whitespace)); endpos = located(RIGHT_BRACE); { 
    Writer.bind children ~f:(fun c -> 
      let Tokens.{ inner; start } = ref_body in
      let span = { endpos.Loc.location with start } in
      return @@ Loc.at span @@ `Reference (`With_text, inner, trim_start c))
  }
  | ref_body = Ref_with_replacement; endpos = located(RIGHT_BRACE); {
    let Tokens.{ inner; start } = ref_body in
    let span = { endpos.Loc.location with start } in
    let node = Loc.at span @@ `Reference (`With_text, inner, []) in
    let warning = 
      let what = Tokens.describe @@ Ref_with_replacement ref_body in
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    Writer.return_warning node warning
  }
  | ref_body = Ref_with_replacement; children = sequence_nonempty(inline_element(whitespace))?; endpos = located(END); {
    let Tokens.{ inner; start } = ref_body in 
    let span = { endpos.Loc.location with start } in
    let not_allowed = 
      let in_what = Tokens.describe (Ref_with_replacement ref_body) in
      Writer.Warning (Parse_error.end_not_allowed ~in_what span)
    in
    let* children = Option.value ~default:(return []) children in
    let node = Loc.at span @@ `Reference (`With_text, inner, children) in
    Writer.return_warning node not_allowed
  }

let link := 
  | content = located(Simple_link); { 
    let Loc.{ value = Tokens.{ inner; start }; location } = content in
    let span = { location with start } in
    let node = Loc.at span @@ `Link (inner, []) in
    let url = String.trim inner in
    if "" = url then 
      let what = Tokens.describe @@ Simple_link content.Loc.value in
      let warning = 
        Writer.Warning (Parse_error.should_not_be_empty ~what span)
      in
      Writer.return_warning node warning
    else
      return node
  }
  | content = Link_with_replacement; children = sequence_nonempty(inline_element(whitespace)); endpos = located(RIGHT_BRACE); { 
    Writer.bind children ~f:(fun c -> 
      let Tokens.{ inner; start } = content in
      let span = { endpos.Loc.location with start } in
      let node = Loc.at span @@ `Link (inner, c) in
      if "" = inner then
        let what = Tokens.describe @@ Link_with_replacement content in
        let warning =  
          Writer.Warning (Parse_error.should_not_be_empty ~what span)
        in
        Writer.return_warning node warning
      else 
        return node)
  }
  | content = Link_with_replacement; endpos = located(RIGHT_BRACE); {
    let Tokens.{ inner; start } = content in
    let span = { endpos.Loc.location with start } in
    let node = Loc.at span @@ `Link (inner, []) in
    let what = Tokens.describe @@ Link_with_replacement content in
    let warning =
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    Writer.return_warning node warning
  }

(* LIST *)

let list_light_start := 
  | MINUS; { Tokens.MINUS }
  | PLUS; { Tokens.PLUS }

let light_list_paragraph_item := 
  | ~ = inline_element(whitespace); <>
  | ~ = symbol_as_word(bar); <>
let paragraph_no_list_symbols := horizontal_whitespace?; x = inline_element_without_whitespace; xs = sequence(light_list_paragraph_item); {
  paragraph <$> Writer.map2 ~f:List.cons x xs
}

let list_light_item := 
  | start = located(list_light_start); horizontal_whitespace?; item = nestable_block_element(paragraph_no_list_symbols); {
    let Loc.{ value; location } = start in
    Writer.map ~f:(fun item ->
      (Loc.span [location; item.Loc.location], item)
    ) (light_list_item value <$> item)
  }
  | horizontal_whitespace; start = located(list_light_start); item = nestable_block_element(paragraph_no_list_symbols); {
    let should_begin_on_its_own_line = 
      let span = Loc.of_position $sloc in
      Writer.Warning (Parse_error.should_begin_on_its_own_line ~what:(Tokens.describe MINUS) span)
    in
    let Loc.{ value; location } = start in
    Writer.map ~f:(fun item ->
      (Loc.span [location; item.Loc.location], item)
    ) (light_list_item value <$> item)
    |> Writer.warning should_begin_on_its_own_line
  }

let list_light := 
  | children = sequence_separated_nonempty(whitespace*, list_light_item); {
    Writer.map children ~f:(fun children -> 
      let spans, children = List.split children in
      let span = Loc.span spans in
      let list_kind, children = split_light_list_items children in
      Loc.at span @@ `List (list_kind, `Light,  [ children ]))
  }
  | children = sequence_separated_nonempty(whitespace*, list_light_item); errpos = position(error); {
    Writer.bind children ~f:(fun children -> 
      let spans, children = List.split children in
      
      let start_pos, end_pos = errpos in
      let errloc = Loc.of_position errpos in
      let span = Loc.span (spans @ [errloc]) in
      let list_kind, children = split_light_list_items children in

      let illegal = Writer.InputNeeded (fun input -> 
        let error_text = Loc.extract ~input ~start_pos ~end_pos in 
        let in_what = Tokens.describe @@ 
          match list_kind with `Ordered -> PLUS | `Unordered -> MINUS 
        in
        Parse_error.illegal ~in_what error_text span)
      in
      `List (list_kind, `Light, [ children ])
      |> Loc.at span
      |> return 
      |> Writer.warning illegal)
  }
  | start = located(list_light_start); horizontal_whitespace?; errpos = position(error); {
    let Loc.{ value; location } = start in
    let list_kind = 
      match value with 
      | PLUS -> `Ordered 
      | MINUS -> `Unordered 
      | _ -> assert false (* unreachable *)
    in
    let errloc = Loc.of_position errpos in
    let span = Loc.span [location; errloc] in
    let illegal = Writer.InputNeeded (fun input -> 
      let (start_pos, end_pos) = errpos in
      let error_text = Loc.extract ~input ~start_pos ~end_pos in
      let in_what = Tokens.describe value in
      Parse_error.illegal ~in_what error_text span)
    in
    `List (list_kind, `Light, [])
    |> Loc.at span
    |> return
    |> Writer.warning illegal
  }
  

let item_open := 
  | LI; { Tokens.LI }
  | DASH; { Tokens.DASH } 

let item_heavy :=
| startpos = located(item_open); any_whitespace*; items = sequence(nestable_block_element(paragraph)); any_whitespace*; endpos = located(RIGHT_BRACE); {
    let span = Loc.delimited startpos endpos in
    let should_not_be_empty = 
      Writer.Warning (Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span) 
    in
    Writer.ensure not_empty should_not_be_empty items 
  }
  | startpos = located(item_open); any_whitespace*; items = sequence(nestable_block_element(paragraph))?; any_whitespace*; endpos = located(END); {
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
      Writer.return_warning [] should_not_be_empty 
      |> Writer.warning end_not_allowed
  }

let list_heavy := 
  | list_kind = located(List); whitespace*; items = sequence(item_heavy); whitespace*; endpos = located(RIGHT_BRACE); { 
    let span = Loc.delimited list_kind endpos in
    let should_not_be_empty = 
      let what = Tokens.describe @@ List list_kind.Loc.value in
      Writer.Warning (Parse_error.should_not_be_empty ~what span) 
    in
    Writer.ensure not_empty should_not_be_empty items 
    |> Writer.bind ~f:(fun items -> 
        `List (Tokens.ast_list_kind list_kind.Loc.value, `Heavy, items) 
        |> Loc.at span
        |> return)
  }
  | list_kind = located(List); whitespace*; items = sequence_nonempty(item_heavy); errloc = position(error); {
    let span = Loc.(span [list_kind.location; of_position errloc]) in
    let illegal = Writer.InputNeeded (fun input ->
      let (start_pos, end_pos) = errloc in 
      let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
      let in_what = Tokens.describe @@ List list_kind.Loc.value in
      Parse_error.illegal ~in_what illegal_input span) 
    in 
    let* items : Ast.nestable_block_element Loc.with_location list list = Writer.warning illegal items in
    let inner = Loc.at span @@ `List (Tokens.ast_list_kind list_kind.Loc.value, `Heavy, items) in
    return inner 
  }
  | list_kind = located(List); errloc = position(error); {
    let span = Loc.(span [list_kind.location; of_position errloc]) in
    let illegal = Writer.InputNeeded (fun input ->
      let (start_pos, end_pos) = errloc in 
      let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
      let in_what = Tokens.describe (List list_kind.Loc.value) in
      Parse_error.illegal ~in_what illegal_input span) 
    in
    let inner = Loc.at span @@ `List (Tokens.ast_list_kind list_kind.Loc.value, `Heavy, []) in
    Writer.return_warning inner illegal
  }

let odoc_list := 
  | ~ = list_light; <>
  | ~ = list_heavy; <>

(* TABLES *)

let cell_heavy := 
  | cell_kind = Table_cell; children = sequence_nonempty(nestable_block_element(paragraph)); RIGHT_BRACE; whitespace*;
    { Writer.map ~f:(fun c -> (c, cell_kind)) children }
  | cell_kind = Table_cell; RIGHT_BRACE; whitespace*;
    { return ([], cell_kind) }
  | cell_kind = Table_cell; children = sequence_nonempty(nestable_block_element(paragraph))?; errloc = position(error); {
    let illegal = Writer.InputNeeded (fun input ->
      let (start_pos, end_pos) as loc = errloc in 
      let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
      let span = Loc.of_position loc in
      let in_what = Tokens.describe @@ Table_cell cell_kind in
      Parse_error.illegal ~in_what illegal_input span) 
    in 
    Option.value ~default:(return []) children
    |> Writer.map ~f:(fun c -> (c, cell_kind))
    |> Writer.warning illegal 
  }

let row_heavy := 
  | TABLE_ROW; whitespace*; ~ = sequence_nonempty(cell_heavy); RIGHT_BRACE; whitespace*; <>
  | TABLE_ROW; whitespace*; RIGHT_BRACE; whitespace*; { return [] }
  | TABLE_ROW; children = sequence_nonempty(cell_heavy)?; errloc = position(error); {
    let illegal = Writer.InputNeeded (fun input ->
      let (start_pos, end_pos) as loc = errloc in 
      let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
      let span = Loc.of_position loc in
      let in_what = Tokens.describe TABLE_ROW in
      Parse_error.illegal ~in_what illegal_input span) 
    in 
    Option.value ~default:(return []) children
    |> Writer.warning illegal 
  }

let table_heavy :=
  | grid = delimited_location(TABLE_HEAVY, whitespace*; sequence_nonempty(row_heavy), RIGHT_BRACE); {
    Loc.map (fun grid -> `Table ((grid, None), `Heavy)) <$> (Writer.sequence_loc grid)
  }
  | startpos = located(TABLE_HEAVY); endpos = located(RIGHT_BRACE); { 
    let span = Loc.(span [startpos.location; endpos.location]) in
    let inner = Loc.at span @@ `Table (([], None), `Heavy) in
    return inner
  }
  | startpos = located(TABLE_HEAVY); whitespace*; grid = sequence_nonempty(row_heavy)?; errloc = position(error); {
    let illegal = Writer.InputNeeded (fun input ->
      let (start_pos, end_pos) as loc = errloc in 
      let illegal_input = Loc.extract ~input ~start_pos ~end_pos in
      let span = Loc.of_position loc in
      let in_what = Tokens.describe TABLE_HEAVY in
      Parse_error.illegal ~in_what illegal_input span) 
    in 
    let span = Loc.(span [startpos.location; (Loc.of_position errloc)]) in
    Option.value ~default:(return []) grid
    |> Writer.map ~f:(fun grid -> Loc.at span @@ `Table ((grid, None), `Heavy))
    |> Writer.warning illegal 
  }

(* LIGHT TABLE *)

let cell_inner := 
  | ~ = inline_element(horizontal_whitespace); <> 
  | s = located(symbols_without_bar); {
    return @@ Loc.map (fun w -> `Word w) s
  }
  | (start_pos, end_pos) = position(error); {
    let span = Loc.of_position (start_pos, end_pos) in
    let illegal = Writer.InputNeeded (fun input ->
      let text_span = Loc.extract ~start_pos ~end_pos ~input in 
      Parse_error.illegal ~in_what:(Tokens.describe TABLE_LIGHT) text_span span)
    in
    (* NOTE: (@FayCarsons)
        This is the best we can do right now. Accepting a `nestable_block_element`,
        for example, causes a reduce/reduce conflict. 
        So we have to lose some information(what the invalid element was) via the 
        `error` keyword and return an empty word. 
        Maybe if we refactored the way that block elements/tags/sections handle
        whitespace we could remove the conflict while still being to match on those
        elements
    *)
    Writer.return_warning (Loc.at span @@ `Word "") illegal 
  }

let cell_content_light := ~ = sequence_nonempty(cell_inner); <>

let cell := 
  | ~ = cell_content_light; <>
  | ~ = cell_content_light; BAR; <>
  | BAR; { return [] }

let cells := BAR?; ~ = sequence_nonempty(cell); <>

let row_light :=
  | ~ = cells; <>
  | ~ = cells; Single_newline; <>
  | ~ = cells; Single_newline; Space; <>
  | ~ = cells; Blank_line; <>
  | ~ = cells; Blank_line; Single_newline; <>

let rows_light := ~ = sequence_nonempty(row_light); <>

let table_start_light := startpos = located(TABLE_LIGHT); any_whitespace*; { startpos }
let table_light :=
  | startpos = table_start_light; data = rows_light; endpos = located(RIGHT_BRACE); { 
    let span = Loc.delimited startpos endpos in
    construct_table ~span <$> data 
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
  | startpos = located(TABLE_LIGHT); any_whitespace?; endpos = located(END); {
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

let media := 
  | content = located(Media); whitespace*; { 
    let Loc.{ value = Tokens.{ inner = ref_kind, media_kind; start }; location } = content in
    let span = { location with start } in
    let ref_kind = 
      let open Tokens in
      match ref_kind with 
      | Reference refr -> Loc.map (fun r -> `Reference r) refr
      | Link link -> Loc.map (fun l -> `Link l) link
    in
    let media_kind = 
      let open Tokens in
      match media_kind with 
      | Audio -> `Audio 
      | Image -> `Image
      | Video -> `Video
    in
    let inner = Loc.at span @@ `Media (`Simple, ref_kind, "", media_kind) in 
    return inner
  }
  | content = located(Media_with_replacement); whitespace*; { 
    let Loc.{ value = Tokens.{ inner = (ref_kind, media_kind, content); start }; location } = content in
    let span = { location with start } in
    let ref_kind = 
      let open Tokens in
      match ref_kind with 
      | Reference refr -> Loc.map (fun r -> `Reference r) refr
      | Link link -> Loc.map (fun l -> `Link l) link
    in
    let media_kind = 
      let open Tokens in
      match media_kind with 
      | Audio -> `Audio 
      | Image -> `Image
      | Video -> `Video
    in
    let inner = Loc.at span @@ `Media (`With_text, ref_kind, content, media_kind) in 
    return inner
  }

(* TOP-LEVEL ELEMENTS *)

let nestable_block_element(paragraph) := ~ = nestable_block_element_inner(paragraph); <>
let nestable_block_element_inner(paragraph) :=
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
  let start = content.Loc.value.Tokens.start in
  let endloc = endpos.Loc.location in
  Writer.bind ws ~f:(fun ws -> 
    Writer.return_warning 
      { ws with Loc.location = { endloc with start }} 
      warning)
    
}

let verbatim := verbatim = located(Verbatim); { 
  let Loc.{ value; location } = verbatim in
  let Tokens.{ start; inner } = value in
  let span = { location with start } in
  let what = Tokens.describe @@ Verbatim value in
  let warning =  
    Writer.Warning (Parse_error.should_not_be_empty ~what span) 
  in
  let verbatim = Loc.at span @@ `Verbatim inner in
  Writer.ensure has_content warning (return inner) 
  *> return verbatim
}

(* Split so that we can exclude bar in i.e. light tables *)
let symbols_without_bar := 
  | PLUS; { "+" }
  | MINUS; { "-" }
let bar := 
  | BAR; { "|" }

let symbols := 
  | ~ = symbols_without_bar; <>
  | ~ = bar; <>
let symbol_as_word(symbols) == s = located(symbols); { return @@ Loc.map (fun w -> `Word w) s }
let paragraph_middle_element := 
  | ~ = inline_element(whitespace); <>
  | ~ = symbol_as_word(symbols); <>
  

let paragraph := horizontal_whitespace?; x = inline_element_without_whitespace; xs = sequence(paragraph_middle_element); {
  paragraph <$> Writer.map2 ~f:List.cons x xs
}

let code_block := 
  | content = located(Code_block); {
    let Loc.{ value = Tokens.{ inner; start }; location } = content in
    let Tokens.{ metadata; delimiter; content } = inner in
    let meta = Option.map (fun Tokens.{ language_tag; tags } -> Ast.{ language = language_tag; tags }) metadata in
    let node = `Code_block Ast.{ meta; delimiter; content; output = None } in
    return @@ Loc.at { location with start } node
  }
  | content = located(Code_block_with_output); output = sequence_nonempty(nestable_block_element(paragraph)); RIGHT_CODE_DELIMITER; {
    let* output = Option.some <$> output in
    let Loc.{ value = Tokens.{ inner; start }; location } = content in
    let Tokens.{ metadata; delimiter; content } = inner in
    let meta = Option.map (fun Tokens.{ language_tag; tags } -> Ast.{ language = language_tag; tags }) metadata in
    let node = `Code_block Ast.{ meta; delimiter; content; output } in
    return @@ Loc.at { location with start } node
  }

let math_block := inner = located(Math_block); {
  let Loc.{ value; location } = inner in
  let Tokens.{ start; inner } = value in
  let span = { location with start } in
  let what = Tokens.describe @@ Math_block value in
  let warning = 
    Writer.Warning (Parse_error.should_not_be_empty ~what span) 
  in 
  Writer.ensure has_content warning (return inner) 
  |> Writer.map ~f:(fun m -> Loc.at span @@ `Math_block m)
}

let modules := startpos = located(MODULES); modules = sequence(inline_element(whitespace)); endpos = located(RIGHT_BRACE); {
    let in_what = Tokens.describe MODULES in
    Writer.bind modules ~f:(fun m -> 
      let not_allowed =  
        let span = Loc.span @@ List.map Loc.location m in
        let first_offending = 
          List.find_opt 
            (function 
              | `Word _ | `Space _ -> false 
              | _ -> true) 
            (List.map Loc.value m : Ast.inline_element list) 
        in
        let what = 
          Option.map Tokens.describe_inline first_offending 
          |> Option.value ~default:String.empty 
        in
        Writer.Warning (Parse_error.not_allowed ~what ~in_what span) 
      in
      let is_empty = 
        let span = Loc.span @@ List.map Loc.location m in
        let what = Tokens.describe MODULES in
        Writer.Warning (Parse_error.should_not_be_empty ~what span) 
      in
      let span = Loc.(span [startpos.location; endpos.location]) in
      let inner = Loc.at span @@ `Modules (List.map (Loc.map inline_element_inner) m) in
      (* Test the content for errors, throwing away the value afterwards with `*>` *)
      (Writer.ensure not_empty is_empty (return m) 
       |> Writer.ensure legal_module_list not_allowed) 
       *> return inner)
  }
  | startpos = located(MODULES); modules = sequence(inline_element(whitespace)); endpos = located(END); {
    let in_what = Tokens.describe MODULES in
    Writer.bind modules ~f:(fun m -> 
      let span = Loc.span @@ List.map Loc.location m in
      let not_allowed = 
        let first_offending = 
          List.find_opt 
            (function 
              | `Word _ | `Space _ -> false 
              | _ -> true) 
            (List.map Loc.value m : Ast.inline_element list) 
        in
        let what = Option.map Tokens.describe_inline first_offending |> Option.value ~default:String.empty in
        Writer.Warning (Parse_error.not_allowed ~what ~in_what span) 
      in
      let unexpected_end = 
        Writer.Warning (Parse_error.end_not_allowed ~in_what:(Tokens.describe MODULES) span)
      in
      let span = Loc.(span [startpos.location; endpos.location]) in
      let inner = Loc.at span @@ `Modules (List.map (Loc.map inline_element_inner) m) in
      Writer.return_warning inner not_allowed 
      |> Writer.warning unexpected_end)
  }


(* UTILITIES *)

(* Utilities which wraps the return value of a rule in `Loc.with_location` *)
let locatedM(rule) == inner = rule; { wrap_location $sloc <$> inner }
let located(rule) == inner = rule; { wrap_location $sloc inner }
let delimited_location(opening, rule, closing) := startpos = located(opening); inner = rule; endpos = located(closing); {
  let span = Loc.delimited startpos endpos in
  Loc.at span inner
}

(* When we have to handle errors with Menhir's `error` token, we need 
   `Lexing.position` as opposed to `Loc.with_location`. Because we can cleanly
   take a slice of the input text with those positions, which would be 
   difficult using `Loc.with_location` *)
let with_position(rule) == inner = rule; { (inner, $sloc) } 
let position(rule) == _ = rule; { $sloc }


(* Wrappers around Menhir's built-in utilities that make working inside the 
   Writer.t monad easier *)
let sequence(rule) == xs = list(rule); { Writer.sequence xs }
let sequence_nonempty(rule) == xs = nonempty_list(rule); { Writer.sequence xs }
let sequence_separated_nonempty(sep, rule) := xs = separated_nonempty_list(sep, rule); { Writer.sequence xs }
let sequence_separated(sep, rule) := xs = separated_list(sep, rule); { Writer.sequence xs } 

(* WHITESPACE *)
let horizontal_whitespace == ~ = Space; <`Space>

let whitespace == 
  | ~ = horizontal_whitespace; <>
  | ~ = Single_newline; <`Space>

let any_whitespace := 
  | ~ = whitespace; <>
  | ~ = Blank_line; <`Space>

let line_break == 
  | ~ = Single_newline; <>
  | ~ = Blank_line; <>
