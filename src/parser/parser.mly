%{
  [@@@warning "-32"]
  open Parser_types

  let point_of_position Lexing.{ pos_lnum; pos_cnum; _ } = 
    Loc.{ line = pos_lnum; column = pos_cnum }

  type lexspan = (Lexing.position * Lexing.position)
  let to_location :  lexspan -> Loc.span =
    fun (start, end_) -> 
      let open Loc in
      let start_point = point_of_position start 
      and end_point = point_of_position end_ in 
      { file = start.pos_fname; start = start_point; end_ = end_point } 

  let wrap_location : lexspan -> 'a -> 'a Loc.with_location = fun loc value -> 
    let location = to_location loc in 
    { location; value }

  let pp_tag : Parser_types.tag -> string = function 
  | Author _ -> "@author"
  | Deprecated -> "@deprecated"
  | Param _ -> "@param"
  | Raise _ -> "@raise/@raises"
  | Return -> "@return"
  | See _ -> "@see"
  | Since _ -> "@since"
  | Before _ -> "@before"
  | Version _ -> "@version"
  | Canonical _ -> "@canonical"
  | Inline | Open | Closed | Hidden -> "<internal>"

  type unimplemented = Top_level_error | Table | Media
  exception Debug of unimplemented Loc.with_location  
  let _ = Printexc.register_printer (function
    | Debug unimplemented_token_with_location -> 
      begin
        let Loc.{ location = _location; value = token } = unimplemented_token_with_location in 
        let error_message = match token with 
        | Top_level_error -> "Error in Parser.main rule"
        | Table -> "table"
        | Media -> "media" 
        in
        Option.some @@ Printf.sprintf "Parser failed on: %s" error_message
      end 
    | _ -> None
  )
  exception No_children of string Loc.with_location

  let exn_location : only_for_debugging:lexspan -> failed_on:unimplemented -> exn = 
    fun ~only_for_debugging:loc ~failed_on  -> Debug (wrap_location loc failed_on)  

  let tag : Ast.tag -> Ast.block_element = fun tag -> `Tag tag 

  let tag_with_element (children : Ast.nestable_block_element Loc.with_location list) : Parser_types.tag -> Ast.block_element = function 
  | Before version -> tag @@ `Before (version, children) 
  | Deprecated -> tag @@ `Deprecated children
  | Return -> tag @@ `Return children
  | Param param_name -> tag @@ `Param (param_name, children)
  | Raise exn -> tag @@ `Raise (exn, children)
  | See (kind, href) -> tag @@ `See (kind, href, children)
  | _ -> assert false (* Unreachable *)

  let tag_bare loc = function 
  | Version version -> tag @@ `Version version 
  | Since version -> tag @@ `Since version 
  | Canonical implementation -> tag @@ `Canonical (wrap_location loc implementation)
  | Author author -> tag @@ `Author author
  | Inline -> `Tag `Inline 
  | Open -> `Tag `Open 
  | Closed -> `Tag `Closed
  | Hidden -> `Tag `Hidden
  | tag -> raise @@ No_children (wrap_location loc @@ Printf.sprintf "Tag %s expects children" (pp_tag tag)) 

  type align_cell = 
  | Valid of Ast.alignment option 
  | Invalid

  (* This could be handled in the parser, I think *)
  let valid_align_cell text = 
      begin
        match String.length text with
        | 0 -> Valid None
        | 1 -> 
          begin
            match text.[0] with
            | ':' -> Valid (Some `Center)
            | '-' -> Valid None
            | _ -> Invalid
          end
        | len -> 
          if String.for_all (Char.equal '-') (String.sub text 1 (len - 2)) then  
            match text.[0], text.[pred len] with
              | ':', '-' -> Valid (Some `Left)
              | '-', ':' -> Valid (Some `Right)
              | ':', ':' -> Valid (Some `Center)
              | '-', '-' -> Valid (None)
              | _ -> Invalid
          else Invalid
      end

  let rec valid_align_row ?(acc = []) = function
  | Loc.{ value = cell; _ } :: rest -> 
    begin
      match valid_align_cell cell with
      | Valid alignment -> 
        valid_align_row ~acc:(alignment :: acc) rest 
      | Invalid ->
        None
    end
  | [] -> 
    Some acc

  let valid_data_grid (rows : Ast.nestable_block_element Ast.row list) = ( rows :> Ast.nestable_block_element Ast.grid )
    
  (* Wrap a list of words in a paragraph, used for 'light' table headers *)
  let paragraph_of_words : string Loc.with_location list -> Ast.nestable_block_element Loc.with_location list = fun words ->
    let words = List.map (Loc.map (fun text -> `Word text)) words 
    and span = Loc.span @@ List.map Loc.location words in 
     [ Loc.at span ( `Paragraph words) ]
  let recover_data = List.map (fun word -> paragraph_of_words [ word ], `Data)
  let assert_row : ( Ast.nestable_block_element Ast.with_location list * [ `Header | `Data ] ) list ->  ( Ast.nestable_block_element Ast.with_location list * [ `Header | `Data ] ) list = Fun.id
%}

%token SPACE NEWLINE
%token RIGHT_BRACE
%token RIGHT_CODE_DELIMITER

%token <string> Blank_line
%token <string> Single_newline
%token <string> Space

%token <string> Word

%token MINUS PLUS BAR

%token <Ast.style> Style
%token <Ast.alignment> Paragraph_style

%token <string> Modules

%token <string> Math_span 
%token <string> Math_block

%token <string option * string> Raw_markup

%token <Ast.code_block> Code_block
%token <string> Code_span 

%token <Ast.list_kind> List
%token <Ast.list_item> List_item

%token TABLE_LIGHT
%token TABLE_HEAVY 
%token TABLE_ROW 
%token <Ast.table_cell_kind> Table_cell

%token <int * string option> Section_heading

%token <Parser_types.tag> Tag

%token <string> Simple_ref 
%token <string> Ref_with_replacement 
%token <string> Simple_link 
%token <string> Link_with_replacement
%token <Parser_types.(media * media_target)> Media 
%token <Parser_types.(media * media_target * string)> Media_with_replacement
%token <string> Verbatim

%token END

%start <Ast.t> main 

%%

(* Utility which wraps the return value of a producer in `Loc.with_location` *)
let located(rule) == value = rule; { wrap_location $loc value }

(* ENTRY-POINT *)

let main :=  
  | ~ = located(toplevel)+; END; <>
  | _ = whitespace; { [] }
  | END; { [] }
  | error; { raise @@ exn_location ~only_for_debugging:$loc ~failed_on:Top_level_error }

let toplevel :=
  | ~ = tag; <>
  | block = nestable_block_element; { (block :> Ast.block_element) }
  | ~ = heading; <>

let whitespace := 
  | SPACE; { `Space " " } 
  | NEWLINE; { `Space "\n" }
  | ~ = Space; <`Space>
  | ~ = Blank_line; <`Space>
  | ~ = Single_newline; <`Space>

(* INLINE ELEMENTS *)

let inline_element := 
  | ~ = whitespace; <>
  | ~ = Word; <`Word>
  | ~ = Code_span; <`Code_span>
  | ~ = Raw_markup; <`Raw_markup>
  | style = Style; inner = located(inline_element)*; { `Styled (style, inner) }
  | ~ = Math_span; <`Math_span>
  | ~ = ref; <>
  | ~ = link; <>

let ref := 
  | ref_body = located(Simple_ref); children = located(inline_element)+; RIGHT_BRACE; 
    { `Reference (`Simple, ref_body, children) }

  | ref_body = located(Ref_with_replacement); children = located(inline_element)*; RIGHT_BRACE; 
    { `Reference (`With_text, ref_body, children) }

(* TODO : Fix the `with_replacement` producers in the following two rules, if they're broken. Ask what `with_replacement` refers to *)
let link := 
  | link_body = Simple_link; children = located(inline_element)+; RIGHT_BRACE; { `Link (link_body, children) }
  | link_body = Link_with_replacement; children = located(inline_element)+; RIGHT_BRACE; { `Link (link_body, children) }

(* LIST *)

let list_light := 
  | MINUS; unordered_items = separated_list(NEWLINE; SPACE?; MINUS, located(nestable_block_element)); 
    { `List (`Unordered, `Light, [ unordered_items ]) }

  | PLUS; ordered_items = separated_list(NEWLINE; SPACE?; PLUS, located(nestable_block_element)); 
    { `List (`Ordered, `Light, [ ordered_items ]) }

(* `List_item` is [ `Li | `Dash ], not sure how that's useful though. Can't find '{li' syntax in Odoc docs *)
let item_heavy == _ = List_item; ~ = located(nestable_block_element)*; RIGHT_BRACE; <>
let list_heavy := 
  | list_kind = List; items = item_heavy*; RIGHT_BRACE;
    { `List (list_kind, `Heavy, items) }

let list_element := 
  | ~ = list_light; <>
  | ~ = list_heavy; <>

(* TABLES *)

let cell_heavy := cell_kind = Table_cell; children = located(nestable_block_element)*; RIGHT_BRACE; { (children, cell_kind) }
let row_heavy == TABLE_ROW; cells = list(cell_heavy); RIGHT_BRACE;  { cells } 
let table_heavy == TABLE_HEAVY; grid = row_heavy*; RIGHT_BRACE; { 
    (* Convert into an 'abstract table' which can be either a light or heavy syntax table. 
       We know this is a heavy table, which cannot have alignment, however, so the alignment field is `None` *)
    let abstract : Ast.nestable_block_element Ast.abstract_table = (grid, None) in 
    (abstract, `Heavy) 
  }

(* 
  cell -> nestable_block_element with_location list * Header | Data
  row -> cell list 
  grid -> row list
  abstract_table -> grid * alignment option list option

  table -> abstract_table * Light | Heavy 
*)

let data_cell_light == BAR?; data = located(nestable_block_element)+; { (data, `Data) } 
let data_row_light := row = data_cell_light+; BAR?; NEWLINE; { row }     

let align_cell == BAR?; inner = located(Word); { inner }
let align_row := ~ = align_cell+; BAR?; NEWLINE; <>

let header_cell_light == BAR?; inner = located(Word)+; { paragraph_of_words inner, `Header }
let header_row_light := header_row = header_cell_light+; BAR?; NEWLINE; { header_row }

(* NOTE: (@FayCarsons) Presently, behavior is to 'recover' when we have an invalid align row. Is this what we want? *)
let table_light :=
  (* If the first row is the alignment row then the rest should be data *)
  | TABLE_LIGHT; align = align_row; data_rows = data_row_light+; RIGHT_BRACE;
    {
      match valid_align_row align with
      | Some _ as alignment -> (data_rows, alignment), `Light
      | None ->  
        let align_as_data = recover_data align in
        (align_as_data :: data_rows, None), `Light
    }

  (* Otherwise the first should be the headers, the second align, and the rest data *)
  | TABLE_LIGHT; header = header_row_light; align = align_row; data = data_row_light+; RIGHT_BRACE;
    { 
      let grid = valid_data_grid data in
      match valid_align_row align with
      | Some _ as alignment -> (header :: grid, alignment), `Light
      | None -> 
        let align_as_data = recover_data align in
        (header :: align_as_data :: data, None), `Light
    }

  (* If there's only one row and it's not the align row, then it's data *)
  | TABLE_LIGHT; data = data_row_light+; RIGHT_BRACE; 
    { (data, None), `Light }

let table := 
  | ~ = table_heavy; <`Table>
  | ~ = table_light; <`Table>

(* TOP-LEVEL ELEMENTS *)

let nestable_block_element := 
  | ~ = Verbatim; <`Verbatim>
  | ~ = located(inline_element)+; <`Paragraph>
  | ~ = Code_block; RIGHT_CODE_DELIMITER; <`Code_block>
  | ~ = located(Modules)+; RIGHT_BRACE; <`Modules>
  | ~ = list_element; <>
  | ~ = table; <> 
  | _ = Media; { raise @@ exn_location ~only_for_debugging:$loc ~failed_on:Media }
  | ~ = Math_block; <`Math_block>

let heading := 
  | (num, title) = Section_heading; children = list(located(inline_element)); RIGHT_BRACE; {
      `Heading (num, title, children) :> Ast.block_element
    }

let tag := 
  | inner_tag = Tag; children = located(nestable_block_element)+; { tag_with_element children inner_tag }
  | inner_tag = Tag; { tag_bare $loc inner_tag }

let style := ~ = Style; <>
let paragraph_style := ~ = Paragraph_style; <>
