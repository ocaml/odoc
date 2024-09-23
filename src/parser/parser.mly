%{
  open Parser_aux

  let point_of_position Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } = 
      Loc.{ line = pos_lnum; column = pos_cnum - pos_bol }

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

  let pp_tag : Parser_aux.tag -> string = function 
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

  type unimplemented = Top_level_error 
  exception Debug of unimplemented Loc.with_location  
  let _ = Printexc.register_printer (function
    | Debug unimplemented_token_with_location -> 
      begin
        let Loc.{ location = _location; value = token } = unimplemented_token_with_location in 
        let error_message = match token with 
          | Top_level_error -> "Error in Parser.main rule"
        in
        Option.some @@ Printf.sprintf "Parser failed on: %s" error_message
      end 
    | _ -> None
  )
  exception No_children of string Loc.with_location

  let exn_location : only_for_debugging:lexspan -> failed_on:unimplemented -> exn = 
    fun ~only_for_debugging:loc ~failed_on  -> Debug (wrap_location loc failed_on)  

  let tag : Ast.tag -> Ast.block_element = fun tag -> `Tag tag 

  let tag_with_element (children : Ast.nestable_block_element Loc.with_location list) : Parser_aux.tag -> Ast.block_element = function 
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
  (* TODO: replace this with real error handling. Don't throw exceptions *)
  | tag -> raise @@ No_children (wrap_location loc @@ Printf.sprintf "Tag %s expects children" (pp_tag tag)) 

  type align_error = 
  | Invalid_align (* An invalid align cell *)
  | Not_align (* Not an align cell *)

  (* This could be handled in the parser, I think *)
  let valid_align_cell text = 
      begin
        match String.length text with
        | 0 -> Ok None
        | 1 -> 
          begin
            match text.[0] with
            | ':' -> Ok (Some `Center)
            | '-' -> Ok None
            | _ -> Error Not_align
          end
        | len -> 
          if String.for_all (Char.equal '-') (String.sub text 1 (len - 2)) then  
            match text.[0], text.[pred len] with
              | ':', '-' -> Ok (Some `Left)
              | '-', ':' -> Ok (Some `Right)
              | ':', ':' -> Ok (Some `Center)
              | '-', '-' -> Ok None
              | _ -> Error Invalid_align
          else Error Not_align
      end

  (* NOTE: (@FayCarsons) 

     This is a short-circuiting fold, if we get something that isn't a valid alignment then bail and return it.
     When we get something that doesn't look like an align at all, we check to see if we've gotten 
     any valid aligns, if so we assume that the cell being considered is supposed to be an align and treat it as an error,
     otherwise we assume the row is not supposed to be an align row *)
  let rec valid_align_row ?(acc = []) : Ast.inline_element Loc.with_location list -> (Ast.alignment option list, align_error) result
  = function
    | Loc.{ value = `Word cell; _ } :: rest -> 
      begin
        match valid_align_cell cell with
        | Ok alignment -> 
          valid_align_row ~acc:(alignment :: acc) rest 
        | Error Not_align when List.length acc > 0 -> Error Invalid_align
        | Error err -> Error err
      end
    | Loc.{ value = `Space "\n"; _ } :: _ when List.length acc > 0 ->
      Error Invalid_align
    | Loc.{ value = `Space _; _ } :: rest -> 
      valid_align_row ~acc rest
    | [] -> 
      Ok acc
    | _ -> Error Not_align

  (* Wrap a list of words in a paragraph, used for 'light' table headers *)
  let to_paragraph : Ast.inline_element Loc.with_location list -> Ast.nestable_block_element Loc.with_location list 
    = fun words ->
        let span = Loc.span @@ List.map Loc.location words in 
        [ Loc.at span (`Paragraph words) ]

  let recover_data : Ast.inline_element Loc.with_location list -> Ast.nestable_block_element Ast.row 
    = List.map (fun word -> 
        let loc = Loc.location word in 
        [ Loc.at loc @@ `Paragraph [ word ] ], `Data
      )

  let media_kind_of_target = function
    | Audio -> `Audio
    | Video -> `Video
    | Image -> `Image

  let href_of_media = function
    | Reference name -> `Reference name
    | Link uri -> `Link uri

  let split_simple_media Loc.{ location; value = (media, target) } = 
    (Loc.at location media, target)

  let split_replacement_media Loc.{ location; value = (media, target, content) } =
    (Loc.at location media, target, content)

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

%token <Parser_aux.tag> Tag

%token <string> Simple_ref 
%token <string> Ref_with_replacement 
%token <string> Simple_link 
%token <string> Link_with_replacement
%token <(Parser_aux.media * Parser_aux.media_target)> Media 
%token <(Parser_aux.media * Parser_aux.media_target * string)> Media_with_replacement
%token <string> Verbatim

%token END

%start <Ast.t> main 

%%

(* Utility which wraps the return value of a producer in `Loc.with_location` *)
let located(rule) == inner = rule; { wrap_location $sloc inner }

(* ENTRY-POINT *)

let main :=  
  | ~ = located(toplevel)+; END; <>
  | _ = whitespace; END; { [] }
  | END; { [] }
  (* TODO: replace w/ real error handling *)
  | error; { raise @@ exn_location ~only_for_debugging:$loc ~failed_on:Top_level_error }

let toplevel :=
  | ~ = tag; whitespace*; <>
  | block = nestable_block_element; whitespace*; { (block :> Ast.block_element) }
  | ~ = heading; whitespace*; <>

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
  | style = Style; inner = located(inline_element)*; RIGHT_BRACE; { `Styled (style, inner) }
  | ~ = Math_span; <`Math_span>
  | ~ = ref; <>
  | ~ = link; <>

let ref := 
  | ref_body = located(Simple_ref); children = located(inline_element)*; 
    { `Reference (`Simple, ref_body, children) }

  | ref_body = located(Ref_with_replacement); children = located(inline_element)*; 
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
let item_heavy == _ = List_item; whitespace*; ~ = located(nestable_block_element)*; whitespace*; RIGHT_BRACE; whitespace*; <>
let list_heavy := 
    | list_kind = List; whitespace*; items = item_heavy*; whitespace*; RIGHT_BRACE;
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

let cell_light == BAR?; data = located(inline_element)+; { (to_paragraph data, `Data) } 
let row_light := ~ = cell_light+; BAR?; NEWLINE; <>     

let align_cell == BAR?; inner = located(inline_element); { inner }
let align_row := ~ = align_cell+; BAR?; NEWLINE; <>

(* NOTE: (@FayCarsons) Presently, behavior is to 'recover' when we have an invalid align row. Is this what we want? *)
let table_light :=
  (* If the first row is the alignment row then the rest should be data *)
  | TABLE_LIGHT; align = align_row; data = row_light+; RIGHT_BRACE;
    {
      match valid_align_row align with
      | Ok alignment -> (data, Some alignment), `Light
      | Error Invalid_align -> (data, None), `Light
      | Error Not_align ->  
        let align_as_data = recover_data align in
        (align_as_data :: data, None), `Light
    }

  (* Otherwise the first should be the headers, the second align, and the rest data *)
  | TABLE_LIGHT; header = row_light; align = align_row; data = row_light+; RIGHT_BRACE;
    { 
      match valid_align_row align with
      | Ok alignment -> (header :: data, Some alignment), `Light
      | Error Invalid_align -> 
        (header :: data, None), `Light
      | Error Not_align -> 
        let align_as_data = recover_data align in
        (header :: align_as_data :: data, None), `Light
    }

  (* If there's only one row and it's not the align row, then it's data *)
  | TABLE_LIGHT; data = row_light+; RIGHT_BRACE; 
    { (data, None), `Light }

  (* If there's nothing inside, return an empty table *)
  | TABLE_LIGHT; SPACE*; RIGHT_BRACE; { ([[]], None), `Light }

let table := 
  | ~ = table_heavy; <`Table>
  | ~ = table_light; <`Table>

(* MEDIA *)

let media := 
  | media = located(Media); whitespace*; 
    { let (located_media_kind, media_href) = split_simple_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      `Media (`Simple, wrapped_located_kind, "", media_kind_of_target media_href) }
  | media = located(Media_with_replacement); whitespace*;
    { let (located_media_kind, media_href, content) = split_replacement_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      `Media (`With_text, wrapped_located_kind, content, media_kind_of_target media_href) }

(* TOP-LEVEL ELEMENTS *)

let nestable_block_element := 
  | ~ = Verbatim; <`Verbatim>
  | ~ = located(inline_element)+; <`Paragraph>
  | ~ = Code_block; RIGHT_CODE_DELIMITER; <`Code_block>
  | ~ = located(Modules)+; RIGHT_BRACE; <`Modules>
  | ~ = list_element; <>
  | ~ = table; <> 
  | ~ = media; <>
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
