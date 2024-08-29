%{
  [@@@warning "-32"]

  open Error
  let point_of_position Lexing.{ pos_lnum; pos_cnum; _ } = 
    Loc.{ line = pos_lnum; column = pos_cnum }

  type lexspan = (Lexing.position * Lexing.position)
  let to_location :  lexspan -> Loc.span =
    fun (start, end_) -> 
      let open Loc in
      let start_point = point_of_position start 
      and end_point = point_of_position end_ in 
      { file = start.pos_fname; start = start_point; end_ = end_point } 

  let wrap_location : type a. lexspan -> a -> a Loc.with_location = fun loc value -> 
    let location = to_location loc in 
    { location; value }
%}

%token SPACE NEWLINE
%token RIGHT_BRACE
%token RIGHT_CODE_DELIMITER
%token COMMENT

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

%token Table_light
%token Table_heavy 
%token Table_row 
%token <Ast.table_cell_kind> Table_cell

%token <int * string option> Section_heading

%token <Parser_types.tag> Tag

%token <string> Simple_ref 
%token <string> Ref_with_replacement 
%token <string> Simple_link 
%token <string> Link_with_replacement
%token <Parser_types.(media * media_target)> Media 
%token <Parser_types.( media * media_target * string )> Media_with_replacement
%token <string> Verbatim

%token END

%start <Ast.t> main 
%%

let main :=  
  | _ = whitespace; { [] }
  | END; { [] }
  | _ = error; { raise @@ Parser_error (wrap_location $sloc (Expecting "UNREACHABLE")) } 

let whitespace := 
  | SPACE; { `Space " " } 
  | NEWLINE; { `Space "\n" }
  | ~ = Space; <`Space>
  | ~ = Blank_line; <`Space>
  | ~ = Single_newline; <`Space>

