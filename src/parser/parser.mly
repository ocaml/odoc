%{
  [@@@warning "-32"]

  let point_of_position Lexing.{ pos_lnum; pos_cnum; _ } = 
    Loc.{ line = pos_lnum; column = pos_cnum }

  let to_location : (Lexing.position * Lexing.position) -> 'a -> 'a Loc.with_location =
    fun (start, end_) inner -> 
      let open Loc in
      let start_point = point_of_position start 
      and end_point = point_of_position end_ in 
      let location = { file = start.pos_fname; start = start_point; end_ = end_point } 
      in 
      { location; value = inner }
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

let located(x) == 
  matched = x; { location_of_position $loc matched }

let main :=  
  | _ws = whitespace; { [] }
  | _ = error; { print_endline "Error"; [] }

let whitespace := 
  | SPACE; { [] } | NEWLINE; { [] }
  | _ = Space; { [] }
  | _ = Blank_line; { [ [] ] } 
  | _ = Single_newline; { [] }

