open Odoc_utils
open Odoc_parser

let rec nestable_block_element = function
  | {
      Loc.location = _;
      value = `Verbatim _ | `Modules _ | `Math_block _ | `Media _ | `Paragraph _;
    } ->
      ()
  | {
      location = _;
      value = `Code_block { Ast.content = { value; location }; _ };
    } ->
      Format.printf "#%d \"%s\"\n" (location.start.line + 1) location.file;
      Format.printf "%s"
        (String.v ~len:(location.start.column + 1) (fun _ -> ' '));
      Format.printf "%s" value
  | { location = _; value = `List (_, _, l) } ->
      List.iter (List.iter nestable_block_element) l
  | { location = _; value = `Table ((table, _), _) } ->
      List.iter
        (List.iter (fun (x, _) -> List.iter nestable_block_element x))
        table

and block_element = function
  | {
      Loc.value =
        `Tag
          ( `Deprecated l
          | `Param (_, l)
          | `Raise (_, l)
          | `Return l
          | `See (_, _, l)
          | `Before (_, l) );
      _;
    } ->
      List.iter nestable_block_element l
  | {
      Loc.value =
        `Tag
          ( `Author _ | `Since _ | `Version _ | `Canonical _ | `Inline | `Open
          | `Children_order _ | `Toc_status _ | `Order_category _
          | `Short_title _ | `Closed | `Hidden );
      _;
    }
  | { Loc.value = `Heading _; _ } ->
      ()
  | { Loc.value = #Ast.nestable_block_element; _ } as x ->
      nestable_block_element x

let extract ~dst:_ ~input ~names:_ ~line_directives:_ =
  let location =
    { Lexing.pos_fname = input; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
  in
  let c = Io_utils.read_lines input |> String.concat ~sep:"\n" in
  let parsed = parse_comment ~location ~text:c in
  let ast = ast parsed in
  List.iter block_element ast
