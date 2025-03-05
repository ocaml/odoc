open Odoc_utils
open Odoc_parser

let tags_included_in_names names tags =
  let fields = String.fields ~empty:false tags in
  List.exists
    (fun tag ->
      match String.cut ~sep:"=" tag with
      | Some ("name", n) -> List.exists (String.equal n) names
      | _ -> false)
    fields

let needs_extraction names meta =
  let check_language () =
    match meta with
    | None -> true
    | Some { Ast.language; _ } -> String.equal "ocaml" language.Loc.value
  in
  let check_name () =
    match meta with
    | Some { Ast.tags = Some tags; _ } ->
        tags_included_in_names names tags.Loc.value
    | _ -> false
  in
  match names with [] -> check_language () | _ :: _ -> check_name ()

let print oc line_directives location value =
  if line_directives then (
    Printf.fprintf oc "#%d \"%s\"\n" (location.Loc.start.line + 1) location.file;
    Printf.fprintf oc "%s%s\n"
      (String.v ~len:(location.start.column + 1) (fun _ -> ' '))
      value)
  else Printf.fprintf oc "%s" value

let rec nestable_block_element line_directives oc names v =
  match v.Loc.value with
  | `Verbatim _ | `Modules _ | `Math_block _ | `Media _ | `Paragraph _ -> ()
  | `Code_block { Ast.content = { value; location }; meta; _ }
    when needs_extraction names meta ->
      print oc line_directives location value
  | `Code_block _ -> ()
  | `List (_, _, l) ->
      List.iter (List.iter (nestable_block_element line_directives oc names)) l
  | `Table ((table, _), _) ->
      List.iter
        (List.iter (fun (x, _) ->
             List.iter (nestable_block_element line_directives oc names) x))
        table

and block_element line_directives oc names v =
  match v.Loc.value with
  | `Tag
      ( `Deprecated l
      | `Param (_, l)
      | `Raise (_, l)
      | `Return l
      | `See (_, _, l)
      | `Before (_, l) ) ->
      List.iter (nestable_block_element line_directives oc names) l
  | `Tag
      ( `Author _ | `Since _ | `Version _ | `Canonical _ | `Inline | `Open
      | `Children_order _ | `Toc_status _ | `Order_category _ | `Short_title _
      | `Closed | `Hidden )
  | `Heading _ ->
      ()
  | #Ast.nestable_block_element as value ->
      nestable_block_element line_directives oc names { v with value }

let extract ~dst ~input ~names ~line_directives =
  let location =
    { Lexing.pos_fname = input; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
  in
  let c = Io_utils.read_lines input |> String.concat ~sep:"\n" in
  let parsed = parse_comment ~location ~text:c in
  let ast = ast parsed in
  let go oc = List.iter (block_element line_directives oc names) ast in
  match dst with None -> go stdout | Some dst -> Io_utils.with_open_out dst go
