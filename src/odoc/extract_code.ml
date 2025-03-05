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

let print line_directives oc location value =
  if line_directives then (
    Printf.fprintf oc "#%d \"%s\"\n" location.Loc.start.line location.file;
    Printf.fprintf oc "%s%s\n"
      (String.v ~len:location.start.column (fun _ -> ' '))
      value)
  else Printf.fprintf oc "%s" value

let rec nestable_block_element line_directives oc names v =
  match v.Loc.value with
  | `Verbatim _ | `Modules _ | `Math_block _ | `Media _ | `Paragraph _ -> ()
  | `Code_block { Ast.content = { value; location }; meta; _ }
    when needs_extraction names meta ->
      print line_directives oc location value
  | `Code_block _ -> ()
  | `List (_, _, l) ->
      List.iter (List.iter (nestable_block_element line_directives oc names)) l
  | `Table ((table, _), _) ->
      List.iter
        (List.iter (fun (x, _) ->
             List.iter (nestable_block_element line_directives oc names) x))
        table

let block_element line_directives oc names v =
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

let pad_loc loc =
  { loc.Location.loc_start with pos_cnum = loc.loc_start.pos_cnum + 3 }

let iterator line_directives oc names =
  let attribute _ attr =
    match Odoc_loader.parse_attribute attr with
    | None | Some (`Stop _ | `Alert _) -> ()
    | Some (`Text (doc, loc) | `Doc (doc, loc)) ->
        let ast_docs =
          Odoc_parser.parse_comment ~location:(pad_loc loc) ~text:doc
        in
        let ast = Odoc_parser.ast ast_docs in
        List.iter (block_element line_directives oc names) ast
  in
  (* For some reason, Tast_iterator.default_iterator does not recurse on
     Tsig_attribute and on attributes... *)
  let signature_item sub sig_ =
    match sig_.Typedtree.sig_desc with
    | Tsig_attribute attr -> attribute sub attr
    | _ -> Tast_iterator.default_iterator.signature_item sub sig_
  in
  let attributes sub attrs = List.iter (attribute sub) attrs in
  { Tast_iterator.default_iterator with attribute; attributes; signature_item }

let load_cmti line_directives oc names input =
  let cmt_info = Cmt_format.read_cmt input in
  match cmt_info.cmt_annots with
  | Interface intf ->
      let iterator = iterator line_directives oc names in
      iterator.signature iterator intf
  | _ -> failwith "TODO"

let load_mld line_directives oc names input =
  let location =
    { Lexing.pos_fname = input; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let c = Io_utils.read_lines input |> String.concat ~sep:"\n" in
  let parsed = parse_comment ~location ~text:c in
  let ast = ast parsed in
  List.iter (block_element line_directives oc names) ast

let extract ~dst ~input ~names ~line_directives =
  let loader =
    match input |> Fpath.v |> Fpath.get_ext with
    | ".mld" -> load_mld
    | ".cmti" -> load_cmti
    | _ -> failwith "TODO"
  in
  match dst with
  | None -> loader line_directives stdout names input
  | Some dst ->
      Io_utils.with_open_out dst @@ fun oc ->
      loader line_directives oc names input
