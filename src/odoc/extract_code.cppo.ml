#if OCAML_VERSION >= (4,10,0)
open Odoc_utils
open Odoc_parser

let tags_included_in_names names tags =
  List.exists
    (function
      | `Binding ({ Loc.value = "name"; _ }, { Loc.value = n; _ })
        when List.exists (String.equal n) names ->
          true
      | _ -> false)
    tags

let needs_extraction names meta =
  let check_language () =
    match meta with
    | None -> true
    | Some { Ast.language; _ } -> String.equal "ocaml" language.Loc.value
  in
  let check_name () =
    match meta with
    | Some { Ast.tags; _ } ->
        tags_included_in_names names tags
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
  let default_iterator = Tast_iterator.default_iterator in
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
  let attributes sub attrs = List.iter (attribute sub) attrs in
  (* For some reason, Tast_iterator.default_iterator does not recurse on
     Tsig_attribute and on attributes... *)
  let signature_item sub sig_ =
    match sig_.Typedtree.sig_desc with
    | Tsig_attribute attr -> attribute sub attr
#if defined OXCAML
    | Tsig_include (incl, _) ->
#else
    | Tsig_include incl ->
#endif
        attributes sub incl.incl_attributes
    | Tsig_open o -> attributes sub o.open_attributes
    | _ -> default_iterator.signature_item sub sig_
  in
  let row_field sub rf =
    attributes sub rf.Typedtree.rf_attributes;
    default_iterator.row_field sub rf
  in
  let value_description sub vd =
    attributes sub vd.Typedtree.val_attributes;
    default_iterator.value_description sub vd
  in
  let label_declaration sub lbls =
    List.iter (fun ld -> attributes sub ld.Typedtree.ld_attributes) lbls
  in
  let constructor_declaration sub cd =
    (match cd.Typedtree.cd_args with
    | Cstr_record lds -> label_declaration sub lds
    | _ -> ());
    attributes sub cd.cd_attributes
  in
  let type_kind sub tk =
    (match tk with
    | Typedtree.Ttype_record lbls -> label_declaration sub lbls
    | Ttype_variant cstrs -> List.iter (constructor_declaration sub) cstrs
    | _ -> ());
    default_iterator.type_kind sub tk
  in
  let type_declaration sub decl =
    attributes sub decl.Typedtree.typ_attributes;
    default_iterator.type_declaration sub decl
  in
  let extension_constructor sub ext =
    attributes sub ext.Typedtree.ext_attributes;
    default_iterator.extension_constructor sub ext
  in
  let class_type_field sub ctf =
    attributes sub ctf.Typedtree.ctf_attributes;
    (match ctf.ctf_desc with
    | Tctf_attribute attr -> attribute sub attr
    | _ -> ());
    default_iterator.class_type_field sub ctf
  in
  let class_type_declaration sub ctd =
    attributes sub ctd.Typedtree.ci_attributes;
    default_iterator.class_type_declaration sub ctd
  in
  let class_description sub cd =
    attributes sub cd.Typedtree.ci_attributes;
    default_iterator.class_description sub cd
  in
  (* let type_exception sub exc = *)
  (*   attributes sub ext.Typedtree.ext_attributes; *)
  (*   default_iterator.extension_constructor sub ext *)
  (* in *)
  let type_extension sub ext =
    attributes sub ext.Typedtree.tyext_attributes;
    default_iterator.type_extension sub ext
  in
  let module_type_declaration sub mtd =
    attributes sub mtd.Typedtree.mtd_attributes;
    default_iterator.module_type_declaration sub mtd
  in
  let module_declaration sub md =
    attributes sub md.Typedtree.md_attributes;
    default_iterator.module_declaration sub md
  in
  let module_expr sub me =
    attributes sub me.Typedtree.mod_attributes;
    default_iterator.module_expr sub me
  in
  let module_substitution sub ms =
    attributes sub ms.Typedtree.ms_attributes;
    default_iterator.module_substitution sub ms
  in
  (* let module_type_substitution sub mtd = *)
  (*   attributes sub mtd.Typedtree.mtd_attributes; *)
  (*   default_iterator.module_type_substitution sub ms *)
  (* in *)
  {
    default_iterator with
    row_field
    (* ; attribute *)
    (* ; attributes *);
    value_description;
    signature_item;
    type_kind;
    type_declaration;
    extension_constructor;
    type_extension;
    class_type_field;
    class_type_declaration;
    class_description;
    module_type_declaration;
    module_declaration;
    module_substitution;
    module_expr;
  }

let load_cmti line_directives oc names input ~warnings_options =
  try
    let res =
      Odoc_loader.wrap_errors ~filename:input @@ fun () ->
      let cmt_info = Cmt_format.read_cmt input in
      match cmt_info.cmt_annots with
      | Interface intf ->
          let iterator = iterator line_directives oc names in
          iterator.signature iterator intf;
          Ok ()
      | _ ->
          Error
            (`Msg (Format.sprintf "Provided file %s is not an interface" input))
    in
    Odoc_model.Error.handle_errors_and_warnings ~warnings_options res
    |> Result.join
  with exn ->
    Error
      (`Msg
         (Format.sprintf
            "Error while unmarshalling input file %s:\n\
             %s\n\
             Check that the input file is a valid cmti file"
            input (Printexc.to_string exn)))

let load_mld line_directives oc names input =
  let location =
    { Lexing.pos_fname = input; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let c = Io_utils.read_lines input |> String.concat ~sep:"\n" in
  let parsed = parse_comment ~location ~text:c in
  let ast = ast parsed in
  List.iter (block_element line_directives oc names) ast;
  Ok ()

let extract ~dst ~input ~names ~line_directives ~warnings_options =
  let ( let* ) = Result.bind in
  let* loader =
    match input |> Fpath.v |> Fpath.get_ext with
    | ".mld" -> Ok load_mld
    | ".cmti" -> Ok (load_cmti ~warnings_options)
    | _ -> Error (`Msg "Input must have either mld or cmti as extension")
  in
  match dst with
  | None -> loader line_directives stdout names input
  | Some dst ->
      Io_utils.with_open_out dst @@ fun oc ->
      loader line_directives oc names input

#else

let extract ~dst:_ ~input:_ ~names:_ ~line_directives:_ ~warnings_options:_ =
  Error (`Msg "Extract-code is not available for OCaml < 4.10")

#endif
