(* This exe will compile a markdown file, outputting a compiled `page-x.odoc` file.
   This is tightly coupled with the internal representation of odoc files and thus needs
   to be run with the exact same version of odoc that it is compiled with. *)

open Odoc_model

let parse id input_s =
  let location =
    Lexing.{ pos_fname = input_s; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }
  in
  let str = In_channel.(with_open_bin input_s input_all) in
  let content, parser_warnings =
    Doc_of_md.parse_comment ~location ~text:str ()
  in
  let (content, ()), semantics_warnings =
    Semantics.ast_to_comment ~internal_tags:Expect_none ~tags_allowed:false
      ~parent_of_sections:(id :> Paths.Identifier.LabelParent.t)
      content []
    |> Error.unpack_warnings
  in
  (content, List.map Error.t_of_parser_t parser_warnings @ semantics_warnings)

let mk_page input_s id elements =
  (* Construct the output file representation *)
  let zero_heading = Comment.find_zero_heading elements in
  let frontmatter = Frontmatter.empty in
  let digest = Digest.file input_s in
  let root =
    let file = Root.Odoc_file.create_page input_s zero_heading frontmatter in
    { Root.id = (id :> Paths.Identifier.OdocId.t); file; digest }
  in
  let children = [] in
  {
    Lang.Page.name = id;
    root;
    children;
    content = { elements; warnings_tag = None };
    digest;
    linked = false;
    frontmatter;
  }

let run input_s parent_id_opt odoc_dir =
  (* Construct the id of this page *)
  let page_name = Filename.basename input_s |> Filename.chop_extension in
  let parent_id =
    match parent_id_opt with
    | Some parent_id_str -> Odoc_odoc.Compile.mk_id parent_id_str
    | None -> None
  in
  let id =
    Odoc_model.Paths.Identifier.Mk.leaf_page
      (parent_id, Odoc_model.Names.PageName.make_std page_name)
  in

  let content, warnings = parse id input_s in
  let page = mk_page input_s id content in

  let output =
    let fname = "page-" ^ page_name ^ ".odoc" in
    match parent_id_opt with
    | None -> Fpath.(v odoc_dir / fname)
    | Some parent_id_str -> Fpath.(v odoc_dir // v parent_id_str / fname)
  in
  Odoc_odoc.Odoc_file.save_page output ~warnings page

open Cmdliner

let input =
  let doc = "Input markdown file." in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE" [])

let parent_id =
  let doc =
    "Parent id. This defines both the location of the resulting odoc file as \
     well as the location of the eventual html or other file."
  in
  Arg.(
    value & opt (some string) None & info ~docv:"PARENT" ~doc [ "parent-id" ])

let output_dir =
  let doc =
    "Output file directory. The output file will be put in the parent-id path \
     below this."
  in
  Arg.(
    required & opt (some string) None & info ~docv:"PATH" ~doc [ "output-dir" ])

let cmd =
  let doc = "Compile a markdown file to an odoc page-*.odoc file." in
  let info = Cmd.info "odoc-md" ~doc in
  Cmd.v info Term.(const run $ input $ parent_id $ output_dir)

let () = Cmdliner.(exit @@ Cmd.eval cmd)
