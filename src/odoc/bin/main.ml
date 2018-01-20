(* CR-someday trefis: the "deps" and "targets" subcommands currently output
   their result on stdout.
   It would make the interaction with jenga nicer if we could specify a file to
   output the result to. *)

open Odoc
open Cmdliner

let convert_directory : Fs.Directory.t Arg.converter =
  let (dir_parser, dir_printer) = Arg.dir in
  let odoc_dir_parser str =
    match dir_parser str with
    | `Ok res  -> `Ok (Fs.Directory.of_string res)
    | `Error e -> `Error e
  in
  let odoc_dir_printer fmt dir = dir_printer fmt (Fs.Directory.to_string dir) in
  (odoc_dir_parser, odoc_dir_printer)

let docs = "ARGUMENTS"

let odoc_file_directories =
  let doc =
    "Where to look for required .odoc files. \
     (Can be present several times)."
  in
  Arg.(value & opt_all convert_directory [] @@
    info ~docs ~docv:"DIR" ~doc ["I"])

let hidden =
  let doc =
    "Mark the unit as hidden. \
     (Useful for files included in module packs)."
  in
  Arg.(value & flag @@ info ~docs ~doc ["hidden"])

let dst =
  let doc = "Output dir" (* TODO: improve *) in
  Arg.(required & opt (some convert_directory) None @@
       info ~docs ~docv:"DIR" ~doc ["o"; "output-dir"])

module Compile : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct

  let compile hidden directories resolve_fwd_refs output package_name input =
    let env =
      Env.create ~important_digests:(not resolve_fwd_refs) ~directories
    in
    let input = Fs.File.of_string input in
    let output =
      match output with
      | Some file ->
        let output = Fs.File.of_string file in
        if
          Fs.File.has_ext ".mld" input &&
          not (Astring.String.is_prefix ~affix:"page-" (Filename.basename file))
        then (
          Printf.eprintf "ERROR: the name of the .odoc file produced from a \
                          .mld must start with 'page-'\n%!";
          exit 1
        );
        output
      | None -> Fs.File.(set_ext ".odoc" input)
    in
    Fs.Directory.mkdir_p (Fs.File.dirname output);
    if Fs.File.has_ext ".cmti" input then
      Compile.cmti ~env ~package:package_name ~hidden ~output input
    else if Fs.File.has_ext ".cmt" input then
      Compile.cmt ~env ~package:package_name ~hidden ~output input
    else if Fs.File.has_ext ".cmi" input then
      Compile.cmi ~env ~package:package_name ~hidden ~output input
    else if Fs.File.has_ext ".mld" input then
      Compile.mld ~env ~package:package_name ~output input
    else (
      Printf.eprintf "Unknown extension, expected one of : cmti, cmt, cmi.\n%!";
      exit 2
    )

  let cmd =
    let dst_file =
      let doc = "Output file path. Non-existing intermediate directories are
                 created. If absent outputs a $(i,BASE).odoc file in the same
                 directory as as the input file where $(i,BASE) is the basename
                 of the input file."
      in
      Arg.(value & opt (some string) None @@ info ~docs ~docv:"PATH" ~doc ["o"])
    in
    let input =
      let doc = "Input file (either .cmti or .cmt)" in
      Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"FILE" [])
    in
    let pkg =
      let doc = "Package the input is part of" in
      Arg.(required & opt (some string) None @@
           info ~docs ~docv:"PKG" ~doc ["package"; "pkg"])
    in
    let resolve_fwd_refs =
      let doc = "Try resolving forward references" in
      Arg.(value & flag @@ info ~doc ["r";"resolve-fwd-refs"])
    in
    Term.(const compile $ hidden $ odoc_file_directories $ resolve_fwd_refs $
      dst_file $ pkg $ input)

  let info =
    Term.info ~doc:"Compile a .cmt[i] file to a .odoc file." "compile"
end

module Css = struct
  let copy_default_css output_dir =
    Assets.write ~output_dir

  let cmd = Term.(const copy_default_css $ dst)

  let info =
    Term.info ~doc:"Copies the default odoc.css to the specified directory"
      "css"
end

module Html : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct

  let html semantic_uris closed_details _hidden directories output_dir index_for
        input_file =
    Html.Html_tree.Relative_link.semantic_uris := semantic_uris;
    Html.Html_tree.open_details := not closed_details;
    let env = Env.create ~important_digests:false ~directories in
    let file = Fs.File.of_string input_file in
    match index_for with
    | None -> Html_page.from_odoc ~env ~output:output_dir file
    | Some pkg_name ->
      Html_page.from_mld ~env ~output:output_dir ~package:pkg_name file

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file.odoc" [])
    in
    let semantic_uris =
      let doc = "Generate pretty (semantic) links" in
      Arg.(value & flag (info ~doc ["semantic-uris";"pretty-uris"]))
    in
    let closed_details =
      let doc = "If this flag is passed <details> tags (used for includes) will \
                 be closed by default."
      in
      Arg.(value & flag (info ~doc ["closed-details"]))
    in
    let index_for =
      let doc = "DEPRECATED: you should use 'odoc compile' to process .mld \
		 files. When this argument is given, then the input file is \
		 expected to be a .mld file. The output will be a \
                 \"index.html\" file in the output directory. \
                 PKG is using to correctly resolve and link references inside \
		 the input file"
      in
      Arg.(value & opt (some string) None @@ info ~docv:"PKG" ~doc ["index-for"])
    in
    Term.(const html $ semantic_uris $ closed_details $ hidden $
      odoc_file_directories $ dst $ index_for $ input)

  let info =
    Term.info ~doc:"Generates an html file from an odoc one" "html"
end

module Depends = struct
  module Compile = struct
    let list_dependencies input_file =
      let deps = Depends.for_compile_step (Fs.File.of_string input_file) in
      List.iter ~f:(fun t ->
        Printf.printf "%s %s\n"
          (Depends.Compile.name t)
          (Digest.to_hex @@ Depends.Compile.digest t)
      ) deps;
      flush stdout

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file.cm{i,t,ti}" [])
    in
    Term.(const list_dependencies $ input)

  let info =
    Term.info "compile-deps"
      ~doc:"List units (with their digest) which needs to be compiled in order \
            to compile this one."
  end

  module Html = struct
    let list_dependencies input_file =
      List.iter (Depends.for_html_step (Fs.Directory.of_string input_file))
        ~f:(fun (root : Model.Root.t) ->
          Printf.printf "%s %s %s\n"
            root.package
            (Model.Root.Odoc_file.name root.file)
            (Digest.to_hex root.digest)
        )

    let cmd =
      let input =
        let doc = "Input directory" in
        Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"PKG_DIR" [])
      in
      Term.(const list_dependencies $ input)

  let info =
    Term.info "html-deps"
      ~doc:"lists the packages which need to be in odoc's load path to process \
            html from the .odoc files in the given directory"
  end
end

module Targets = struct
  module Compile = struct
    let list_targets output_dir input_file =
      let input = Fs.File.of_string input_file in
      let targets =
        Targets.for_compile_step ~output:output_dir input
        |> List.map ~f:Fs.File.to_string
      in
      List.iter ~f:(Printf.printf "%s\n") targets;
      flush stdout

    let cmd =
      let input =
        let doc = "Input file" in
        Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file.cm{i,t,ti}" [])
      in
      Term.(const list_targets $ dst $ input)

    let info =
      Term.info "compile-targets" ~doc:"TODO: Fill in."
  end

  module Html = struct
    let list_targets directories output_dir odoc_file =
      let env = Env.create ~important_digests:false ~directories in
      let odoc_file = Fs.File.of_string odoc_file in
      let targets =
        Targets.unit ~env ~output:output_dir odoc_file
        |> List.map ~f:Fs.File.to_string
      in
      Printf.printf "%s\n%!" (String.concat ~sep:"\n" targets)

    let cmd =
      let input =
        let doc = "Input file" in
        Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file.odoc" [])
      in
      Term.(const list_targets $ odoc_file_directories $ dst $ input)

    let info =
      Term.info "html-targets" ~doc:"TODO: Fill in."
  end
end

let () =
  let subcommands =
    [ Compile.(cmd, info)
    ; Html.(cmd, info)
    ; Css.(cmd, info)
    ; Depends.Compile.(cmd, info)
    ; Depends.Html.(cmd, info)
    ; Targets.Compile.(cmd, info)
    ; Targets.Html.(cmd, info)
    ]
  in
  let default =
    let print_default () =
      let available_subcommands =
        List.map subcommands ~f:(fun (_, info) -> Term.name info)
      in
      Printf.printf "Available subcommands: %s\n\
                     See --help for more information.\n%!"
        (String.concat ~sep:", " available_subcommands)
    in
    Term.(const print_default $ const ()),
    Term.info ~version:"%%VERSION%%" "odoc"
  in
  match Term.eval_choice ~err:Format.err_formatter default subcommands with
  | `Error _ ->
    Format.pp_print_flush Format.err_formatter ();
    exit 2
  | _ -> ()
