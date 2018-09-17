(* CR-someday trefis: the "deps" and "targets" subcommands currently output
   their result on stdout.
   It would make the interaction with jenga nicer if we could specify a file to
   output the result to. *)

open Odoc
open Cmdliner

let convert_syntax : Html.Html_tree.syntax Arg.converter =
  let open Html.Html_tree in
  let syntax_parser str =
    match str with
  | "ml" | "ocaml" -> `Ok OCaml
  | "re" | "reason" -> `Ok Reason
  | s -> `Error (Printf.sprintf "Unknown syntax '%s'" s)
  in
  let syntax_printer fmt syntax =
    Format.pp_print_string fmt (Html__.Html_tree.string_of_syntax syntax)
  in
  (syntax_parser, syntax_printer)

let convert_directory : Fs.Directory.t Arg.converter =
  let (dir_parser, dir_printer) = Arg.dir in
  let odoc_dir_parser str =
    match dir_parser str with
    | `Ok res  -> `Ok (Fs.Directory.of_string res)
    | `Error e -> `Error e
  in
  let odoc_dir_printer fmt dir = dir_printer fmt (Fs.Directory.to_string dir) in
  (odoc_dir_parser, odoc_dir_printer)

(* Very basic validation and normalization for URI paths. *)
let convert_uri : Html.Html_tree.uri Arg.converter =
  let parser str =
    if String.length str = 0 then
      `Error "invalid URI"
    else
      (* The URI is absolute if it starts with a scheme or with '/'. *)
      let is_absolute =
        List.exists ["http"; "https"; "file"; "data"; "ftp"]
          ~f:(fun scheme -> Astring.String.is_prefix ~affix:(scheme ^ ":") str)
        || String.get str 0 = '/'
      in
      let last_char = String.get str (String.length str - 1) in
      let str = if last_char <> '/' then str ^ "/" else str in
      `Ok Html.Html_tree.(if is_absolute then Absolute str else Relative str)
  in
  let printer ppf = function
    | Html.Html_tree.Absolute uri
    | Html.Html_tree.Relative uri -> Format.pp_print_string ppf uri
  in
  (parser, printer)

let docs = "ARGUMENTS"

let odoc_file_directories =
  let doc =
    "Where to look for required .odoc files. \
     (Can be present several times)."
  in
  Arg.(value & opt_all convert_directory [] &
    info ~docs ~docv:"DIR" ~doc ["I"])

let hidden =
  let doc =
    "Mark the unit as hidden. \
     (Useful for files included in module packs)."
  in
  Arg.(value & flag & info ~docs ~doc ["hidden"])

let dst =
  let doc = "Output directory where the HTML tree is expected to be saved." in
  Arg.(required & opt (some convert_directory) None &
       info ~docs ~docv:"DIR" ~doc ["o"; "output-dir"])

module Compile : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct

  let has_page_prefix file =
    file
    |> Fs.File.basename
    |> Fs.File.to_string
    |> Astring.String.is_prefix ~affix:"page-"

  let compile hidden directories resolve_fwd_refs output package_name input =
    let env =
      Env.create ~important_digests:(not resolve_fwd_refs) ~directories
    in
    let input = Fs.File.of_string input in
    let output =
      match output with
      | Some file ->
        let output = Fs.File.of_string file in
        if Fs.File.has_ext ".mld" input && not (has_page_prefix output)
        then (
          Printf.eprintf "ERROR: the name of the .odoc file produced from a \
                          .mld must start with 'page-'\n%!";
          exit 1
        );
        output
      | None ->
        let output =
          if Fs.File.has_ext ".mld" input && not (has_page_prefix input)
          then
            let directory = Fs.File.dirname input in
            let name = Fs.File.basename input in
            let name = "page-" ^ Fs.File.to_string name in
            Fs.File.create ~directory ~name
          else input
        in
        Fs.File.(set_ext ".odoc" output)
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
      Printf.eprintf "Unknown extension, expected one of: cmti, cmt, cmi or mld.\n%!";
      exit 2
    )

  let cmd =
    let dst_file =
      let doc = "Output file path. Non-existing intermediate directories are
                 created. If absent outputs a $(i,BASE).odoc file in the same
                 directory as the input file where $(i,BASE) is the basename
                 of the input file (for mld files the \"page-\" prefix will be
                 added if not already present in the input basename)."
      in
      Arg.(value & opt (some string) None & info ~docs ~docv:"PATH" ~doc ["o"])
    in
    let input =
      let doc = "Input cmti, cmt, cmi or mld file" in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE" [])
    in
    let pkg =
      let doc = "Package the input is part of" in
      Arg.(required & opt (some string) None &
           info ~docs ~docv:"PKG" ~doc ["package"; "pkg"])
    in
    let resolve_fwd_refs =
      let doc = "Try resolving forward references" in
      Arg.(value & flag & info ~doc ["r";"resolve-fwd-refs"])
    in
    Term.(const compile $ hidden $ odoc_file_directories $ resolve_fwd_refs $
      dst_file $ pkg $ input)

  let info =
    Term.info "compile"
      ~doc:"Compile a cmti, cmt, cmi or mld file to an odoc file."
end

module Support_files = struct
  let support_files without_theme output_dir =
    Support_files.write ~without_theme output_dir

  let cmd =
    let without_theme =
      let doc = "Don't copy the default theme to output directory." in
      Arg.(value & flag & info ~doc ["without-theme"])
    in
    Term.(const support_files $ without_theme $ dst)

  let info =
    let doc =
      "Copy the support files (e.g. default theme, JavaScript files) to the \
       output directory."
    in
    Term.info ~doc "support-files"
end

module Css = struct
  let cmd = Support_files.cmd

  let info =
    let doc =
      "DEPRECATED: Use `odoc support-files' to copy the CSS file for the \
       default theme."
    in
    Term.info ~doc "css"
end

module Html : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct

  let html semantic_uris closed_details _hidden directories output_dir index_for
        syntax theme_uri input_file =
    Html.Html_tree.Relative_link.semantic_uris := semantic_uris;
    Html.Html_tree.open_details := not closed_details;
    let env = Env.create ~important_digests:false ~directories in
    let file = Fs.File.of_string input_file in
    match index_for with
    | None -> Html_page.from_odoc ~env ~syntax ~theme_uri ~output:output_dir file
    | Some pkg_name ->
      Html_page.from_mld ~env ~syntax ~output:output_dir ~package:pkg_name file

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.odoc" [])
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
      Arg.(value & opt (some string) None & info ~docv:"PKG" ~doc ["index-for"])
    in
    let theme_uri =
      let doc = "Where to look for theme files (e.g. `URI/odoc.css'). \
                 Relative URIs are resolved using `--output-dir' as a target." in
      let default = Html.Html_tree.Relative "./" in
      Arg.(value & opt convert_uri default & info ~docv:"URI" ~doc ["theme-uri"])
    in
    let syntax =
      let doc = "Available options: ml | re"
      in
      Arg.(value & opt (pconv convert_syntax) (Html.Html_tree.OCaml) @@ info ~docv:"SYNTAX" ~doc ["syntax"])
    in
    Term.(const html $ semantic_uris $ closed_details $ hidden $
      odoc_file_directories $ dst $ index_for $ syntax $ theme_uri $ input)

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
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.cm{i,t,ti}" [])
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
        Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PKG_DIR" [])
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
        Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.cm{i,t,ti}" [])
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
        Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.odoc" [])
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
    ; Support_files.(cmd, info)
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
