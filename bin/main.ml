(* CR-someday trefis: the "deps" and "targets" subcommands currently output
   their result on stdout.
   It would make the interaction with jenga nicer if we could specify a file to
   output the result to. *)

open Odoc
open Cmdliner

let odoc_dir : Fs.Directory.t Arg.converter =
  let (dir_parser, dir_printer) = Arg.dir in
  let odoc_dir_parser str =
    match dir_parser str with
    | `Ok res  -> `Ok (Fs.Directory.of_string res)
    | `Error e -> `Error e
  in
  let odoc_dir_printer fmt dir = dir_printer fmt (Fs.Directory.to_string dir) in
  (odoc_dir_parser, odoc_dir_printer)

let docs = "ARGUMENTS"

let env =
  let doc =
    "Where to look for required .odoc files. \
     (Can be present several times)."
  in
  Arg.(value & opt_all odoc_dir [] @@ info ~docs ~docv:"DIR" ~doc ["I"])

let dst =
  let doc = "Output dir" (* TODO: improve *) in
  Arg.(required & opt (some odoc_dir) None @@
       info ~docs ~docv:"DIR" ~doc ["o"; "output-dir"])

module Compile : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct
  let compile directories resolve_fwd_refs output package_name filename =
    let env = Env.create ~important_digests:(not resolve_fwd_refs) ~directories in
    let file = Fs.File.of_string filename in
    let package = Root.Package.create package_name in
    let output_dir =
      let cwd = Fs.Directory.of_string (Sys.getcwd ()) in
      Fs.Directory.create ~parent:cwd ~name:package_name
    in
    if Filename.check_suffix filename "cmti" then
      Compile.cmti ~env ~output_dir ~package ?output file
    else
      Compile.cmt ~env ~output_dir ~package ?output file

  let cmd =
    let dst_file =
      let doc = "Output file name" in
      Arg.(value & opt (some string) None @@ info ~docs ~docv:"FILE" ~doc ["o"])
    in
    let input =
      let doc = "Input file (either .cmti or .cmt)" in
      Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file" [])
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
    Term.(const compile $ env $ resolve_fwd_refs $ dst_file $ pkg $ input)

  let info =
    Term.info ~doc:"Compile a .cmt[i] file to a .odoc file." "compile"
end

module Css = struct
  let copy_default_css output_dir =
    Css.copy_default_css ~etc_dir:Odoc_etc.dir ~output_dir

  let cmd = Term.(const copy_default_css $ dst)

  let info =
    Term.info ~doc:"Copies the default odoc.css to the specified directory"
      "css"
end

module Html : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct

  let link semantic_uris directories output_dir odoc_file =
    DocOckHtml.Html_tree.Relative_link.semantic_uris := semantic_uris;
    let env = Env.create ~important_digests:false ~directories in
    let odoc_file = Fs.File.of_string odoc_file in
    Html.unit ~env ~output:output_dir odoc_file

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file.odoc" [])
    in
    let semantic_uris =
      let doc = "Generate pretty (semantic) links" in
      Arg.(value & flag (info ~doc ["semantic-uris";"pretty-uris"]))
    in
    Term.(const link $ semantic_uris $ env $ dst $ input)

  let info =
    Term.info ~doc:"Generates an html file from an odoc one" "html"
end

module Depends = struct
  module Compile = struct
    let list_dependencies input_file =
      let deps = Depends.for_compile_step (Fs.File.of_string input_file) in
      List.iter (fun t ->
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
      let deps = Depends.for_link_step (Fs.File.of_string input_file) in
      Printf.printf "%s\n%!" (String.concat ~sep:"\n" deps)

    let cmd =
      let input =
        let doc = "Input file" in
        Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file.odoc" [])
      in
      Term.(const list_dependencies $ input)

  let info =
    Term.info "link-deps"
      ~doc:"List units (with their digest) which needs to be compiled in order \
            to compile this one."
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
      List.iter (Printf.printf "%s\n") targets;
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
      Term.(const list_targets $ env $ dst $ input)

    let info =
      Term.info "html-targets" ~doc:"TODO: Fill in."
  end
end

module To_xml = struct
  let to_xml odoc_file =
    let output =
      Filename.chop_extension odoc_file
      |> (fun file -> file ^ ".xml")
      |> Fs.File.of_string
    in
    let odoc_file = Fs.File.of_string odoc_file in
    Unit.load odoc_file
    |> Unit.save_xml output

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None @@ info ~doc ~docv:"file.odoc" [])
    in
    Term.(const to_xml $ input)

  let info =
    Term.info ~doc:"Takes a .odoc file and output a.xml version" "to_xml"
end

let () =
  let default =
    Term.(const
            (fun () ->
               prerr_endline "Available subcommands: compile, html, deps")
          $ const ()),
    Term.info ~version:"%%VERSION%%" "odoc"
  in
  let subcommands =
    [ Compile.(cmd, info)
    ; Html.(cmd, info)
    ; Css.(cmd, info)
    ; Depends.Compile.(cmd, info)
    ; Depends.Html.(cmd, info)
    ; Targets.Compile.(cmd, info)
    ; Targets.Html.(cmd, info)
    ; To_xml.(cmd, info) ]
  in
  match Term.eval_choice ~err:Format.err_formatter default subcommands with
  | `Error _ ->
    Format.pp_print_flush Format.err_formatter ();
    exit 2
  | _ -> ()
