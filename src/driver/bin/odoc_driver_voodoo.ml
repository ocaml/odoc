(* Voodoo-style driver

   - Must be run package-by-package
*)

open Odoc_driver_lib

type action_mode = CompileOnly | LinkAndGen | All

let generate_status ~html_dir pkg =
  let redirections =
    let redirections = Hashtbl.create 10 in
    let create_redirection old_path new_path =
      if Bos.OS.File.exists old_path |> Result.get_ok then ()
      else
        let pkg_dir = Fpath.( // ) html_dir (Odoc_unit.pkg_dir pkg) in
        Hashtbl.add redirections
          (Fpath.rem_prefix pkg_dir old_path |> Option.get)
          (Fpath.rem_prefix pkg_dir new_path |> Option.get)
    in
    List.iter
      (fun lib ->
        let lib_dir = Odoc_unit.lib_dir pkg lib in
        let lib_dir = Fpath.( // ) html_dir lib_dir in
        let old_lib_dir = Fpath.(html_dir // Odoc_unit.pkg_dir pkg / "doc") in
        Bos.OS.Dir.fold_contents
          ~elements:(`Sat (fun x -> Ok (Fpath.has_ext "html" x)))
          (fun path () ->
            match Fpath.rem_prefix lib_dir path with
            | None -> ()
            | Some suffix ->
                let old_path = Fpath.(old_lib_dir // suffix) in
                create_redirection old_path path)
          () lib_dir
        |> function
        | Ok e -> e
        | Error _ -> ())
      pkg.Packages.libraries;
    redirections
  in
  Status.file ~html_dir ~pkg ~redirections ()

let run package_name blessed actions odoc_dir odocl_dir
    ({ Common_args.html_dir; nb_workers; _ } as common) =
  Common_args.setup common;
  let index_dir = Fpath.v "_index" in
  let mld_dir = Fpath.v "_mld" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () = Worker_pool.start_workers env sw nb_workers in
  let odocl_dir = Option.value odocl_dir ~default:odoc_dir in

  let pkg =
    match Voodoo.find_pkg package_name ~blessed with
    | Some pkg -> pkg
    | None -> exit 1
  in
  let voodoo_pkg, installed = Voodoo.of_voodoo pkg in
  let all = Packages.remap_virtual [ voodoo_pkg ] in
  let units =
    let dirs = { Odoc_unit.odoc_dir; odocl_dir; index_dir; mld_dir } in
    match
      Odoc_units_of.packages ~installed ~dirs ~indices_style:Voodoo ~remap:false
        ~selected:(Util.StringSet.singleton package_name)
        all
    with
    | [ units ] -> units
    | _ -> failwith "Error, expecting a single package in voodoo mode"
  in
  Compile.init_stats [ units ];
  (* The dependencies were compiled by earlier runs (ocaml-docs-ci builds one
     package per job) and are found through the search path, so only this
     package's own libraries are compiled, in their own dependency order. *)
  (match actions with
  | LinkAndGen -> ()
  | CompileOnly | All -> Build.compile_package units);
  (match actions with
  | CompileOnly -> ()
  | LinkAndGen | All ->
      Compile.link ~warnings_tags:[ package_name ] units;
      Compile.html_support html_dir;
      Compile.generate ~generate_json:true html_dir units;
      List.iter (generate_status ~html_dir) all;
      (* Occurrence counts feed only the JSON search index, so they are a
         final step over the linked output. *)
      let occurrence_file =
        Fpath.(odocl_dir // Voodoo.occurrence_file_of_pkg pkg)
      in
      let odocl_dirs =
        List.concat_map
          (fun (p : Packages.t) ->
            Fpath.(odocl_dir // Odoc_unit.pages_dir p)
            :: List.map
                 (fun l -> Fpath.(odocl_dir // Odoc_unit.lib_obj_dir l))
                 p.libraries)
          all
      in
      Cmd_outputs.run
      @@ Odoc.count_occurrences ~input:odocl_dirs ~output:occurrence_file;
      Compile.json_index ~occurrence_file html_dir units);

  List.iter
    (fun { Cmd_outputs.log_dest; prefix; run } ->
      match log_dest with
      | `Link ->
          [ run.Run.output; run.Run.errors ]
          |> List.iter @@ fun content ->
             if String.length content = 0 then ()
             else
               let lines = String.split_on_char '\n' content in
               List.iter (fun l -> Format.printf "%s: %s\n" prefix l) lines
      | _ -> ())
    !Cmd_outputs.outputs

open Cmdliner

let run package_name blessed actions = run package_name blessed actions

let package_name =
  let doc = "Name of package to process" in
  Arg.(value & pos 0 string "" & info [] ~doc ~docv:"PACKAGE")

let blessed =
  let doc = "Blessed" in
  Arg.(value & flag & info [ "blessed" ] ~doc)

let action_conv =
  Arg.enum
    [
      ("compile-only", CompileOnly); ("link-and-gen", LinkAndGen); ("all", All);
    ]

let actions =
  let doc =
    "Actions to perform. Valid values are 'compile-only', 'link-and-gen' and \
     'all'."
  in
  Arg.(value & opt action_conv All & info [ "actions" ] ~doc)

let odoc_dir =
  let doc = "Directory in which the intermediate odoc files go" in
  Arg.(
    required & opt (some Common_args.fpath_arg) None & info [ "odoc-dir" ] ~doc)

let odocl_dir =
  let doc = "Directory in which the intermediate odocl files go" in
  Arg.(
    value & opt (some Common_args.fpath_arg) None & info [ "odocl-dir" ] ~doc)

let cmd =
  let doc = "Process output from voodoo-prep" in
  let info = Cmd.info "odoc_driver_voodoo" ~doc in
  Cmd.v info
    Term.(
      const run $ package_name $ blessed $ actions $ odoc_dir $ odocl_dir
      $ Common_args.term)

let _ = exit (Cmd.eval cmd)
