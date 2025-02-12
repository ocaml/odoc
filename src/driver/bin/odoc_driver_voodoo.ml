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
    { Common_args.verbose; html_dir; nb_workers; odoc_bin; odoc_md_bin; _ } =
  Option.iter (fun odoc_bin -> Odoc.odoc := Bos.Cmd.v odoc_bin) odoc_bin;
  Option.iter
    (fun odoc_md_bin -> Odoc.odoc_md := Bos.Cmd.v odoc_md_bin)
    odoc_md_bin;
  let index_dir = Fpath.v "_index" in
  let mld_dir = Fpath.v "_mld" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Stats.init_nprocs nb_workers;
  let () = Worker_pool.start_workers env sw nb_workers in

  let all, extra_paths, actions, generate_json =
    let all = Voodoo.of_voodoo package_name ~blessed in
    let extra_paths = Voodoo.extra_paths odoc_dir in
    (all, extra_paths, actions, true)
  in

  let all = Packages.remap_virtual all in

  let partial =
    match all with
    | [ p ] ->
        let output_path = Fpath.(odoc_dir // p.pkg_dir) in
        Some output_path
    | _ -> failwith "Error, expecting singleton library in voodoo mode"
  in
  let units =
    let dirs =
      let odocl_dir = Option.value odocl_dir ~default:odoc_dir in
      { Odoc_unit.odoc_dir; odocl_dir; index_dir; mld_dir }
    in
    Odoc_units_of.packages ~dirs ~indices_style:Voodoo ~extra_paths ~remap:false
      all
  in
  Compile.init_stats units;
  let compiled =
    match actions with
    | LinkAndGen -> units
    | CompileOnly | All -> Compile.compile ?partial ~partial_dir:odoc_dir units
  in
  let () = Voodoo.write_lib_markers odoc_dir all in
  let () =
    match actions with
    | CompileOnly -> ()
    | LinkAndGen | All ->
        let linked =
          Compile.link ~warnings_tags:[ package_name ] ~custom_layout:false
            compiled
        in
        let occurrence_file =
          let output =
            Fpath.( / ) odoc_dir "occurrences-all.odoc-occurrences"
          in
          let () = Odoc.count_occurrences ~input:[ odoc_dir ] ~output in
          output
        in
        let () =
          Compile.html_generate ~occurrence_file ~remaps:[] ~generate_json
            ~simplified_search_output:true html_dir linked
        in
        List.iter (generate_status ~html_dir) all;
        let _ = Odoc.support_files html_dir in
        ()
  in

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
