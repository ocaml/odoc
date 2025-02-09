(* Monorepo driver

   - Outputs in the same structure as the source repo
   - Uses a single package to represent the contents of the whole repo
*)

open Odoc_driver_lib

let real_run ~odoc_dir ~odocl_dir ~index_dir ~mld_dir path extra_pkgs extra_libs
    {
      Common_args.verbose;
      html_dir;
      stats;
      nb_workers;
      odoc_bin;
      odoc_md_bin;
      generate_json;
      _;
    } () =
  Option.iter (fun odoc_bin -> Odoc.odoc := Bos.Cmd.v odoc_bin) odoc_bin;
  Option.iter
    (fun odoc_md_bin -> Odoc.odoc_md := Bos.Cmd.v odoc_md_bin)
    odoc_md_bin;

  let _ = Voodoo.find_universe_and_version "foo" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Stats.init_nprocs nb_workers;
  let () = Worker_pool.start_workers env sw nb_workers in

  let all, extra_paths, generate_json =
    ( Monorepo_style.of_dune_build path ~extra_pkgs ~extra_libs,
      Voodoo.empty_extra_paths,
      generate_json )
  in

  let all = Packages.remap_virtual all in

  Logs.app (fun m -> m "Starting the compilation process...");
  let () =
    Eio.Fiber.both
      (fun () ->
        let units =
          let dirs = { Odoc_unit.odoc_dir; odocl_dir; index_dir; mld_dir } in
          Odoc_units_of.packages ~dirs ~indices_style:Odoc_units_of.Automatic
            ~extra_paths ~remap:false all
        in
        Compile.init_stats units;
        let compiled = Compile.compile ~partial_dir:odoc_dir units in
        let linked =
          Compile.link ~warnings_tags:[] ~custom_layout:true compiled
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
            ~simplified_search_output:false html_dir linked
        in
        let _ = Odoc.support_files html_dir in
        Stats.stats.finished <- true;
        ())
      (fun () -> Stats.render_stats env ~generate_json nb_workers)
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
    !Cmd_outputs.outputs;

  if stats then Stats.bench_results html_dir

let run dirs path extra_pkgs extra_libs common : unit =
  let fn = real_run path extra_pkgs extra_libs common in
  Common_args.with_dirs dirs fn

open Cmdliner

let path =
  let doc = "Path to the root of the dune monorepo" in
  Arg.(
    value
    & pos 0 Common_args.fpath_arg (Fpath.v ".")
    & info ~doc ~docv:"PATH" [])

let extra_pkgs =
  let doc = "Extra packages to link with" in
  Arg.(value & opt_all string [] & info [ "P" ] ~doc)

let extra_libs =
  let doc = "Extra libraries to link with" in
  Arg.(value & opt_all string [] & info [ "L" ] ~doc)

let cmd =
  let doc = "Generate documentation from a dune monorepo" in
  let info = Cmd.info "odoc_driver_monorepo" ~doc in
  let module A = Common_args in
  Cmd.v info
    Term.(
      const run $ A.dirs_term $ path $ extra_pkgs $ extra_libs
      $ Common_args.term)

let _ = exit (Cmd.eval cmd)
