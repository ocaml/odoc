(* Odoc driver *)
open Odoc_driver_lib

let check_packages packages =
  match packages with
  | [] -> ()
  | _ -> (
      match Opam.check packages with
      | Ok () -> ()
      | Error missing ->
          Logs.err (fun m ->
              m "Error: Unknown/uninstalled packages: %a"
                Fmt.Dump.(list string)
                (Util.StringSet.elements missing));
          exit 1)

let run_inner ~odoc_dir ~odocl_dir ~index_dir ~mld_dir ~compile_grep ~link_grep
    ~generate_grep ~index_grep ~remap ~index_mld packages
    ({ Common_args.html_dir; stats; nb_workers; generate_json; _ } as common) ()
    =
  Common_args.setup common;
  check_packages packages;

  let index_mld_content =
    Option.bind index_mld (fun fpath ->
        match Bos.OS.File.read fpath with
        | Ok content -> Some content
        | Error (`Msg msg) ->
            Logs.err (fun m ->
                m "Failed to read index_mld file '%a': %s" Fpath.pp fpath msg);
            exit 1)
  in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () = Worker_pool.start_workers env sw nb_workers in
  let installed = Installed.of_switch () in
  let all = Packages.of_packages ~installed ~packages_dir:None packages in
  let all = Packages.remap_virtual all in

  let selected = Util.StringSet.of_list packages in
  (* The packages this run does not render keep their links working by
     pointing at ocaml.org. *)
  let remaps =
    if not remap then []
    else
      List.concat_map
        (fun (pkg : Packages.t) ->
          if Util.StringSet.mem pkg.name selected then []
          else Packages.remaps pkg)
        all
  in

  let units () =
    let dirs = { Odoc_unit.odoc_dir; odocl_dir; index_dir; mld_dir } in
    Odoc_units_of.packages ~installed ~dirs
      ~indices_style:
        (Odoc_units_of.Normal { toplevel_content = index_mld_content })
      ~remap ~selected all
  in

  Logs.app (fun m -> m "Starting the compilation process...");

  let () =
    Eio.Fiber.both
      (fun () ->
        let pkgs = units () in
        Compile.init_stats pkgs;
        Build.all ~html_dir ~remaps ~generate_json ~warnings_tags:packages pkgs;
        List.iter (fun pkg -> Status.file ~html_dir ~pkg ()) all;
        Stats.finish ();
        ())
      (fun () -> Stats.render_stats env nb_workers)
  in

  Logs.app (fun m ->
      m "Documentation generation complete. Results are in %a" Fpath.pp html_dir);

  let grep_log ty s =
    let open Astring in
    let do_ affix =
      let grep { Cmd_outputs.log_dest; prefix; run } =
        if log_dest = ty then
          let l = run.Run.cmd |> String.concat ~sep:" " in
          if String.is_infix ~affix l then Format.printf "%s: %s\n" prefix l
      in
      List.iter grep !Cmd_outputs.outputs
    in
    Option.iter do_ s
  in
  (* Grep log compile commands *)
  grep_log `Compile compile_grep;
  (* Grep log link commands *)
  grep_log `Link link_grep;
  (* Grep log generate commands *)
  grep_log `Generate generate_grep;
  (* Grep log index and co commands *)
  grep_log `Count_occurrences index_grep;
  grep_log `Index index_grep;

  let maybe_write_header =
    let written = ref false in
    fun () ->
      if not !written then (
        written := true;
        Logs.app (fun m -> m "Output from commands:"))
  in

  List.iter
    (fun { Cmd_outputs.log_dest; prefix; run } ->
      match log_dest with
      | `Link | `Compile ->
          [ run.Run.output; run.Run.errors ]
          |> List.iter @@ fun content ->
             if String.length content = 0 then ()
             else (
               maybe_write_header ();
               Logs.app (fun m -> m "%s" prefix);
               Logs.app (fun m ->
                   m "%s" (String.init (String.length prefix) (fun _ -> '-')));
               let lines = String.split_on_char '\n' content in
               List.iter (fun l -> Logs.app (fun m -> m "%s" l)) lines;
               Logs.app (fun m -> m ""))
      | _ -> ())
    !Cmd_outputs.outputs;

  if stats then Stats.bench_results html_dir

let run dirs compile_grep link_grep generate_grep index_grep remap packages
    index_mld common : unit =
  let fn =
    run_inner ~compile_grep ~link_grep ~generate_grep ~index_grep ~remap
      ~index_mld packages common
  in
  Common_args.with_dirs dirs fn

open Cmdliner

let compile_grep =
  let doc = "Show compile commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "compile-grep" ] ~doc ~docs:Manpage.s_none)

let link_grep =
  let doc = "Show link commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "link-grep" ] ~doc ~docs:Manpage.s_none)

let generate_grep =
  let doc = "Show html-generate commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "html-grep" ] ~doc ~docs:Manpage.s_none)

let index_grep =
  let doc = "Show compile-index commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "index-grep" ] ~doc ~docs:Manpage.s_none)

let remap =
  let doc = "Remap paths in non-selected packages to ocaml.org" in
  Arg.(value & flag & info [ "remap" ] ~doc ~docs:Manpage.s_common_options)

let packages = Arg.(value & pos_all string [] & info [] ~docv:"PACKAGES")

let index_mld =
  let doc =
    "Provide an index.mld file to serve as the top-level index of the \
     documentation"
  in
  Arg.(
    value
    & opt (some Common_args.fpath_arg) None
    & info [ "index-mld" ] ~docv:"INDEX" ~doc)

let cmd_term =
  let module A = Common_args in
  Term.(
    const run $ A.dirs_term $ compile_grep $ link_grep $ generate_grep
    $ index_grep $ remap $ packages $ index_mld $ Common_args.term)

let cmd =
  let doc =
    "Documents packages present in your opam switch. This mode will select all \
     opam packages that are dependencies of the selected packages."
  in
  let info = Cmd.info "odoc_driver" ~doc in
  Cmd.v info cmd_term

let _ = exit (Cmd.eval cmd)
