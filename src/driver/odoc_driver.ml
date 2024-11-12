(* Odoc driver *)

let render_stats env nprocs =
  let if_app f =
    match Logs.level () with Some (App | Warning) | None -> f () | _ -> ()
  in
  (* Avoids overkill indentation  *)
  if_app @@ fun () ->
  let open Progress in
  let clock = Eio.Stdenv.clock env in
  let total = Atomic.get Stats.stats.total_units in
  let total_impls = Atomic.get Stats.stats.total_impls in
  let total_mlds = Atomic.get Stats.stats.total_mlds in
  let total_assets = Atomic.get Stats.stats.total_assets in
  let total_indexes = Atomic.get Stats.stats.total_indexes in
  let bar message total =
    let open Progress.Line in
    list [ lpad 16 (const message); bar total; count_to total ]
  in
  let procs total =
    let open Progress.Line in
    list [ lpad 16 (const "Processes"); bar total; count_to total ]
  in
  let description =
    let open Progress.Line in
    string
  in
  let descriptions = Multi.lines (List.init nprocs (fun _ -> description)) in

  let non_hidden = Atomic.get Stats.stats.non_hidden_units in

  let dline x y = Multi.line (bar x y) in
  with_reporters
    Multi.(
      dline "Compiling" total
      ++ dline "Compiling impls" total_impls
      ++ dline "Compiling pages" total_mlds
      ++ dline "Compiling assets" total_assets
      ++ dline "Linking" non_hidden
      ++ dline "Linking impls" total_impls
      ++ dline "Linking mlds" total_mlds
      ++ dline "Indexes" total_indexes
      ++ dline "HTML" (total_impls + non_hidden + total_mlds)
      ++ line (procs nprocs)
      ++ descriptions)
    (fun comp compimpl compmld compassets link linkimpl linkmld indexes html
         procs descr ->
      let rec inner (a, b, c, j, d, e, f, i, g, h) =
        Eio.Time.sleep clock 0.1;
        let a' = Atomic.get Stats.stats.compiled_units in
        let b' = Atomic.get Stats.stats.compiled_impls in
        let c' = Atomic.get Stats.stats.compiled_mlds in
        let j' = Atomic.get Stats.stats.compiled_assets in
        let d' = Atomic.get Stats.stats.linked_units in
        let e' = Atomic.get Stats.stats.linked_impls in
        let f' = Atomic.get Stats.stats.linked_mlds in
        let i' = Atomic.get Stats.stats.generated_indexes in
        let g' = Atomic.get Stats.stats.generated_units in
        let h' = Atomic.get Stats.stats.processes in
        List.iteri
          (fun i descr -> descr (Atomic.get Stats.stats.process_activity.(i)))
          descr;
        comp (a' - a);
        compimpl (b' - b);
        compmld (c' - c);
        compassets (j' - j);
        link (d' - d);
        linkimpl (e' - e);
        linkmld (f' - f);
        indexes (i' - i);
        html (g' - g);
        procs (h' - h);
        if g' < non_hidden + total_impls + total_mlds then
          inner (a', b', c', j', d', e', f', i', g', h')
      in
      inner (0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

let remap_virtual_interfaces duplicate_hashes pkgs =
  let open Packages in
  Util.StringMap.map
    (fun pkg ->
      {
        pkg with
        libraries =
          pkg.libraries
          |> List.map (fun lib ->
                 {
                   lib with
                   modules =
                     lib.modules
                     |> List.map (fun m ->
                            let m_intf =
                              if
                                Util.StringMap.mem m.m_intf.mif_hash
                                  duplicate_hashes
                                && Fpath.has_ext "cmt" m.m_intf.mif_path
                              then
                                match
                                  List.filter
                                    (fun intf ->
                                      Fpath.has_ext "cmti" intf.mif_path)
                                    (Util.StringMap.find m.m_intf.mif_hash
                                       duplicate_hashes)
                                with
                                | [ x ] -> x
                                | _ -> m.m_intf
                              else m.m_intf
                            in
                            { m with m_intf });
                 });
      })
    pkgs

type action_mode = CompileOnly | LinkAndGen | All

type mode =
  | Voodoo of { package_name : string; blessed : bool; actions : action_mode }
  | Dune of { path : Fpath.t }
  | OpamLibs of { libs : string list }
  | OpamPackages of { packages : string list }

let run mode
    {
      Common_args.verbose;
      odoc_dir;
      odocl_dir;
      index_dir;
      mld_dir;
      html_dir;
      stats;
      nb_workers;
      odoc_bin;
      compile_grep;
      link_grep;
      generate_grep;
    } =
  Option.iter (fun odoc_bin -> Odoc.odoc := Bos.Cmd.v odoc_bin) odoc_bin;
  let _ = Voodoo.find_universe_and_version "foo" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Stats.init_nprocs nb_workers;
  let () = Worker_pool.start_workers env sw nb_workers in

  let all, extra_paths, actions =
    match mode with
    | Voodoo { package_name = p; blessed; actions } ->
        let all = Voodoo.of_voodoo p ~blessed in
        let extra_paths = Voodoo.extra_paths odoc_dir in
        (all, extra_paths, actions)
    | Dune { path } -> (Dune_style.of_dune_build path, Voodoo.empty_extra_paths, All)
    | OpamLibs { libs } ->
        ( Packages.of_libs ~packages_dir:None (Util.StringSet.of_list libs),
          Voodoo.empty_extra_paths, All )
    | OpamPackages { packages } ->
        (Packages.of_packages ~packages_dir:None packages, Voodoo.empty_extra_paths, All)
  in

  let virtual_check =
    let hashes =
      List.fold_left
        (fun acc (_name, pkg) ->
          List.fold_left
            (fun acc lib ->
              List.fold_left
                (fun acc m ->
                  let hash = m.Packages.m_intf.mif_hash in
                  Util.StringMap.update hash
                    (function
                      | None -> Some [ m.m_intf ]
                      | Some l -> Some (m.m_intf :: l))
                    acc)
                acc lib.Packages.modules)
            acc pkg.Packages.libraries)
        Util.StringMap.empty
        (Util.StringMap.to_list all)
    in
    Util.StringMap.filter (fun _hash intfs -> List.length intfs > 1) hashes
  in

  let all = remap_virtual_interfaces virtual_check all in

  let partial =
    match mode with
    | Voodoo _ -> (
        match Util.StringMap.to_list all with
        | [ (_, p) ] ->
            let output_path = Fpath.(odoc_dir // p.pkg_dir) in
            Some output_path
        | _ -> failwith "Error, expecting singleton library in voodoo mode")
    | _ -> None
  in
  let () =
    Eio.Fiber.both
      (fun () ->
        let units =
          let all = Util.StringMap.bindings all |> List.map snd in
          let dirs =
            let odocl_dir = Option.value odocl_dir ~default:odoc_dir in
            { Odoc_unit.odoc_dir; odocl_dir; index_dir; mld_dir }
          in
          Odoc_units_of.packages ~dirs ~extra_paths all
        in
        Compile.init_stats units;
        let compiled =
          match actions with
          | LinkAndGen -> units
          | CompileOnly | All ->
              Compile.compile ?partial ~partial_dir:odoc_dir units
        in
        let () =
          match mode with
          | Voodoo _ -> Voodoo.write_lib_markers odoc_dir all
          | Dune _ | Opam _ -> ()
        in
        match actions with
        | CompileOnly -> ()
        | LinkAndGen | All ->
            let linked = Compile.link compiled in
            let occurrence_file =
              let output =
                Fpath.( / ) odoc_dir "occurrences-all.odoc-occurrences"
              in
              let () = Odoc.count_occurrences ~input:[ odoc_dir ] ~output in
              output
            in
            let () = Compile.html_generate ~occurrence_file html_dir linked in
            let _ = Odoc.support_files html_dir in
            ())
      (fun () -> render_stats env nb_workers)
  in

  let grep_log ty s =
    let open Astring in
    let do_ affix =
      let grep (dst, _err, prefix, content) =
        if dst = ty then
          let lines = String.cuts ~sep:"\n" content in
          List.iter
            (fun l ->
              if String.is_infix ~affix l then Format.printf "%s: %s\n" prefix l)
            lines
      in
      List.iter grep !Cmd_outputs.outputs
    in
    Option.iter do_ s
  in
  grep_log `Compile compile_grep;
  grep_log `Link link_grep;
  grep_log `Generate generate_grep;

  List.iter
    (fun (dst, _err, prefix, content) ->
      match dst with
      | `Link ->
          if String.length content = 0 then ()
          else
            let lines = String.split_on_char '\n' content in
            List.iter (fun l -> Format.printf "%s: %s\n" prefix l) lines
      | _ -> ())
    !Cmd_outputs.outputs;

  Format.eprintf "Final stats: %a@.%!" Stats.pp_stats Stats.stats;
  Format.eprintf "Total time: %f@.%!" (Stats.total_time ());
  if stats then Stats.bench_results html_dir

open Cmdliner

module Voodoo_mode = struct
  let run package_name blessed actions =
    run (Voodoo { package_name; blessed; actions })

  let package_name =
    let doc = "Name of package to process with voodoo" in
    Arg.(value & pos 0 string "" & info [] ~doc)

  let blessed =
    let doc = "Blessed" in
    Arg.(value & flag & info [ "blessed" ] ~doc)

  let action_of_string = function
    | "compile-only" -> Ok CompileOnly
    | "link-and-gen" -> Ok LinkAndGen
    | "all" -> Ok All
    | _ ->
        Error
          (`Msg
            "Invalid action. Options are 'compile-only', 'link-and-gen' or \
             'all'")

  let string_of_action fmt = function
    | CompileOnly -> Format.fprintf fmt "compile-only"
    | LinkAndGen -> Format.fprintf fmt "link-and-gen"
    | All -> Format.fprintf fmt "all"

  let action_conv = Arg.conv (action_of_string, string_of_action)

  let actions =
    let doc = "Actions to perform" in
    Arg.(value & opt action_conv All & info [ "actions" ] ~doc)

  let cmd =
    let doc = "Process output from voodoo-prep" in
    let info = Cmd.info "voodoo" ~doc in
    Cmd.v info
      Term.(const run $ package_name $ blessed $ actions $ Common_args.term)
end

module Dune_mode = struct
  let run path = run (Dune { path })

  let dune_style =
    Arg.(value & pos 0 Common_args.fpath_arg (Fpath.v ".") & info [])

  let cmd =
    let doc =
      "Dune mode, which builds the documentation of the local libraries of a \
       dune project."
    in
    let info = Cmd.info "dune" ~doc in
    Cmd.v info Term.(const run $ dune_style $ Common_args.term)
end

module OpamLibs = struct
  let run libs = run (OpamLibs { libs })

  let libs =
    (* TODO: Is it package or library? *)
    let doc = "The libraries to document" in
    Arg.(value & opt_all string [] & info [ "l" ] ~doc)

  let cmd_term = Term.(const run $ libs $ Common_args.term)

  let cmd =
    let doc =
      "Documents libraries present in your opam switch. This mode will select  \n\
      \               all libraries that are dependent on the selected \
       libraries. This is a\n\
      \               narrower set of dependencies than those chosen by \
       invoking 'opam' mode."
    in
    let info = Cmd.info "opam-lib" ~doc in
    Cmd.v info cmd_term
end

module OpamPackages = struct
  let run packages = run (OpamPackages { packages })

  let packages =
    (* TODO: Is it package or library? *)
    let doc = "The packages to document" in
    Arg.(value & opt_all string [] & info [ "p" ] ~doc)

  let cmd_term = Term.(const run $ packages $ Common_args.term)

  let cmd =
    let doc =
      "Documents packages present in your opam switch. This mode will select \
       all opam packages that are dependencies of the selected packages."
    in
    let info = Cmd.info "opam" ~doc in
    Cmd.v info cmd_term
end

let cmd =
  let doc = "Generate odoc documentation" in
  let info = Cmd.info "odoc_driver" ~doc in
  Cmd.group info
    [ Voodoo_mode.cmd; Dune_mode.cmd; OpamPackages.cmd; OpamLibs.cmd ]

let _ = exit (Cmd.eval cmd)
