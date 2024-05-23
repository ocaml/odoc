(* compile *)

type ty = Module of Packages.modulety | Mld of Packages.mld

type compiled = {
  m : ty;
  output_dir : Fpath.t;
  output_file : Fpath.t;
  include_dirs : Fpath.Set.t;
  impl : (Fpath.t * Fpath.t) option;
}

let mk_byhash (pkgs : Packages.t Util.StringMap.t) =
  Util.StringMap.fold
    (fun _pkg_name pkg acc ->
      List.fold_left
        (fun acc (lib : Packages.libty) ->
          List.fold_left
            (fun acc (m : Packages.modulety) ->
              Util.StringMap.add m.m_intf.mif_hash m acc)
            acc lib.modules)
        acc pkg.Packages.libraries)
    pkgs Util.StringMap.empty

let init_stats (pkgs : Packages.t Util.StringMap.t) =
  let total, total_impl, non_hidden, mlds =
    Util.StringMap.fold
      (fun _pkg_name pkg acc ->
        let (tt, ti, nh, md) = List.fold_left
          (fun (tt, ti, nh, md) (lib : Packages.libty) ->
            List.fold_left
              (fun (total, total_impl, non_hidden, mlds) (m : Packages.modulety) ->
                let total = total + 1 in
                let total_impl =
                  match m.m_impl with
                  | Some _ -> total_impl + 1
                  | None -> total_impl
                in
                let non_hidden =
                  if m.m_hidden then non_hidden else non_hidden + 1
                in
                (total, total_impl, non_hidden, mlds))
              (tt, ti, nh, md) lib.modules)
          acc pkg.Packages.libraries
              in           (tt, ti, nh, md + List.length pkg.Packages.mlds)
              )
      pkgs (0, 0, 0, 0)
  in
  Atomic.set Stats.stats.total_units total;
  Atomic.set Stats.stats.total_impls total_impl;
  Atomic.set Stats.stats.non_hidden_units non_hidden;
  Atomic.set Stats.stats.total_mlds mlds

open Eio.Std

let compile output_dir all =
  let hashes = mk_byhash all in
  let tbl = Hashtbl.create 10 in

  let compile_one compile_other hash =
    match Util.StringMap.find_opt hash hashes with
    | None ->
        Logs.debug (fun m -> m "Error locating hash: %s" hash);
        Error Not_found
    | Some modty ->
        let deps = modty.m_intf.mif_deps in
        let output_file = Fpath.(output_dir // modty.m_intf.mif_odoc_file) in
        let fibers =
          Fiber.List.map
            (fun (n, h) ->
              match compile_other h with
              | Ok r -> Some r
              | Error _exn ->
                  Logs.debug (fun m -> m "Missing module %s (hash %s)" n h);
                  None)
            deps
        in
        let includes =
          List.fold_left
            (fun acc opt ->
              match opt with
              | Some s -> Fpath.(Set.add s.output_dir acc)
              | _ -> acc)
            Fpath.Set.empty fibers
        in
        let includes = Fpath.Set.add output_dir includes in
        let impl =
          match modty.m_impl with
          | Some impl -> (
              match impl.mip_src_info with
              | Some si ->
                  let output_file = Fpath.(output_dir // impl.mip_odoc_file) in
                  Odoc.compile_impl output_dir impl.mip_path includes
                    impl.mip_parent_id si.src_id;
                  Atomic.incr Stats.stats.compiled_impls;
                  Some (output_file, si.src_path)
              | None -> None)
          | None -> None
        in

        Odoc.compile output_dir modty.m_intf.mif_path includes
          modty.m_intf.mif_parent_id;
        Atomic.incr Stats.stats.compiled_units;

        let output_dir = Fpath.split_base output_file |> fst in
        Ok
          {
            m = Module modty;
            output_dir;
            output_file;
            include_dirs = includes;
            impl;
          }
  in

  let rec compile : string -> (compiled, exn) Result.t =
   fun hash ->
    match Hashtbl.find_opt tbl hash with
    | Some p -> Promise.await_exn p
    | None ->
        let p, r = Promise.create () in
        Hashtbl.add tbl hash p;
        let result = compile_one compile hash in
        Promise.resolve_ok r result;
        result
  in
  let all_hashes = Util.StringMap.bindings hashes |> List.map fst in
  let mod_results = Fiber.List.map compile all_hashes in
  let mods = List.filter_map (function Ok x -> Some x | Error _ -> None) mod_results in
  Util.StringMap.fold
    (fun _ (pkg : Packages.t) acc ->
      Logs.debug (fun m ->
          m "Package %s mlds: [%a]" pkg.name
            Fmt.(list ~sep:sp Packages.pp_mld)
            pkg.mlds);
      List.fold_left
        (fun acc (mld : Packages.mld) ->
          let output_file = Fpath.(output_dir // mld.Packages.mld_odoc_file) in
          let odoc_output_dir = Fpath.split_base output_file |> fst in
          Odoc.compile output_dir mld.mld_path Fpath.Set.empty mld.mld_parent_id;
          Atomic.incr Stats.stats.compiled_mlds;
          let include_dirs =
            List.map (fun f -> Fpath.(output_dir // f)) mld.mld_deps
            |> Fpath.Set.of_list
          in
          let include_dirs = Fpath.Set.add odoc_output_dir include_dirs in
          { m = Mld mld; output_dir; output_file; include_dirs; impl = None }
          :: acc)
        acc pkg.mlds)
    all mods

type linked = { output_file : Fpath.t; src : Fpath.t option }

let link : compiled list -> _ =
 fun compiled ->
  let link : compiled -> linked list =
   fun c ->
    let include_dirs = Fpath.Set.add c.output_dir c.include_dirs in
    let impl =
      match c.impl with
      | Some (x, y) ->
          Logs.debug (fun m -> m "Linking impl: %a" Fpath.pp x);
          Odoc.link x include_dirs;
          Atomic.incr Stats.stats.linked_impls;
          [ { output_file = Fpath.(set_ext "odocl" x); src = Some y } ]
      | None -> []
    in
    match c.m with
    | Module m when m.m_hidden ->
        Logs.debug (fun m -> m "not linking %a" Fpath.pp c.output_file);
        impl
    | _ ->
        Logs.debug (fun m -> m "linking %a" Fpath.pp c.output_file);
        Odoc.link c.output_file include_dirs;
        (match c.m with | Module _ -> Atomic.incr Stats.stats.linked_units | Mld _ -> Atomic.incr Stats.stats.linked_mlds);
        { output_file = Fpath.(set_ext "odocl" c.output_file); src = None }
        :: impl
  in
  Fiber.List.map link compiled |> List.concat

let html_generate : Fpath.t -> linked list -> _ =
 fun output_dir linked ->
  let html_generate : linked -> unit =
   fun l ->
    Odoc.html_generate (Fpath.to_string output_dir) l.output_file l.src;
    Atomic.incr Stats.stats.generated_units
  in
  Fiber.List.iter html_generate linked
