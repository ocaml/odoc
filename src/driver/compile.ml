(* compile *)

type ty = Module of Packages.modulety | Mld of Packages.mld

type impl = { impl_odoc : Fpath.t; impl_odocl: Fpath.t; src : Fpath.t }

type pkg_args = {
  docs : (string * Fpath.t) list;
  libs : (string * Fpath.t) list;
}

type compiled = {
  m : ty;
  odoc_output_dir : Fpath.t; (* e.g. "_odoc/base/lib/base/" *)
  odoc_file : Fpath.t; (* Full path to odoc file *)
  odocl_file : Fpath.t;
  include_dirs : Fpath.Set.t;
  impl : impl option;
  pkg_args : pkg_args;
  pkg_name : string;
  pkg_dir : Fpath.t;
}

let mk_byhash (pkgs : Packages.t Util.StringMap.t) =
  Util.StringMap.fold
    (fun pkgname pkg acc ->
      List.fold_left
        (fun acc (lib : Packages.libty) ->
          List.fold_left
            (fun acc (m : Packages.modulety) ->
              Util.StringMap.add m.m_intf.mif_hash (pkgname, m) acc)
            acc lib.modules)
        acc pkg.Packages.libraries)
    pkgs Util.StringMap.empty

let init_stats (pkgs : Packages.t Util.StringMap.t) =
  let total, total_impl, non_hidden, mlds =
    Util.StringMap.fold
      (fun _pkg_name pkg acc ->
        let tt, ti, nh, md =
          List.fold_left
            (fun (tt, ti, nh, md) (lib : Packages.libty) ->
              List.fold_left
                (fun (total, total_impl, non_hidden, mlds)
                     (m : Packages.modulety) ->
                  let total = total + 1 in
                  let total_impl =
                    match m.m_impl with
                    | Some impl -> (
                        match impl.mip_src_info with
                        | Some _ -> total_impl + 1
                        | None -> total_impl)
                    | None -> total_impl
                  in
                  let non_hidden =
                    if m.m_hidden then non_hidden else non_hidden + 1
                  in
                  (total, total_impl, non_hidden, mlds))
                (tt, ti, nh, md) lib.modules)
            acc pkg.Packages.libraries
        in
        (tt, ti, nh, md + List.length pkg.Packages.mlds))
      pkgs (0, 0, 0, 0)
  in
  Atomic.set Stats.stats.total_units total;
  Atomic.set Stats.stats.total_impls total_impl;
  Atomic.set Stats.stats.non_hidden_units non_hidden;
  Atomic.set Stats.stats.total_mlds mlds

open Eio.Std

type partial = 
  (string * compiled) list * (string * Packages.modulety) Util.StringMap.t

let unmarshal filename =
  let ic = open_in_bin (Fpath.to_string filename) in
  let (v : partial) = Marshal.from_channel ic in
  close_in ic;
  v

let marshal (v : partial) filename =
  let p = Fpath.parent filename in
  Util.mkdir_p p;
  let oc = open_out_bin (Fpath.to_string filename) in
  Marshal.to_channel oc v [];
  close_out oc

let find_partials odoc_dir =
  let tbl = Hashtbl.create 1000 in
  let hashes_result = Bos.OS.Dir.fold_contents ~dotfiles:false
  (fun p hashes ->
    if Fpath.filename p = "index.m"
    then
      let (tbl', hashes') = unmarshal p in
      List.iter (fun (k,v) -> Hashtbl.replace tbl k (Promise.create_resolved (Ok v))) tbl';
      Util.StringMap.union (fun _x o1 _o2 -> Some o1) hashes hashes'
    else hashes) Util.StringMap.empty odoc_dir in
  match hashes_result with
  | Ok h -> h, tbl
  | Error _ -> (* odoc_dir doesn't exist...? *) Util.StringMap.empty, tbl

let compile partial ~output_dir ?linked_dir all =
  let linked_dir = Option.value linked_dir ~default:output_dir in
  let hashes = mk_byhash all in
  let other_hashes, tbl =
    match partial with
    | Some _ -> find_partials output_dir
    | None -> Util.StringMap.empty, Hashtbl.create 10 in
  let all_hashes = Util.StringMap.union (fun _x o1 _o2 -> Some o1) hashes other_hashes in
  let pkg_args =
    let docs, libs =
      Util.StringMap.fold
        (fun pkgname (pkg : Packages.t) (docs, libs) ->
          let doc = (pkgname, Fpath.(output_dir // pkg.mld_odoc_dir)) in
          let lib =
            List.map
              (fun lib ->
                ( lib.Packages.lib_name, Fpath.(output_dir // lib.Packages.odoc_dir )))
              pkg.Packages.libraries
          in
          let docs = doc :: docs and libs = List.rev_append lib libs in
          (docs, libs))
        all ([], [])
    in
    { docs; libs }
  in

  let compile_one compile_other hash =
    match Util.StringMap.find_opt hash all_hashes with
    | None ->
        Logs.debug (fun m -> m "Error locating hash: %s" hash);
        Error Not_found
    | Some (package_name, modty) ->
        let deps = modty.m_intf.mif_deps in
        let odoc_file = Fpath.(output_dir // modty.m_intf.mif_odoc_file) in
        let odocl_file = Fpath.(linked_dir // modty.m_intf.mif_odocl_file) in
        let fibers =
          List.map
            (fun (n, h) ->
              match compile_other h with
              | Ok r -> Some r
              | Error _exn ->
                  Logs.debug (fun m ->
                      m "Missing module %s (hash %s, required by %s)" n h
                        modty.m_name);
                  None)
            deps
        in
        let includes =
          List.fold_left
            (fun acc opt ->
              match opt with
              | Some s -> Fpath.(Set.add s.odoc_output_dir acc)
              | _ -> acc)
            Fpath.Set.empty fibers
        in
        let includes = Fpath.Set.add output_dir includes in
        let impl =
          match modty.m_impl with
          | Some impl -> (
              match impl.mip_src_info with
              | Some si ->
                  let odoc_file = Fpath.(output_dir // impl.mip_odoc_file) in
                  let odocl_file = Fpath.(linked_dir // impl.mip_odocl_file) in
                  Odoc.compile_impl ~output_dir ~input_file:impl.mip_path
                    ~includes ~parent_id:impl.mip_parent_id ~source_id:si.src_id;
                  Atomic.incr Stats.stats.compiled_impls;
                  Some { impl_odoc = odoc_file; impl_odocl=odocl_file; src = si.src_path }
              | None -> None)
          | None -> None
        in

        Odoc.compile ~output_dir ~input_file:modty.m_intf.mif_path ~includes
          ~parent_id:modty.m_intf.mif_parent_id;
        Atomic.incr Stats.stats.compiled_units;

        let odoc_output_dir = Fpath.split_base odoc_file |> fst in

        Ok
          {
            m = Module modty;
            odoc_output_dir;
            odoc_file;
            odocl_file;
            include_dirs = includes;
            impl;
            pkg_args;
            pkg_dir = modty.m_pkg_dir;
            pkg_name = package_name;
          }
  in

  let rec compile : string -> (compiled, exn) Result.t =
   fun hash ->
    match Hashtbl.find_opt tbl hash with
    | Some p -> Promise.await p
    | None ->
        let p, r = Promise.create () in
        Hashtbl.add tbl hash p;
        let result = compile_one compile hash in
        Promise.resolve r result;
        result
  in
  let to_build = Util.StringMap.bindings hashes |> List.map fst in
  let mod_results = List.map compile to_build in
  let zipped_res = List.map2 (fun a b -> (a,b)) to_build mod_results in
  let zipped = List.filter_map (function (a, Ok b) -> Some (a,b) | _ -> None) zipped_res in
  let mods =
    List.filter_map (function Ok x -> Some x | Error _ -> None) mod_results
  in
  let result = Util.StringMap.fold
    (fun package_name (pkg : Packages.t) acc ->
      Logs.debug (fun m ->
          m "Package %s mlds: [%a]" pkg.name
            Fmt.(list ~sep:sp Packages.pp_mld)
            pkg.mlds);
      List.fold_left
        (fun acc (mld : Packages.mld) ->
          let odoc_file = Fpath.(output_dir // mld.Packages.mld_odoc_file) in
          let odocl_file = Fpath.(linked_dir // mld.Packages.mld_odocl_file) in
          let odoc_output_dir = Fpath.split_base odoc_file |> fst in
          Odoc.compile ~output_dir ~input_file:mld.mld_path
            ~includes:Fpath.Set.empty ~parent_id:mld.mld_parent_id;
          Atomic.incr Stats.stats.compiled_mlds;
          let include_dirs =
            List.map (fun f -> Fpath.(output_dir // f)) mld.mld_deps
            |> Fpath.Set.of_list
          in
          let include_dirs = Fpath.Set.add odoc_output_dir include_dirs in
          let odoc_output_dir = Fpath.split_base odoc_file |> fst in
          {
            m = Mld mld;
            odoc_output_dir;
            odoc_file;
            odocl_file;
            include_dirs;
            impl = None;
            pkg_args;
            pkg_dir = mld.mld_pkg_dir;
            pkg_name = package_name;
          }
          :: acc)
        acc pkg.mlds)
    all mods in

  (match partial with
  | Some l -> marshal (zipped, hashes) Fpath.(l / "index.m")
  | None -> ());
  result

type linked = {
  output_file : Fpath.t;
  src : Fpath.t option;
  pkg_dir : Fpath.t;
}

let link : compiled list -> _ =
 fun compiled ->
  let link : compiled -> linked list =
   fun c ->
    let includes = Fpath.Set.add c.odoc_output_dir c.include_dirs in
    let link input_file output_file =
      let { pkg_args = { libs; docs }; pkg_name; _ } =
        c
      in
      Odoc.link ~input_file ~output_file ~includes ~libs ~docs ~current_package:pkg_name ()
    in
    let impl =
      match c.impl with
      | Some { impl_odoc; impl_odocl; src } ->
          Logs.debug (fun m -> m "Linking impl: %a -> %a" Fpath.pp impl_odoc Fpath.pp impl_odocl);
          link impl_odoc impl_odocl;
          Atomic.incr Stats.stats.linked_impls;
          [
            {
              pkg_dir = c.pkg_dir;
              output_file = impl_odocl;
              src = Some src;
            };
          ]
      | None -> []
    in
    match c.m with
    | Module m when m.m_hidden ->
        Logs.debug (fun m -> m "not linking %a" Fpath.pp c.odoc_file);
        impl
    | _ ->
        Logs.debug (fun m -> m "linking %a" Fpath.pp c.odoc_file);
        link c.odoc_file c.odocl_file;
        (match c.m with
        | Module _ -> Atomic.incr Stats.stats.linked_units
        | Mld _ -> Atomic.incr Stats.stats.linked_mlds);
        {
          output_file = c.odocl_file;
          src = None;
          pkg_dir = c.pkg_dir;
        }
        :: impl
  in
  Fiber.List.map link compiled |> List.concat

let index_one ~odocl_dir pkgname pkg =
  let dir = pkg.Packages.pkg_dir in
  let output_file = Fpath.(odocl_dir // dir / Odoc.index_filename) in
  let libs =
    List.map
      (fun lib ->
        (lib.Packages.lib_name, Fpath.(odocl_dir // lib.odoc_dir)))
      pkg.Packages.libraries
  in
  Odoc.compile_index ~json:false ~output_file ~libs
    ~docs:[ (pkgname, Fpath.(odocl_dir // pkg.mld_odoc_dir)) ]
    ()

let index ~odocl_dir pkgs = Util.StringMap.iter (index_one ~odocl_dir) pkgs

let sherlodoc_index_one ~html_dir ~odocl_dir _ pkg_content =
  let inputs = [ Fpath.(odocl_dir // pkg_content.Packages.pkg_dir / Odoc.index_filename) ] in
  let dst = Fpath.(html_dir // Sherlodoc.db_js_file pkg_content.pkg_dir) in
  let dst_dir, _ = Fpath.split_base dst in
  Util.mkdir_p dst_dir;
  Sherlodoc.index ~format:`js ~inputs ~dst ()

let sherlodoc ~html_dir ~odocl_dir pkgs =
  ignore @@ Bos.OS.Dir.create html_dir;
  Sherlodoc.js Fpath.(html_dir // Sherlodoc.js_file);
  Util.StringMap.iter (sherlodoc_index_one ~html_dir ~odocl_dir) pkgs;
  let format = `marshal in
  let dst = Fpath.(html_dir // Sherlodoc.db_marshal_file) in
  let dst_dir, _ = Fpath.split_base dst in
  Util.mkdir_p dst_dir;
  let inputs =
    pkgs |> Util.StringMap.bindings
    |> List.map (fun (_pkgname, pkg) -> Fpath.(odocl_dir // pkg.Packages.pkg_dir / Odoc.index_filename))
  in
  Sherlodoc.index ~format ~inputs ~dst ()

let html_generate output_dir ~odocl_dir linked =
  let html_generate : linked -> unit =
   fun l ->
    let search_uris =
      [
        Sherlodoc.db_js_file l.pkg_dir;
        Sherlodoc.js_file;
      ]
    in
    let index = Some (Fpath.(odocl_dir // l.pkg_dir / Odoc.index_filename)) in
    Odoc.html_generate ~search_uris ?index
      ~output_dir:(Fpath.to_string output_dir)
      ~input_file:l.output_file ?source:l.src ();
    Atomic.incr Stats.stats.generated_units
  in
  Fiber.List.iter html_generate linked
