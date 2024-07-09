(* compile *)

type ty = Module of Packages.modulety | Mld of Packages.mld

type impl = { impl : Fpath.t; src : Fpath.t }

type pkg_args = {
  docs : (string * Fpath.t) list;
  libs : (string * Fpath.t) list;
}

type compiled = {
  m : ty;
  output_dir : Fpath.t;
  output_file : Fpath.t;
  include_dirs : Fpath.Set.t;
  impl : impl option;
  pkg_args : pkg_args;
  package_name : string;
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

let compile output_dir all =
  let hashes = mk_byhash all in
  let tbl = Hashtbl.create 10 in
  let pkg_args =
    let docs, libs =
      Util.StringMap.fold
        (fun pkgname pkg (docs, libs) ->
          let ( / ) = Fpath.( / ) in
          let doc = (pkgname, output_dir / pkgname / "doc") in
          let lib =
            List.map
              (fun lib ->
                ( lib.Packages.lib_name,
                  output_dir / pkgname / "lib" / lib.lib_name ))
              pkg.Packages.libraries
          in
          let docs = doc :: docs and libs = List.rev_append lib libs in
          (docs, libs))
        all ([], [])
    in
    { docs; libs }
  in

  let compile_one compile_other hash =
    match Util.StringMap.find_opt hash hashes with
    | None ->
        Logs.debug (fun m -> m "Error locating hash: %s" hash);
        Error Not_found
    | Some (package_name, modty) ->
        let deps = modty.m_intf.mif_deps in
        let output_file = Fpath.(output_dir // modty.m_intf.mif_odoc_file) in
        let fibers =
          Fiber.List.map
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
                  Odoc.compile_impl ~output_dir ~input_file:impl.mip_path
                    ~includes ~parent_id:impl.mip_parent_id ~source_id:si.src_id;
                  Atomic.incr Stats.stats.compiled_impls;
                  Some { impl = output_file; src = si.src_path }
              | None -> None)
          | None -> None
        in

        Odoc.compile ~output_dir ~input_file:modty.m_intf.mif_path ~includes
          ~parent_id:modty.m_intf.mif_parent_id;
        Atomic.incr Stats.stats.compiled_units;

        let output_dir = Fpath.split_base output_file |> fst in
        Ok
          {
            m = Module modty;
            output_dir;
            output_file;
            include_dirs = includes;
            impl;
            pkg_args;
            package_name;
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
  let mods =
    List.filter_map (function Ok x -> Some x | Error _ -> None) mod_results
  in
  Util.StringMap.fold
    (fun package_name (pkg : Packages.t) acc ->
      Logs.debug (fun m ->
          m "Package %s mlds: [%a]" pkg.name
            Fmt.(list ~sep:sp Packages.pp_mld)
            pkg.mlds);
      List.fold_left
        (fun acc (mld : Packages.mld) ->
          let output_file = Fpath.(output_dir // mld.Packages.mld_odoc_file) in
          let odoc_output_dir = Fpath.split_base output_file |> fst in
          Odoc.compile ~output_dir ~input_file:mld.mld_path
            ~includes:Fpath.Set.empty ~parent_id:mld.mld_parent_id;
          Atomic.incr Stats.stats.compiled_mlds;
          let include_dirs =
            List.map (fun f -> Fpath.(output_dir // f)) mld.mld_deps
            |> Fpath.Set.of_list
          in
          let include_dirs = Fpath.Set.add odoc_output_dir include_dirs in
          {
            m = Mld mld;
            output_dir;
            output_file;
            include_dirs;
            impl = None;
            pkg_args;
            package_name;
          }
          :: acc)
        acc pkg.mlds)
    all mods

type linked = {
  output_file : Fpath.t;
  src : Fpath.t option;
  package_name : string;
}

let link : compiled list -> _ =
 fun compiled ->
  let link : compiled -> linked list =
   fun c ->
    let includes = Fpath.Set.add c.output_dir c.include_dirs in
    let link input_file =
      let { pkg_args = { libs; docs }; package_name = current_package; _ } =
        c
      in
      Odoc.link ~input_file ~includes ~libs ~docs ~current_package ()
    in
    let impl =
      match c.impl with
      | Some { impl; src } ->
          Logs.debug (fun m -> m "Linking impl: %a" Fpath.pp impl);
          link impl;
          Atomic.incr Stats.stats.linked_impls;
          [
            {
              package_name = c.package_name;
              output_file = Fpath.(set_ext "odocl" impl);
              src = Some src;
            };
          ]
      | None -> []
    in
    match c.m with
    | Module m when m.m_hidden ->
        Logs.debug (fun m -> m "not linking %a" Fpath.pp c.output_file);
        impl
    | _ ->
        Logs.debug (fun m -> m "linking %a" Fpath.pp c.output_file);
        link c.output_file;
        (match c.m with
        | Module _ -> Atomic.incr Stats.stats.linked_units
        | Mld _ -> Atomic.incr Stats.stats.linked_mlds);
        {
          output_file = Fpath.(set_ext "odocl" c.output_file);
          src = None;
          package_name = c.package_name;
        }
        :: impl
  in
  Fiber.List.map link compiled |> List.concat

let odoc_index_path ~odoc_dir pkgname =
  Fpath.(odoc_dir / pkgname / "index.odoc-index")
let sherlodoc_js_index_path_relative_to_html pkgname =
  Fpath.(v pkgname / "sherlodoc_db.js")

let sherlodoc_js_path_relative_to_html = Fpath.v "sherlodoc.js"
let sherlodoc_js_index_path ~html_dir pkgname =
  Fpath.(html_dir // sherlodoc_js_index_path_relative_to_html pkgname)

let sherlodoc_js_path ~html_dir =
  Fpath.(html_dir // sherlodoc_js_path_relative_to_html)

let sherlodoc_marshall_path ~html_dir =
  Fpath.(html_dir / "sherlodoc_db.marshal")
let index_one output_dir pkgname _pkg =
  let dir = Fpath.(output_dir / pkgname) in
  let dst = odoc_index_path ~odoc_dir:output_dir pkgname in
  let include_rec = Fpath.Set.singleton dir in
  Odoc.compile_index ~json:false ~dst ~include_rec ()
let index odoc_dir pkgs = Util.StringMap.iter (index_one odoc_dir) pkgs

let sherlodoc_index_one ~html_dir ~odoc_dir pkgname _pkg_content =
  ignore @@ Bos.OS.Dir.create Fpath.(html_dir / pkgname);
  let format = `js in
  let inputs = [ odoc_index_path ~odoc_dir pkgname ] in
  let dst = sherlodoc_js_index_path ~html_dir pkgname in
  Sherlodoc.index ~format ~inputs ~dst ()

let sherlodoc ~html_dir ~odoc_dir pkgs =
  ignore @@ Bos.OS.Dir.create html_dir;
  Sherlodoc.js (sherlodoc_js_path ~html_dir);
  Util.StringMap.iter (sherlodoc_index_one ~html_dir ~odoc_dir) pkgs;
  let format = `marshal in
  let dst = sherlodoc_marshall_path ~html_dir in
  let inputs =
    pkgs |> Util.StringMap.bindings
    |> List.map (fun (pkgname, _pkg) -> odoc_index_path ~odoc_dir pkgname)
  in
  Sherlodoc.index ~format ~inputs ~dst ()

let compile_sidebars ~odoc_dir ~output_dir all =
  Util.StringMap.map
    (fun (pkg : Packages.t) ->
      let package_name = pkg.name in
      let ( / ) = Fpath.( / ) in
      let libs =
        List.map
          (fun lib ->
            ( lib.Packages.lib_name,
              odoc_dir / package_name / "lib" / lib.lib_name ))
          pkg.Packages.libraries
      in
      let output_file = Fpath.( / ) output_dir package_name in
      Odoc.sidebar
        ~docs:[ (package_name, odoc_dir / package_name / "doc") ]
        ~libs ~output_file ();
      output_file)
    all

let html_generate : Fpath.t -> Fpath.t Util.StringMap.t -> linked list -> _ =
 fun output_dir sidebars linked ->
  let html_generate : linked -> unit =
   fun l ->
    let search_uris =
      [
        sherlodoc_js_index_path_relative_to_html l.package_name;
        sherlodoc_js_path_relative_to_html;
      ]
    in
    let sidebar = Util.StringMap.find_opt l.package_name sidebars in
    Odoc.html_generate ~search_uris ?sidebar
      ~output_dir:(Fpath.to_string output_dir)
      ~input_file:l.output_file ?source:l.src ();
    Atomic.incr Stats.stats.generated_units
  in
  Fiber.List.iter html_generate linked
