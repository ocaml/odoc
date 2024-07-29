(* compile *)

type compiled = Odoc_unit.t

let mk_byhash (pkgs : Odoc_unit.intf Odoc_unit.unit list) =
  List.fold_left
    (fun acc (u : Odoc_unit.intf Odoc_unit.unit) ->
      match u.Odoc_unit.kind with
      | `Intf { hash; _ } -> Util.StringMap.add hash u acc)
    Util.StringMap.empty pkgs

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
  (string * Odoc_unit.intf Odoc_unit.unit) list
  * Odoc_unit.intf Odoc_unit.unit Util.StringMap.t

let unmarshal filename : partial =
  let ic = open_in_bin (Fpath.to_string filename) in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Marshal.from_channel ic)

let marshal (v : partial) filename =
  let p = Fpath.parent filename in
  Util.mkdir_p p;
  let oc = open_out_bin (Fpath.to_string filename) in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> Marshal.to_channel oc v [])

let find_partials odoc_dir : Odoc_unit.intf Odoc_unit.unit Util.StringMap.t * _
    =
  let tbl = Hashtbl.create 1000 in
  let hashes_result =
    Bos.OS.Dir.fold_contents ~dotfiles:false ~elements:`Dirs
      (fun p hashes ->
        let index_m = Fpath.( / ) p "index.m" in
        match Bos.OS.File.exists index_m with
        | Ok true ->
            let tbl', hashes' = unmarshal index_m in
            List.iter
              (fun (k, v) ->
                Hashtbl.replace tbl k (Promise.create_resolved (Ok v)))
              tbl';
            Util.StringMap.union (fun _x o1 _o2 -> Some o1) hashes hashes'
        | _ -> hashes)
      Util.StringMap.empty odoc_dir
  in
  match hashes_result with
  | Ok h -> (h, tbl)
  | Error _ -> (* odoc_dir doesn't exist...? *) (Util.StringMap.empty, tbl)

let compile ?partial ~output_dir ?linked_dir:_ (all : Odoc_unit.t list) =
  (* let linked_dir = Option.value linked_dir ~default:output_dir in *)
  let intf_units, impl_units, mld_units =
    List.fold_left
      (fun (intf_units, impl_units, page_units) (unit : Odoc_unit.t) ->
        match unit with
        | { kind = `Intf _; _ } as intf ->
            (intf :: intf_units, impl_units, page_units)
        | { kind = `Impl; _ } as impl ->
            (intf_units, impl :: impl_units, page_units)
        | { kind = `Mld; _ } as mld ->
            (intf_units, impl_units, mld :: page_units))
      ([], [], []) all
  in
  let hashes = mk_byhash intf_units in
  let other_hashes, tbl =
    match partial with
    | Some _ -> find_partials output_dir
    | None -> (Util.StringMap.empty, Hashtbl.create 10)
  in
  let all_hashes =
    Util.StringMap.union (fun _x o1 _o2 -> Some o1) hashes other_hashes
  in
  let compile_one compile_other hash =
    match Util.StringMap.find_opt hash all_hashes with
    | None ->
        Logs.debug (fun m -> m "Error locating hash: %s" hash);
        Error Not_found
    | Some unit ->
        let deps = match unit.kind with `Intf { deps; _ } -> deps in
        let _fibers =
          Fiber.List.map
            (fun other_unit ->
              match compile_other other_unit with
              | Ok r -> Some r
              | Error _exn ->
                  Logs.debug (fun m ->
                      m "Missing module %s (hash %s, required by %s)" "TODO"
                        (* n h *) "TODO" "TODO" (* unit.m_name *));
                  None)
            deps
        in

        (* let includes = Fpath.Set.add output_dir includes in ?????????? *)

        (* TOOOODOOOOO *)
        (* let impl = *)
        (*   match modty.m_impl with *)
        (*   | Some impl -> ( *)
        (*       match impl.mip_src_info with *)
        (*       | Some si -> *)
        (*           let odoc_file = Fpath.(output_dir // impl.mip_odoc_file) in *)
        (*           let odocl_file = Fpath.(linked_dir // impl.mip_odocl_file) in *)
        (*           Odoc.compile_impl ~output_dir ~input_file:impl.mip_path *)
        (*             ~includes ~parent_id:impl.mip_parent_id ~source_id:si.src_id; *)
        (*           Atomic.incr Stats.stats.compiled_impls; *)
        (*           Some *)
        (*             { *)
        (*               impl_odoc = odoc_file; *)
        (*               impl_odocl = odocl_file; *)
        (*               src = si.src_path; *)
        (*             } *)
        (*       | None -> None) *)
        (*   | None -> None *)
        (* in *)
        let includes = Fpath.Set.of_list unit.include_dirs in
        Odoc.compile ~output_dir:unit.output_dir ~input_file:unit.input_file
          ~includes ~parent_id:unit.parent_id;
        Atomic.incr Stats.stats.compiled_units;

        Ok unit
  in

  let rec compile_mod :
      Odoc_unit.intf Odoc_unit.unit ->
      (Odoc_unit.intf Odoc_unit.unit, exn) Result.t =
   fun unit ->
    let hash = match unit.kind with `Intf { hash; _ } -> hash in
    match Hashtbl.find_opt tbl hash with
    | Some p -> Promise.await p
    | None ->
        let p, r = Promise.create () in
        Hashtbl.add tbl hash p;
        let result = compile_one compile_mod hash in
        Promise.resolve r result;
        result
  in
  let to_build = Util.StringMap.bindings hashes |> List.map snd in
  let mod_results = Fiber.List.map compile_mod to_build in
  let compile_mld (unit : Odoc_unit.mld Odoc_unit.unit) =
    let includes = Fpath.Set.of_list unit.include_dirs in
    Odoc.compile ~output_dir:unit.output_dir ~input_file:unit.input_file
      ~includes ~parent_id:unit.parent_id;
    Atomic.incr Stats.stats.compiled_mlds
  in
  let _compiled_mlds = Fiber.List.map compile_mld mld_units in
  let compile_impl (unit : Odoc_unit.impl Odoc_unit.unit) =
    let includes = Fpath.Set.of_list unit.include_dirs in
    Odoc.compile ~output_dir:unit.output_dir ~input_file:unit.input_file
      ~includes ~parent_id:unit.parent_id;
    Atomic.incr Stats.stats.compiled_impls
  in
  let _compiled_impls = Fiber.List.map compile_impl impl_units in
  let zipped_res =
    List.map2
      (fun Odoc_unit.{ kind = `Intf { hash; _ }; _ } b -> (hash, b))
      to_build mod_results
  in
  let zipped =
    List.filter_map (function a, Ok b -> Some (a, b) | _ -> None) zipped_res
  in
  (match partial with
  | Some l -> marshal (zipped, hashes) Fpath.(l / "index.m")
  | None -> ());
  all

type linked = Odoc_unit.t

let link : compiled list -> _ =
 fun compiled ->
  let link : compiled -> linked =
   fun c ->
    let link input_file output_file =
      let { Odoc_unit.libs; pages } = c.pkg_args in
      let includes = c.include_dirs |> Fpath.Set.of_list in
      Odoc.link ~input_file ~output_file ~includes ~libs ~docs:pages
        ~current_package:c.pkgname ()
    in
    match c.kind with
    | `Intf { hidden = true; _ } ->
        Logs.debug (fun m -> m "not linking %a" Fpath.pp c.odoc_file);
        c
    | _ ->
        Logs.debug (fun m -> m "linking %a" Fpath.pp c.odoc_file);
        link c.odoc_file c.odocl_file;
        (match c.kind with
        | `Intf _ -> Atomic.incr Stats.stats.linked_units
        | `Mld -> Atomic.incr Stats.stats.linked_mlds
        | `Impl -> Atomic.incr Stats.stats.linked_impls);
        c
  in
  Fiber.List.map link compiled

(* let index_one ~odocl_dir pkg_name pkg = *)
(*   let dir = pkg.Packages.pkg_dir in *)
(*   let output_file = Fpath.(odocl_dir // dir / Odoc.index_filename) in *)
(*   let libs = *)
(*     List.map *)
(*       (fun lib -> (lib.Packages.lib_name, Fpath.(odocl_dir // lib.odoc_dir))) *)
(*       pkg.Packages.libraries *)
(*   in *)
(*   Odoc.compile_index ~json:false ~output_file ~libs *)
(*     ~docs:[ (pkg_name, Fpath.(odocl_dir // pkg.mld_odoc_dir)) ] *)
(*     () *)

(* let index ~odocl_dir pkgs = Util.StringMap.iter (index_one ~odocl_dir) pkgs *)

(* let sherlodoc_index_one ~html_dir ~odocl_dir _ pkg_content = *)
(*   let inputs = *)
(*     [ Fpath.(odocl_dir // pkg_content.Packages.pkg_dir / Odoc.index_filename) ] *)
(*   in *)
(*   let dst = Fpath.(html_dir // Sherlodoc.db_js_file pkg_content.pkg_dir) in *)
(*   let dst_dir, _ = Fpath.split_base dst in *)
(*   Util.mkdir_p dst_dir; *)
(*   Sherlodoc.index ~format:`js ~inputs ~dst () *)

(* let sherlodoc ~html_dir ~odocl_dir pkgs = *)
(*   ignore @@ Bos.OS.Dir.create html_dir; *)
(*   Sherlodoc.js Fpath.(html_dir // Sherlodoc.js_file); *)
(*   Util.StringMap.iter (sherlodoc_index_one ~html_dir ~odocl_dir) pkgs; *)
(*   let format = `marshal in *)
(*   let dst = Fpath.(html_dir // Sherlodoc.db_marshal_file) in *)
(*   let dst_dir, _ = Fpath.split_base dst in *)
(*   Util.mkdir_p dst_dir; *)
(*   let inputs = *)
(*     pkgs |> Util.StringMap.bindings *)
(*     |> List.map (fun (_pkgname, pkg) -> *)
(*            Fpath.(odocl_dir // pkg.Packages.pkg_dir / Odoc.index_filename)) *)
(*   in *)
(*   Sherlodoc.index ~format ~inputs ~dst () *)

let html_generate output_dir (* ~odocl_dir *) linked =
  let html_generate : linked -> unit =
   fun l ->
    match l.kind with
    | `Intf { hidden = true; _ } -> ()
    | _ ->
        (* let pkg_dir = l. in *)
        (* let search_uris = [ Sherlodoc.db_js_file pkg_dir; Sherlodoc.js_file ] in *)
        (* let index = Some Fpath.(odocl_dir // pkg_dir / Odoc.index_filename) in *)
        Odoc.html_generate ~search_uris:[] ?index:None
          ~output_dir:(Fpath.to_string output_dir)
          ~input_file:l.odocl_file ?source:None (* l.src *) ();
        Atomic.incr Stats.stats.generated_units
  in
  Fiber.List.iter html_generate linked
