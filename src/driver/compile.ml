(* compile *)

type compiled = Odoc_unit.t

let mk_byhash (pkgs : Odoc_unit.t list) =
  List.fold_left
    (fun acc (u : Odoc_unit.t) ->
      match u.Odoc_unit.kind with
      | `Intf { hash; _ } as kind -> Util.StringMap.add hash { u with kind } acc
      | _ -> acc)
    Util.StringMap.empty pkgs

let init_stats (units : Odoc_unit.t list) =
  let total, total_impl, non_hidden, mlds, assets, indexes =
    List.fold_left
      (fun (total, total_impl, non_hidden, mlds, assets, indexes)
           (unit : Odoc_unit.t) ->
        let total = match unit.kind with `Intf _ -> total + 1 | _ -> total in
        let total_impl =
          match unit.kind with `Impl _ -> total_impl + 1 | _ -> total_impl
        in
        let assets =
          match unit.kind with `Asset -> assets + 1 | _ -> assets
        in
        let indexes = Fpath.Set.add unit.index.output_file indexes in
        let non_hidden =
          match unit.kind with
          | `Intf { hidden = false; _ } -> non_hidden + 1
          | _ -> non_hidden
        in
        let mlds = match unit.kind with `Mld -> mlds + 1 | _ -> mlds in
        (total, total_impl, non_hidden, mlds, assets, indexes))
      (0, 0, 0, 0, 0, Fpath.Set.empty)
      units
  in

  Atomic.set Stats.stats.total_units total;
  Atomic.set Stats.stats.total_impls total_impl;
  Atomic.set Stats.stats.non_hidden_units non_hidden;
  Atomic.set Stats.stats.total_mlds mlds;
  Atomic.set Stats.stats.total_assets assets;
  Atomic.set Stats.stats.total_indexes (Fpath.Set.cardinal indexes)

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

let compile ?partial ~partial_dir ?linked_dir:_ (all : Odoc_unit.t list) =
  let hashes = mk_byhash all in
  let compile_mod =
    (* Modules have a more complicated compilation because:
       - They have dependencies and must be compiled in the right order
       - In Voodoo mode, there might exists already compiled parts *)
    let other_hashes, tbl =
      match partial with
      | Some _ -> find_partials partial_dir
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
              (fun (other_unit : Odoc_unit.intf Odoc_unit.unit) ->
                match compile_other other_unit with
                | Ok r -> Some r
                | Error _exn ->
                    Logs.debug (fun m ->
                        m
                          "Error during compilation of module %s (hash %s, \
                           required by %s)"
                          (Fpath.filename other_unit.input_file)
                          (match other_unit.kind with
                          | `Intf { hash; _ } -> hash)
                          (Fpath.filename unit.input_file));
                    None)
              deps
          in
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
    compile_mod
  in

  let compile (unit : Odoc_unit.t) =
    match unit.kind with
    | `Intf _ as kind ->
        (compile_mod { unit with kind } :> (Odoc_unit.t, _) Result.t)
    | `Impl src ->
        let includes = Fpath.Set.of_list unit.include_dirs in
        let source_id = src.src_id in
        Odoc.compile_impl ~output_dir:unit.output_dir
          ~input_file:unit.input_file ~includes ~parent_id:unit.parent_id
          ~source_id;
        Atomic.incr Stats.stats.compiled_impls;
        Ok unit
    | `Asset ->
        Odoc.compile_asset ~output_dir:unit.output_dir ~parent_id:unit.parent_id
          ~name:(Fpath.filename unit.input_file);
        Atomic.incr Stats.stats.compiled_assets;
        Ok unit
    | `Mld ->
        let includes = Fpath.Set.of_list unit.include_dirs in
        Odoc.compile ~output_dir:unit.output_dir ~input_file:unit.input_file
          ~includes ~parent_id:unit.parent_id;
        Atomic.incr Stats.stats.compiled_mlds;
        Ok unit
  in
  let res = Fiber.List.map compile all in
  (* For voodoo mode, we need to keep which modules successfully compiled *)
  let zipped =
    List.filter_map
      (function
        | Ok (Odoc_unit.{ kind = `Intf { hash; _ }; _ } as b) -> Some (hash, b)
        | _ -> None)
      res
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
        | `Asset -> ()
        | `Impl _ -> Atomic.incr Stats.stats.linked_impls);
        c
  in
  Fiber.List.map link compiled

let sherlodoc_index_one ~output_dir (index : Odoc_unit.index) =
  let inputs = [ index.output_file ] in
  let rel_path = Fpath.(index.search_dir / "sherlodoc_db.js") in
  let dst = Fpath.(output_dir // rel_path) in
  let dst_dir, _ = Fpath.split_base dst in
  Util.mkdir_p dst_dir;
  Sherlodoc.index ~format:`js ~inputs ~dst ();
  rel_path

let html_generate output_dir linked =
  let tbl = Hashtbl.create 10 in
  Sherlodoc.js Fpath.(output_dir // Sherlodoc.js_file);
  let compile_index : Odoc_unit.index -> _ =
   fun index ->
    let compile_index_one
        ({ pkg_args = { pages; libs }; output_file; json; search_dir = _ } as
         index :
          Odoc_unit.index) =
      let () = Odoc.compile_index ~json ~output_file ~libs ~docs:pages () in
      sherlodoc_index_one ~output_dir index
    in
    match Hashtbl.find_opt tbl index.output_file with
    | None ->
        let p, r = Promise.create () in
        Hashtbl.add tbl index.output_file p;
        let rel_path = compile_index_one index in
        Atomic.incr Stats.stats.generated_indexes;
        Promise.resolve r rel_path;
        rel_path
    | Some p -> Promise.await p
  in
  let html_generate : linked -> unit =
   fun l ->
    let output_dir = Fpath.to_string output_dir in
    let input_file = l.odocl_file in
    match l.kind with
    | `Intf { hidden = true; _ } -> ()
    | `Impl { src_path; _ } ->
        Odoc.html_generate_source ~search_uris:[] ~output_dir ~input_file
          ~source:src_path ();
        Atomic.incr Stats.stats.generated_units
    | `Asset ->
        Odoc.html_generate_asset ~output_dir ~input_file:l.odoc_file
          ~asset_path:l.input_file ()
    | _ ->
        let db_path = compile_index l.index in
        let search_uris = [ db_path; Sherlodoc.js_file ] in
        let index = l.index.output_file in
        Odoc.html_generate ~search_uris ~index ~output_dir ~input_file ();
        Atomic.incr Stats.stats.generated_units
  in
  Fiber.List.iter html_generate linked
