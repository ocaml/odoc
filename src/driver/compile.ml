(* compile *)

open Bos

type compiled = Odoc_unit.any

let odoc_partial_filename = "__odoc_partial.m"

let mk_byhash (pkgs : Odoc_unit.any list) =
  List.fold_left
    (fun acc (u : Odoc_unit.any) ->
      match u.Odoc_unit.kind with
      | `Intf { hash; _ } as kind ->
          let elt = { u with kind } in
          Util.StringMap.update hash
            (function None -> Some [ elt ] | Some x -> Some (elt :: x))
            acc
      | _ -> acc)
    Util.StringMap.empty pkgs

let init_stats (units : Odoc_unit.any list) =
  let total, total_impl, non_hidden, mlds, assets, indexes =
    List.fold_left
      (fun (total, total_impl, non_hidden, mlds, assets, indexes)
           (unit : Odoc_unit.any) ->
        let total = match unit.kind with `Intf _ -> total + 1 | _ -> total in
        let total_impl =
          match unit.kind with `Impl _ -> total_impl + 1 | _ -> total_impl
        in
        let assets =
          match unit.kind with `Asset -> assets + 1 | _ -> assets
        in
        let indexes =
          match unit.index with
          | None -> indexes
          | Some index -> Fpath.Set.add index.output_file indexes
        in
        let non_hidden =
          match unit.kind with
          | `Intf { hidden = false; _ } -> non_hidden + 1
          | _ -> non_hidden
        in
        let mlds = match unit.kind with `Mld | `Md -> mlds + 1 | _ -> mlds in
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

type partial = Odoc_unit.intf Odoc_unit.t list Util.StringMap.t

let unmarshal filename : partial =
  let ic = open_in_bin (Fpath.to_string filename) in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Marshal.from_channel ic)

let marshal (v : partial) filename =
  let _ = OS.Dir.create (Fpath.parent filename) |> Result.get_ok in
  let oc = open_out_bin (Fpath.to_string filename) in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> Marshal.to_channel oc v [])

let find_partials odoc_dir :
    Odoc_unit.intf Odoc_unit.t list Util.StringMap.t * _ =
  let tbl = Hashtbl.create 1000 in
  let hashes_result =
    OS.Dir.fold_contents ~dotfiles:false ~elements:`Dirs
      (fun p hashes ->
        let index_m = Fpath.( / ) p odoc_partial_filename in
        match OS.File.exists index_m with
        | Ok true ->
            let hashes' = unmarshal index_m in
            Util.StringMap.iter
              (fun h units ->
                List.iter
                  (fun u ->
                    Hashtbl.replace tbl
                      (h, Odoc.Id.to_string u.Odoc_unit.parent_id)
                      (Promise.create_resolved ()))
                  units)
              hashes';
            Util.StringMap.union (fun _x o1 _o2 -> Some o1) hashes hashes'
        | _ -> hashes)
      Util.StringMap.empty odoc_dir
  in
  match hashes_result with
  | Ok h -> (h, tbl)
  | Error _ -> (* odoc_dir doesn't exist...? *) (Util.StringMap.empty, tbl)

let compile ?partial ~partial_dir (all : Odoc_unit.any list) =
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
    let hashes =
      Odoc_unit.fix_virtual ~precompiled_units:other_hashes ~units:hashes
    in
    let all_hashes =
      Util.StringMap.union (fun _x o1 o2 -> Some (o1 @ o2)) hashes other_hashes
    in
    let compile_one compile_other (unit : Odoc_unit.intf Odoc_unit.t) =
      let (`Intf { Odoc_unit.deps; _ }) = unit.kind in
      let _fibers =
        Fiber.List.map
          (fun (other_unit_name, other_unit_hash) ->
            match compile_other other_unit_hash with
            | Ok r -> Some r
            | Error _exn ->
                Logs.debug (fun m ->
                    m
                      "Error during compilation of module %s (hash %s, \
                       required by %s)"
                      other_unit_name other_unit_hash
                      (Fpath.filename unit.input_file));
                None)
          deps
      in
      let includes =
        List.fold_left
          (fun acc (_lib, path) -> Fpath.Set.add path acc)
          Fpath.Set.empty
          (Odoc_unit.Pkg_args.compiled_libs unit.pkg_args)
      in
      Odoc.compile ~output_dir:unit.output_dir ~input_file:unit.input_file
        ~includes ~warnings_tag:unit.pkgname ~parent_id:unit.parent_id
        ~ignore_output:(not unit.enable_warnings);
      (match unit.input_copy with
      | None -> ()
      | Some p -> Util.cp (Fpath.to_string unit.input_file) (Fpath.to_string p));
      Atomic.incr Stats.stats.compiled_units
    in
    let rec compile_mod : string -> ('a list, [> `Msg of string ]) Result.t =
     fun hash ->
      let map_units =
        Fiber.List.map (fun unit ->
            match
              Hashtbl.find_opt tbl
                (hash, Odoc.Id.to_string unit.Odoc_unit.parent_id)
            with
            | Some p ->
                Promise.await p;
                None
            | None ->
                let p, r = Promise.create () in
                Hashtbl.add tbl (hash, Odoc.Id.to_string unit.parent_id) p;
                let _result = compile_one compile_mod unit in
                Promise.resolve r ();
                Some unit)
      in
      try
        let units = Util.StringMap.find hash all_hashes in
        let r = map_units units in
        Ok (List.filter_map Fun.id r)
      with Not_found ->
        Error (`Msg ("Module with hash " ^ hash ^ " not found"))
    in
    compile_mod
  in

  let compile (unit : Odoc_unit.any) =
    match unit.kind with
    | `Intf intf -> (compile_mod intf.hash :> (Odoc_unit.any list, _) Result.t)
    | `Impl src ->
        let includes =
          List.fold_left
            (fun acc (_lib, path) -> Fpath.Set.add path acc)
            Fpath.Set.empty
            (Odoc_unit.Pkg_args.compiled_libs unit.pkg_args)
        in
        let source_id = src.src_id in
        Odoc.compile_impl ~output_dir:unit.output_dir
          ~input_file:unit.input_file ~includes ~parent_id:unit.parent_id
          ~source_id;
        Atomic.incr Stats.stats.compiled_impls;
        Ok [ unit ]
    | `Asset ->
        Odoc.compile_asset ~output_dir:unit.output_dir ~parent_id:unit.parent_id
          ~name:(Fpath.filename unit.input_file);
        Atomic.incr Stats.stats.compiled_assets;
        Ok [ unit ]
    | `Mld ->
        let includes = Fpath.Set.empty in
        Odoc.compile ~output_dir:unit.output_dir ~input_file:unit.input_file
          ~includes ~warnings_tag:None ~parent_id:unit.parent_id
          ~ignore_output:(not unit.enable_warnings);
        Atomic.incr Stats.stats.compiled_mlds;
        Ok [ unit ]
    | `Md ->
        Odoc.compile_md ~output_dir:unit.output_dir ~input_file:unit.input_file
          ~parent_id:unit.parent_id;
        Atomic.incr Stats.stats.compiled_mlds;
        Ok [ unit ]
  in
  let _ = Fiber.List.map compile all in
  (match partial with
  | Some l -> marshal hashes Fpath.(l / odoc_partial_filename)
  | None -> ());

  all

type linked = Odoc_unit.any

let link : warnings_tags:string list -> custom_layout:bool -> compiled list -> _
    =
 fun ~warnings_tags ~custom_layout compiled ->
  let link : compiled -> linked =
   fun c ->
    let link input_file output_file enable_warnings =
      let libs = Odoc_unit.Pkg_args.compiled_libs c.pkg_args in
      let pages = Odoc_unit.Pkg_args.compiled_pages c.pkg_args in
      let includes = Odoc_unit.Pkg_args.includes c.pkg_args in
      Odoc.link ~custom_layout ~input_file ~output_file ~libs ~docs:pages
        ~includes ~ignore_output:(not enable_warnings) ~warnings_tags
        ?current_package:c.pkgname ()
    in
    match c.kind with
    | `Intf { hidden = true; _ } ->
        Logs.debug (fun m -> m "not linking %a" Fpath.pp c.odoc_file);
        c
    | _ ->
        Logs.debug (fun m -> m "linking %a" Fpath.pp c.odoc_file);
        if c.to_output then link c.odoc_file c.odocl_file c.enable_warnings;
        (match c.kind with
        | `Intf _ -> Atomic.incr Stats.stats.linked_units
        | `Mld -> Atomic.incr Stats.stats.linked_mlds
        | `Asset -> ()
        | `Impl _ -> Atomic.incr Stats.stats.linked_impls
        | `Md -> Atomic.incr Stats.stats.linked_mlds);
        c
  in
  Fiber.List.map link compiled

let sherlodoc_index_one ~output_dir (index : Odoc_unit.index) =
  let inputs = [ index.output_file ] in
  let rel_path = Fpath.(index.search_dir / "sherlodoc_db.js") in
  let dst = Fpath.(output_dir // rel_path) in
  let dst_dir, _ = Fpath.split_base dst in
  let _ = OS.Dir.create dst_dir |> Result.get_ok in
  Sherlodoc.index ~format:`js ~inputs ~dst ();
  rel_path

let html_generate ~occurrence_file ~remaps ~generate_json
    ~simplified_search_output output_dir linked =
  let tbl = Hashtbl.create 10 in
  let _ = OS.Dir.create output_dir |> Result.get_ok in
  Sherlodoc.js Fpath.(output_dir // Sherlodoc.js_file);
  let compile_index : Odoc_unit.index -> _ =
   fun index ->
    let compile_index_one
        ({ roots; output_file; json; search_dir = _; sidebar } as index :
          Odoc_unit.index) =
      let () =
        Odoc.compile_index ~json ~occurrence_file ~output_file ~roots
          ~simplified:false ~wrap:false ()
      in
      let sidebar =
        match sidebar with
        | None -> None
        | Some { output_file; json; pkg_dir } ->
            Odoc.sidebar_generate ~output_file ~json index.output_file ();
            Odoc.sidebar_generate
              ~output_file:Fpath.(output_dir // pkg_dir / "sidebar.json")
              ~json:true index.output_file ();
            if simplified_search_output then
              Odoc.compile_index ~json:true ~occurrence_file
                ~output_file:Fpath.(output_dir // pkg_dir / "index.js")
                ~simplified:true ~wrap:true ~roots ();

            Some output_file
      in
      (sherlodoc_index_one ~output_dir index, sidebar)
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
  let html_generate : Fpath.t option -> linked -> unit =
   fun remap_file l ->
    if l.to_output then
      let output_dir = Fpath.to_string output_dir in
      let home_breadcrumb = "Package index" in
      let input_file = l.odocl_file in
      match l.kind with
      | `Intf { hidden = true; _ } -> ()
      | `Impl { src_path; _ } ->
          let search_uris, sidebar =
            match l.index with
            | None -> (None, None)
            | Some index ->
                let db_path, sidebar = compile_index index in
                let search_uris = [ db_path; Sherlodoc.js_file ] in
                (Some search_uris, sidebar)
          in
          Odoc.html_generate_source ?search_uris ?sidebar ~output_dir
            ~input_file ~home_breadcrumb ~source:src_path ();
          Atomic.incr Stats.stats.generated_units;
          if generate_json then (
            Odoc.html_generate_source ?search_uris ?sidebar ~output_dir
              ~input_file ~source:src_path ~as_json:true ~home_breadcrumb ();
            Atomic.incr Stats.stats.generated_units)
      | `Asset ->
          Odoc.html_generate_asset ~output_dir ~input_file:l.odoc_file
            ~asset_path:l.input_file ~home_breadcrumb ()
      | _ ->
          let search_uris, sidebar =
            match l.index with
            | None -> (None, None)
            | Some index ->
                let db_path, sidebar = compile_index index in
                let search_uris = [ db_path; Sherlodoc.js_file ] in
                (Some search_uris, sidebar)
          in
          Odoc.html_generate ?search_uris ?sidebar ?remap:remap_file ~output_dir
            ~input_file ~home_breadcrumb ();
          Atomic.incr Stats.stats.generated_units;
          if generate_json then (
            Odoc.html_generate ?search_uris ?sidebar ~output_dir ~input_file
              ~as_json:true ~home_breadcrumb ();
            Atomic.incr Stats.stats.generated_units)
  in
  if List.length remaps = 0 then Fiber.List.iter (html_generate None) linked
  else
    Bos.OS.File.with_tmp_oc "remap.%s.txt"
      (fun fpath oc () ->
        List.iter (fun (a, b) -> Printf.fprintf oc "%s:%s\n%!" a b) remaps;
        Fiber.List.iter (html_generate (Some fpath)) linked)
      ()
    |> ignore
