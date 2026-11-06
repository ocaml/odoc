(* Compile, link and render the units of one package. *)

open Bos
open Eio.Std

let init_stats (pkgs : Odoc_unit.pkg list) =
  let units = List.concat_map Odoc_unit.all_units pkgs in
  let output = List.length (List.filter Odoc_unit.is_output units) in
  Stats.expect Compile (List.length units);
  Stats.expect Link output;
  Stats.expect Generate output;
  Stats.expect Index
    (List.length
       (List.filter (fun (p : Odoc_unit.pkg) -> p.index <> None) pkgs))

(* An implementation of a virtual library ships its modules as [.cmt] files
   only: the interface, and the documentation written in it, belong to the
   virtual library's [.cmti], which shares the module's digest. When the
   virtual library is built in the same run, [Packages.remap_virtual] has
   already substituted the [.cmti]; when it was built earlier (voodoo mode),
   the copy stashed next to its [.odoc] file ([input_copy]) is among the
   implementation's transitive dependencies, so look for a same-named [.cmti]
   with the right digest in their directories. *)
let find_virtual_interface (lib : Odoc_unit.lib)
    (unit : Odoc_unit.intf Odoc_unit.t) =
  if not (Fpath.has_ext "cmt" unit.input_file) then unit
  else
    let hash = Odoc_unit.hash unit in
    let name = Fpath.(unit.input_file |> rem_ext |> basename) in
    let candidate dir =
      let cmti = Fpath.(dir / (name ^ ".cmti")) in
      match OS.File.exists cmti with
      | Ok true -> (
          match Odoc.compile_deps cmti with
          | Ok { digest; _ } when digest = hash -> Some cmti
          | _ -> None)
      | _ -> None
    in
    match List.find_map (fun (_, dir) -> candidate dir) lib.lib_includes with
    | None -> unit
    | Some cmti ->
        Logs.debug (fun m ->
            m "Using %a as the interface of %a" Fpath.pp cmti Fpath.pp
              unit.input_file);
        { unit with input_file = cmti }

(* The interfaces of a library, keyed by digest: a virtual library's interface
   and those of its implementations share one. *)
let by_hash (units : [ Odoc_unit.intf | Odoc_unit.impl ] Odoc_unit.t list) =
  List.fold_left
    (fun acc (u : _ Odoc_unit.t) ->
      match u.kind with
      | `Intf _ as kind ->
          let u = { u with kind } in
          Util.StringMap.update (Odoc_unit.hash u)
            (function None -> Some [ u ] | Some x -> Some (u :: x))
            acc
      | `Impl _ -> acc)
    Util.StringMap.empty units

let compile_lib (lib : Odoc_unit.lib) =
  let hashes = by_hash lib.units in
  let compile_intf (unit : Odoc_unit.intf Odoc_unit.t) =
    let unit = find_virtual_interface lib unit in
    List.iter Cmd_outputs.run
      (Commands.compile_module lib (unit :> Odoc_unit.module_unit));
    Stats.did Compile
  in
  (* A module is compiled after the modules it imports. [compile_mod] on a
     digest compiles the interfaces with that digest, once. An import whose
     digest is not among this library's modules belongs to a library compiled
     earlier, and is found among the [-L] directories. *)
  let compile_mod =
    Util.memo ~key:Fun.id @@ fun compile_mod hash ->
    match Util.StringMap.find_opt hash hashes with
    | None -> ()
    | Some units ->
        Fiber.List.iter
          (fun (unit : Odoc_unit.intf Odoc_unit.t) ->
            Fiber.List.iter compile_mod (Odoc_unit.deps unit);
            compile_intf unit)
          units
  in
  (* An implementation imports interfaces it records no digest for here, so
     it waits for all of the library's (D5). *)
  let intfs, impls =
    List.partition
      (fun (u : Odoc_unit.module_unit) ->
        match u.kind with `Intf _ -> true | `Impl _ -> false)
      lib.units
  in
  Fiber.List.iter
    (fun (u : Odoc_unit.module_unit) ->
      match u.kind with `Intf { hash; _ } -> compile_mod hash | `Impl _ -> ())
    intfs;
  Fiber.List.iter
    (fun unit ->
      List.iter Cmd_outputs.run (Commands.compile_module lib unit);
      Stats.did Compile)
    impls

let compile_pages (pkg : Odoc_unit.pkg) =
  let compile (unit : Odoc_unit.page) =
    Cmd_outputs.run (Commands.compile_page unit);
    Stats.did Compile
  in
  let lib_pages =
    List.filter_map (fun (l : Odoc_unit.lib) -> l.page) pkg.libs
  in
  Fiber.List.iter compile (pkg.pages @ (lib_pages :> Odoc_unit.page list))

let link ~warnings_tags (pkg : Odoc_unit.pkg) =
  let link (u : Odoc_unit.any) =
    if Odoc_unit.is_output u then (
      Cmd_outputs.run (Commands.link ~warnings_tags pkg u);
      Stats.did Link)
  in
  Fiber.List.iter link (Odoc_unit.all_units pkg)

(* The index of a package is built from the [.odocl] files of its linked units.
   Listing them explicitly puts the package's pages and modules in one hierarchy
   even though they live in different directories. *)
let index_file_list (pkg : Odoc_unit.pkg) (index : Odoc_unit.index) =
  let inputs = Odoc_unit.index_inputs pkg in
  let file_list = Fpath.(parent index.index_file / "index-inputs.txt") in
  Util.with_out_to file_list (fun oc ->
      List.iter (fun f -> Printf.fprintf oc "%s\n" (Fpath.to_string f)) inputs)
  |> Result.get_ok;
  file_list

(* The files every page relies on: odoc's support files and sherlodoc's
   runtime. *)
let html_support html_dir =
  let _ = OS.Dir.create html_dir |> Result.get_ok in
  Cmd_outputs.run @@ Sherlodoc.js Fpath.(html_dir // Sherlodoc.js_file);
  ignore (Cmd_outputs.run @@ Odoc.support_files html_dir)

let with_remaps remaps f =
  match remaps with
  | [] -> f None
  | remaps ->
      OS.File.with_tmp_oc "remap.%s.txt"
        (fun fpath oc () ->
          List.iter (fun (a, b) -> Printf.fprintf oc "%s:%s\n%!" a b) remaps;
          f (Some fpath))
        ()
      |> Result.get_ok

let generate ?remap_file ~generate_json html_dir (pkg : Odoc_unit.pkg) =
  (* The package's index, sidebar and search database, which its pages then
     point at. *)
  (match pkg.index with
  | None -> ()
  | Some index ->
      let file_list = index_file_list pkg index in
      let _ =
        OS.Dir.create Fpath.(html_dir // index.html_dir) |> Result.get_ok
      in
      let compile_index, from_index =
        Commands.index ~html_dir ~file_list index
      in
      Cmd_outputs.run compile_index;
      List.iter Cmd_outputs.run from_index;
      Stats.did Index);
  let generate (u : Odoc_unit.any) =
    match Commands.generate ~html_dir ?remap_file ~generate_json pkg u with
    | [] -> ()
    | actions ->
        List.iter Cmd_outputs.run actions;
        Stats.did Generate
  in
  Fiber.List.iter generate (Odoc_unit.all_units pkg)

(* The JSON search index of a package, for ocaml.org. It is the only consumer
   of the occurrence counts, which is why it is a separate, final step. *)
let json_index ~occurrence_file html_dir (pkg : Odoc_unit.pkg) =
  match pkg.index with
  | None -> ()
  | Some ({ html_dir = pkg_html; _ } as index) ->
      let file_list = index_file_list pkg index in
      Cmd_outputs.run
      @@ Odoc.compile_index ~json:true ~occurrence_file
           ~output_file:Fpath.(html_dir // pkg_html / "index.js")
           ~simplified:true ~wrap:true ~file_list ()
