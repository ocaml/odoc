(* Odoc driver *)

(* Output hierarchy:

   /<pkg name>/doc/<manual pages>
   /<pkg name>/lib/<library>/Module/index.html
   /<pkg name>/src/...
*)

(* open Bos
   let ( >>= ) = Result.bind
   let ( >>|= ) m f = m >>= fun x -> Ok (f x)
   let get_ok = function Ok x -> x | Error (`Msg m) -> failwith m
   let relativize p = Fpath.(v ".." // p) *)
(*
(* this driver is run from the [doc] dir *)

let dep_libraries_core =
  [
    "odoc-parser";
    "astring";
    "cmdliner";
    "fpath";
    "result";
    "tyxml";
    "fmt";
    "stdlib";
    "yojson";
  ]

let extra_deps =
  [
    "base";
    "base_bigstring";
    "base_quickcheck";
    "bin_prot";
    "camlp-streams";
    "core";
    "fieldslib";
    "int_repr";
    "ocaml-compiler-libs";
    "parsexp";
    "ppx_bench.runtime-lib";
    "ppx_compare";
    "ppx_enumerate";
    "ppx_expect";
    "ppx_expect.collector";
    "ppx_expect.common";
    "ppx_expect.config";
    "ppx_expect.config_types";
    "ppx_expect.evaluator";
    "ppx_expect.make_corrected_file";
    "ppx_expect.matcher";
    "ppx_expect.payload";
    "ppx_hash";
    "ppx_inline_test.config";
    "ppx_inline_test.runtime-lib";
    "ppx_module_timer";
    "ppx_sexp_conv";
    "ppx_stable_witness";
    "ppx_stable_witness.runtime";
    "ppx_stable_witness.stable_witness";
    "ppxlib";
    "ppxlib.ast";
    "ppxlib.astlib";
    "ppxlib.traverse_builtins";
    "sexplib";
    "sexplib0";
    "splittable_random";
    "stdio";
    "typerep";
    "variantslib";
  ]

let dep_libraries =
  match Sys.getenv_opt "ODOC_BENCHMARK" with
  | Some "true" -> dep_libraries_core @ extra_deps
  | _ -> dep_libraries_core

let odoc_libraries =
  [
    "odoc_xref_test";
    "odoc_xref2";
    "odoc_odoc";
    "odoc_html_support_files";
    "odoc_model_desc";
    "odoc_model";
    "odoc_manpage";
    "odoc_loader";
    "odoc_latex";
    "odoc_html";
    "odoc_document";
    "odoc_examples";
    "odoc_parser";
    "ocamlary";
    "odoc_search";
    "odoc_html_frontend";
    "odoc_json_index";
    "syntax_highlighter";
    "type_desc_to_yojson";
  ] *)

(* let all_libraries = dep_libraries @ odoc_libraries *)

(* let extra_docs =
   [
     "interface";
     "driver";
     "parent_child_spec";
     "features";
     "odoc_for_authors";
     "dune";
     "ocamldoc_differences";
     "api_reference";
   ] *)

(* let parents =
     let add_parent p l = List.map (fun lib -> (lib, p)) l in
     add_parent "deps" dep_libraries @ add_parent "odoc" odoc_libraries

   let ocamlfind = Cmd.v "ocamlfind"

   let reach t ~from =
     let rec loop t from =
       match (t, from) with
       | a :: t, b :: from when a = b -> loop t from
       | _ -> List.fold_right (fun _ acc -> ".." :: acc) from t
     in
     let v s = String.split_on_char '/' s in
     loop (v t) (v from) |> String.concat "/"

   let relativize_path =
     let pwd = Sys.getcwd () in
     fun p -> reach p ~from:pwd

   let lib_path env lib =
     let cmd = Cmd.(ocamlfind % "query" % lib) in
     Run.run env cmd |> List.hd |> relativize_path

   let _lib_paths env =
     List.fold_right (fun lib acc -> (lib, lib_path env lib) :: acc) dep_libraries []

   let _find_units p =
     OS.Dir.fold_contents ~dotfiles:true
       (fun p acc ->
         if List.exists (fun ext -> Fpath.has_ext ext p) [ "cmt"; "cmti"; "cmi" ]
         then p :: acc
         else acc)
       [] (Fpath.v p)
     >>|= fun paths ->
     let l = List.map Fpath.rem_ext paths in
     let l =
       List.filter
         (fun f ->
           not @@ Astring.String.is_infix ~affix:"ocamldoc" (Fpath.to_string f))
         l
     in
     List.fold_right Fpath.Set.add l Fpath.Set.empty

   let _best_file base =
     List.map (fun ext -> Fpath.add_ext ext base) [ "cmti"; "cmt"; "cmi" ]
     |> List.find (fun f -> Bos.OS.File.exists f |> get_ok)

   let _is_hidden path = Astring.String.is_infix ~affix:"__" (Fpath.to_string path)

   type unit = {
     file : Fpath.t;
     ignore_output : bool;
     source : Fpath.t option;
     assets : string list;
   }

   (* let odoc_source_tree = Fpath.v "srctree-source.odoc" *)

   let _source_dir_of_odoc_lib lib =
     match String.split_on_char '_' lib with
     | "odoc" :: s ->
         let libname = Fpath.(v (String.concat "_" s)) in
         Some Fpath.(v "src" // libname)
     | _ -> None *)

(* let source_files_of_odoc_module lib module_ =
   let filename =
     let module_ =
       match Astring.String.cut ~rev:true ~sep:"__" module_ with
       | None -> module_
       | Some (_, "") -> module_
       | Some (_, module_) -> module_
     in
     (* ML.ml should not be renamed *)
     if String.for_all (fun c -> Char.equal (Char.uppercase_ascii c) c) module_
     then module_
     else String.uncapitalize_ascii module_
   in
   match source_dir_of_odoc_lib lib with
   | None -> None
   | Some relpath ->
       let add_filename path ext =
         Fpath.( / ) path filename |> Fpath.add_ext ext
       in
       let find_by_extension path exts =
         exts
         |> List.map (fun ext -> add_filename path ext)
         |> List.find_opt (fun f -> Bos.OS.File.exists (relativize f) |> get_ok)
       in
       find_by_extension relpath [ "pp.ml"; "ml"; "ml-gen" ] *)

(* let compile_source_tree env units =
   let sources =
     List.filter_map
       (fun (_, _, _, file) -> Option.map Fpath.to_string file)
       units
   in
   let source_map = Fpath.v "source.map" in
   let () = Bos.OS.File.write_lines source_map sources |> get_ok in
   let () =
     Odoc.source_tree env ~parent:"odoc" ~output:odoc_source_tree source_map
   in
   { file = odoc_source_tree; ignore_output = false; source = None; assets = [] } *)

(* let odoc_units () =
   let odoc_all_unit_paths = find_units ".." |> get_ok in
   List.map
     (fun lib ->
       Fpath.Set.fold
         (fun p acc ->
           if Astring.String.is_infix ~affix:lib (Fpath.to_string p) then
             let impl =
               let module_ = Fpath.basename p in
               source_files_of_odoc_module lib module_
             in
             ("odoc", lib, p, impl) :: acc
           else acc)
         odoc_all_unit_paths [])
     odoc_libraries *)

(* let all_units () =
   let lib_units =
     List.map
       (fun (lib, p) ->
         Fpath.Set.fold
           (fun p acc -> ("deps", lib, p, None) :: acc)
           (find_units p |> get_ok)
           [])
       lib_paths
   in
   odoc_units () @ lib_units |> List.flatten *)

(* let update_api_reference_page () =
     let libs =
       List.sort String.compare odoc_libraries |> List.map String.capitalize_ascii
     in
     OS.File.with_oc
       (Fpath.v "api_reference.mld")
       (fun oc () ->
         let pf = Printf.fprintf in
         pf oc "{0 API Reference}\n\n";
         List.iter (pf oc "- {!%s}\n") libs;
         Ok ())
       ()
     |> get_ok |> get_ok

   let search_file = "index.js" *)
(*
   let compile_mlds env all_units =
     update_api_reference_page ();
     let mkpage x = "page-\"" ^ x ^ "\"" in
     let mkmod x = "module-" ^ String.capitalize_ascii x in
     let mkmld x =
       let f = Fpath.(add_ext "mld" (v x)) in
       if not (Bos.OS.File.exists f |> get_ok) then
         Bos.OS.File.write_lines f [ Printf.sprintf "{0 %s}" x ] |> get_ok;
       f
     in
     ignore
       (Odoc.compile env (mkmld "odoc")
          ("srctree-source" :: "page-deps"
          :: List.map mkpage (odoc_libraries @ extra_docs)));
     ignore
       (Odoc.compile env (mkmld "deps") ~parent:"odoc" (List.map mkpage dep_libraries));
     let extra_odocs =
       List.map
         (fun p ->
           ignore (Odoc.compile env (mkmld p) ~parent:"odoc" []);
           "page-" ^ p ^ ".odoc")
         extra_docs
     in
     let odocs =
       List.map
         (fun library ->
           let parent = List.assoc library parents in
           let children =
             List.filter_map
               (fun (_, lib, child, _) ->
                 if lib = library then Some (Fpath.basename child |> mkmod)
                 else None)
               all_units
           in
           ignore
             (Odoc.compile env (mkmld ("library_mlds/" ^ library)) ~parent children);
           "page-" ^ library ^ ".odoc")
         all_libraries
     in
     {
       file = Fpath.v "page-odoc.odoc";
       ignore_output = false;
       source = None;
       assets = [];
     }
     :: List.map
          (fun f ->
            { file = Fpath.v f; ignore_output = false; source = None; assets = [] })
          (("page-deps.odoc" :: odocs) @ extra_odocs)

   let _compile_all env all_units =
     let mld_odocs = compile_mlds env all_units in
     let source_tree = compile_source_tree env all_units in
     let compile_src file ~ignore_output source_args () =
       match source_args with
       | None -> ()
       | Some source_name ->
           Odoc.compile_src env (Fpath.set_ext "cmt" file) ~source_name ~ignore_output
             ~source_parent_file:odoc_source_tree ()
     in
     let rec rec_compile ?impl parent lib file =
       let output = Fpath.(base (set_ext "odoc" file)) in
       if OS.File.exists output |> get_ok then []
       else
         let deps = Odoc.compile_deps env file |> get_ok in
         ignore deps.digest;
         let files =
           List.fold_left
             (fun acc (dep_name, _digest) ->
               match
                 List.find_opt
                   (fun (_, _, f, _) ->
                     Fpath.basename f |> String.capitalize_ascii = dep_name)
                   all_units
               with
               | None -> acc
               | Some (parent, lib, dep_path, impl) ->
                   let file = best_file dep_path in
                   rec_compile ?impl parent lib file @ acc)
             [] deps.deps
         in
         let ignore_output = parent = "deps" in
         compile_src file impl ~ignore_output ();
         Odoc.compile env file ~parent:lib ~ignore_output [];
         { file = output; ignore_output; source = impl; assets = [] } :: files
     in
     source_tree
     :: List.fold_left
          (fun acc (parent, lib, dep, impl) ->
            acc @ rec_compile ?impl parent lib (best_file dep))
          [] all_units
     @ mld_odocs *)

(* let src_file file =
     let fdir, fname = Fpath.split_base file in
     let fname = Fpath.v ("src-" ^ Fpath.to_string fname) in
     Fpath.( // ) fdir fname
   let _link_all env odoc_files =
     List.map
       (fun ({ file = odoc_file; ignore_output; source; _ } as unit) ->
         if Option.is_some source then
           ignore (Odoc.link env ~ignore_output (src_file odoc_file));
         ignore (Odoc.link env ~ignore_output odoc_file);
         { unit with file = Fpath.set_ext "odocl" odoc_file })
       odoc_files

   let _generate_all env odocl_files =
     let search_uris = [ Fpath.v "minisearch.js"; Fpath.v "index.js" ] in
     List.iter
       (fun { file; ignore_output = _; source; assets } ->
         ignore (Odoc.html_generate env ~assets ~search_uris file None);
         match source with
         | None -> ()
         | Some source ->
             ignore (Odoc.html_generate env (src_file file) (Some (relativize source))))
       odocl_files;
     Odoc.support_files env *)

(* let index_generate ?(ignore_output = false) () =
   let open Cmd in
   let files =
     OS.Dir.contents (Fpath.v ".")
     |> get_ok
     |> List.filter (Fpath.has_ext "odocl")
     |> List.filter (fun p ->
            String.starts_with ~prefix:"src-" (Fpath.filename p))
     |> List.filter (fun p -> not (is_hidden p))
     |> List.map Fpath.to_string
   in
   let index_map = Fpath.v "index.map" in
   let () = Bos.OS.File.write_lines index_map files |> get_ok in
   let cmd =
     Odoc.odoc % "compile-index" % "-o" % "html/index.json" % "--file-list"
     % p index_map
   in
   let lines = Run.run cmd in
   if not ignore_output then
     Odoc.add_prefixed_output cmd Odoc.generate_output "index compilation" lines *)

(* let _js_index () =
   let index = Bos.OS.File.read Fpath.(v "html" / "index.json") |> get_ok in
   Bos.OS.File.writef (Fpath.v search_file)
     {|
                let documents =
                  %s
                ;

                let miniSearch = new MiniSearch({
                 fields: ['id', 'doc', 'entry_id'], // fields to index for full-text search
                  storeFields: ['display'], // fields to return with search results
                  idField: 'entry_id',
                  extractField: (document, fieldName) => {
                    if (fieldName === 'id') {
                      return document.id.map(e => e.kind + "-" + e.name).join('.')
                    }
                    return document[fieldName]
                  }
                })


                // Use a unique id since some entries' id are not unique (type extension or
                // standalone doc comments for instance)
                documents.forEach((entry,i) => entry.entry_id = i)
                miniSearch.addAll(documents);

                onmessage = (m) => {
                  let query = m.data;
                  let result = miniSearch.search(query);
                  postMessage(result.slice(0,200).map(a => a.display));
                }
                |}
     index
   |> get_ok;
   Bos.OS.Cmd.run Bos.Cmd.(v "cp" % search_file % "html/") |> get_ok;
   Bos.OS.Cmd.run Bos.Cmd.(v "cp" % "minisearch.js" % "html/") |> get_ok *)

open Cmdliner

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
  let bar message total =
    let open Progress.Line in
    list [ lpad 16 (const message); bar total; count_to total ]
  in
  let procs total =
    let open Progress.Line in
    list [ lpad 16 (const "Processes"); bar total; count_to total ]
  in
  let non_hidden = Atomic.get Stats.stats.non_hidden_units in

  let dline x y = Multi.line (bar x y) in
  with_reporters
    Multi.(
      dline "Compiling" total
      ++ dline "Compiling impls" total_impls
      ++ dline "Compiling pages" total_mlds
      ++ dline "Linking" non_hidden
      ++ dline "Linking impls" total_impls
      ++ dline "Linking mlds" total_mlds
      ++ dline "HTML" (total_impls + non_hidden + total_mlds)
      ++ line (procs nprocs))
    (fun comp compimpl compmld link linkimpl linkmld html procs ->
      let rec inner (a, b, c, d, e, f, g, h) =
        Eio.Time.sleep clock 0.1;
        let a' = Atomic.get Stats.stats.compiled_units in
        let b' = Atomic.get Stats.stats.compiled_impls in
        let c' = Atomic.get Stats.stats.compiled_mlds in
        let d' = Atomic.get Stats.stats.linked_units in
        let e' = Atomic.get Stats.stats.linked_impls in
        let f' = Atomic.get Stats.stats.linked_mlds in
        let g' = Atomic.get Stats.stats.generated_units in
        let h' = Atomic.get Stats.stats.processes in

        comp (a' - a);
        compimpl (b' - b);
        compmld (c' - c);
        link (d' - d);
        linkimpl (e' - e);
        linkmld (f' - f);
        html (g' - g);
        procs (h' - h);
        if g' < non_hidden + total_impls + total_mlds then
          inner (a', b', c', d', e', f', g', h')
      in
      inner (0, 0, 0, 0, 0, 0, 0, 0))

let run libs verbose odoc_dir html_dir stats nb_workers =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let () = Worker_pool.start_workers env sw nb_workers in
  let libs =
    List.map Ocamlfind.sub_libraries libs
    |> List.fold_left Util.StringSet.union Util.StringSet.empty
  in
  let all = Packages.of_libs libs in
  Compile.init_stats all;
  let () =
    Eio.Fiber.both
      (fun () ->
        let compiled = Compile.compile odoc_dir all in
        let linked = Compile.link compiled in
        let sidebars =
          Compile.compile_sidebars odoc_dir
            (Fpath.( / ) odoc_dir "sidebars")
            all
        in
        let () = Compile.html_generate html_dir sidebars linked in
        let _ = Odoc.support_files html_dir in
        ())
      (fun () -> render_stats env nb_workers)
  in

  Format.eprintf "Final stats: %a@.%!" Stats.pp_stats Stats.stats;
  Format.eprintf "Total time: %f@.%!" (Stats.total_time ());
  if stats then Stats.bench_results html_dir;
  let indexes = Util.StringMap.map (fun _i pkg -> Indexes.package pkg) all in

  ignore indexes

let fpath_arg =
  let parse s =
    match Fpath.of_string s with
    | Ok v -> Ok v
    | Error (`Msg m) -> Error (`Msg m)
  in
  let print ppf v = Fpath.pp ppf v in
  Arg.conv (parse, print)

let odoc_dir =
  let doc = "Directory in which the intermediate odoc files go" in
  Arg.(value & opt fpath_arg (Fpath.v "_odoc/") & info [ "odoc-dir" ] ~doc)

let html_dir =
  let doc = "Directory in which the generated HTML files go" in
  Arg.(value & opt fpath_arg (Fpath.v "_html/") & info [ "html-dir" ] ~doc)

let packages =
  (* TODO: Is it package or library? *)
  let doc = "The packages to document" in
  Arg.(value & opt_all string [] & info [ "p" ] ~doc)

let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let stats =
  let doc = "Produce 'driver-benchmarks.json' with run stats" in
  Arg.(value & flag & info [ "stats" ] ~doc)

let nb_workers =
  let doc = "Number of workers." in
  Arg.(value & opt int 15 & info [ "j" ] ~doc)

let cmd =
  let doc = "Generate odoc documentation" in
  let info = Cmd.info "odoc_driver" ~doc in
  Cmd.v info
    Term.(
      const run $ packages $ verbose $ odoc_dir $ html_dir $ stats $ nb_workers)

(* let map = Ocamlfind.package_to_dir_map () in
   let _dirs = List.map (fun lib -> List.assoc lib map) deps in


   let (_, lib_to_pkg_map) = Opam.pkg_to_dir_map () in
   Opam.StringMap.iter (fun k v ->
     if k <> v.Opam.name then
       Format.printf "%s -> %a\n" k Opam.pp v) lib_to_pkg_map;
   List.iter (fun dep -> Format.printf "%s\n%!" dep) deps;
   ignore (exit 0); *)
(* let all_units = all_units () in
   let compiled = compile_all all_units in
   let linked = link_all compiled in
   let () = index_generate () in
   (* let _ = js_index () in *)
   ignore js_index;
   let _ = Odoc.count_occurrences (Fpath.v "occurrences-from-odoc.odoc") in
   ignore (generate_all linked);
   let _ = Stats.bench_results () in *)

let _ = exit (Cmd.eval cmd)
