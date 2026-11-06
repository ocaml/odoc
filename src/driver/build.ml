(* Build the packages of a run, each as a whole.

   A package is compiled, then linked and rendered. Its compile starts only
   when every package it depends on is ready, and packages that do not depend
   on one another are built at the same time. A package depends on another
   when one of its libraries has one of the other's among its transitive
   dependencies ({!Odoc_unit.pkg.deps}).

   A package whose documentation needs packages it does not depend on, its
   doc deps ({!Opam.doc_deps}), is split as ocaml-docs-ci splits it: it is
   compiled, its doc deps are compiled against it, and only then is it
   linked. So eio is compiled, then eio_main, which depends on eio, and then
   eio is linked, with eio_main in its scope. For the packages that depend on
   it, a split package is ready once it is compiled, since nothing else could
   start before its link; any other package is ready once it is done.

   Within a package, a library is compiled after the libraries of the package
   among its transitive dependencies, and the modules of a library as [Compile.compile_lib] orders
   them.

   The top-level index, whose scope is every package, is built last. *)

open Eio.Std

(* Compiling a package's libraries, each after the package's libraries among
   its transitive dependencies. Those of other packages have been compiled
   before this package starts. *)
let compile_libs (pkg : Odoc_unit.pkg) =
  let own (name, _) =
    List.find_opt (fun (l : Odoc_unit.lib) -> l.lib_name = name) pkg.libs
  in
  let compile_lib =
    Util.memo ~key:(fun (lib : Odoc_unit.lib) -> lib.lib_name)
    @@ fun compile_lib (lib : Odoc_unit.lib) ->
    Fiber.List.iter
      (fun (l : Odoc_unit.lib) ->
        if l.lib_name <> lib.lib_name then compile_lib l)
      (List.filter_map own lib.lib_includes);
    Logs.debug (fun m -> m "Compiling library %s" lib.lib_name);
    Compile.compile_lib lib
  in
  Fiber.List.iter compile_lib pkg.libs

let compile_package (pkg : Odoc_unit.pkg) =
  compile_libs pkg;
  Compile.compile_pages pkg

let link_and_generate ?remap_file ~html_dir ~generate_json ~warnings_tags
    (pkg : Odoc_unit.pkg) =
  Logs.debug (fun m -> m "Linking %a" (Fmt.option Fmt.string) pkg.pkgname);
  Compile.link ~warnings_tags pkg;
  Compile.generate ?remap_file ~generate_json html_dir pkg

type state = {
  pkg : Odoc_unit.pkg;
  compiled : unit Promise.t * unit Promise.u;
  done_ : unit Promise.t * unit Promise.u;
}

let all ~html_dir ~remaps ~generate_json ~warnings_tags
    (pkgs : Odoc_unit.pkg list) =
  let named, toplevel =
    List.partition (fun p -> p.Odoc_unit.pkgname <> None) pkgs
  in
  let states =
    List.fold_left
      (fun acc (pkg : Odoc_unit.pkg) ->
        Util.StringMap.add (Option.get pkg.pkgname)
          { pkg; compiled = Promise.create (); done_ = Promise.create () }
          acc)
      Util.StringMap.empty named
  in
  let in_run names =
    List.filter_map (fun n -> Util.StringMap.find_opt n states) names
  in
  let doc_deps (pkg : Odoc_unit.pkg) = in_run pkg.doc_deps in
  let ready s = if doc_deps s.pkg <> [] then fst s.compiled else fst s.done_ in
  (* The steps wait for one another as a graph: compiling a package waits for
     the packages it depends on to be ready, and linking it for its doc deps to
     be compiled. A cycle would leave every step in it waiting, so say which
     packages it goes through instead. *)
  let finished = Hashtbl.create 16 in
  let pp_step ppf (step, name) =
    Fmt.pf ppf "%s %s"
      (match step with `Compile -> "compile" | `Link -> "link")
      name
  in
  let rec visit path (step, s) =
    let node = (step, Option.get s.pkg.pkgname) in
    if Hashtbl.mem finished node then ()
    else if List.mem node path then
      failwith
        (Fmt.str "The packages wait for one another: %a"
           Fmt.(list ~sep:(any " -> ") pp_step)
           (List.rev (node :: path)))
    else
      let path = node :: path in
      (match step with
      | `Compile ->
          List.iter
            (fun d ->
              visit path ((if doc_deps d.pkg <> [] then `Compile else `Link), d))
            (in_run s.pkg.deps)
      | `Link ->
          visit path (`Compile, s);
          List.iter (fun d -> visit path (`Compile, d)) (doc_deps s.pkg));
      Hashtbl.replace finished node ()
  in
  Util.StringMap.iter (fun _ s -> visit [] (`Link, s)) states;
  Compile.html_support html_dir;
  Compile.with_remaps remaps @@ fun remap_file ->
  let build s =
    List.iter (fun d -> Promise.await (ready d)) (in_run s.pkg.deps);
    compile_package s.pkg;
    Promise.resolve (snd s.compiled) ();
    List.iter (fun d -> Promise.await (fst d.compiled)) (doc_deps s.pkg);
    link_and_generate ?remap_file ~html_dir ~generate_json ~warnings_tags s.pkg;
    Promise.resolve (snd s.done_) ()
  in
  Fiber.List.iter build (List.map snd (Util.StringMap.bindings states));
  List.iter
    (fun pkg ->
      compile_package pkg;
      link_and_generate ?remap_file ~html_dir ~generate_json ~warnings_tags pkg)
    toplevel
