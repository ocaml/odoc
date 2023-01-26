# How to Drive `odoc`

This 'live' document describes how to use `odoc` to produce the documentation of `odoc` itself. The aim is
to show a short, simple example of how `odoc` can be used, covering most of the important features.
The document built here includes not only the documentation of `odoc` itself, but it also builds the
docs for a subset of `odoc`'s dependent libraries to show how this may be done. For a much more
complete and comprehensive use of `odoc`, see the [Voodoo project](https://github.com/ocaml-doc/voodoo), the tool that is being used to build
the package docs for
[ocaml.org/packages](https://ocaml.org/packages).

First we need to initialise MDX with some libraries and helpful values.

```ocaml env=e1
(* Prelude *)
#require "bos";;
#install_printer Fpath.pp;;
#print_length 100;;
#print_depth 10;;
open Bos;;
let (>>=) = Result.bind;;
let (>>|=) m f = m >>= fun x -> Ok (f x);;
let get_ok = function | Ok x -> x | Error (`Msg m) -> failwith m
```

Since the mdx file is not at the root of the project, we need a function to turn
a relative path to one that starts at the beginning of the project:

```ocaml env=e1
let relativize p = Fpath.(v ".." // p)
```


## Desired Output

`odoc` produces output files (html or others) in a structured directory tree, so before running `odoc`, the structure of the output must be decided. For these docs, we want the following structure:

- `odoc/index.html` : main page
- `odoc/{odoc_for_authors.html,...}` : other documentation pages
- `odoc/odoc_model/index.html` : `odoc` model library subpage
- `odoc/odoc_model/Odoc_model/index.html` : Module page for the module `Odoc_model`
- `odoc/odoc_model/Odoc_model/...` : Further pages for the submodules of `Odoc_model`
- `odoc/odoc_.../index.html` : other `odoc` library pages
- `odoc/deps/stdlib/index.html` : stdlib main page
- `odoc/deps/stdlib/Stdlib/index.html` : Module page for the module `Stdlib`
- `odoc/deps/astring/index.html` : astring main page
- `odoc/deps/...` : other dependencies

The `odoc` model for achieving this is that we have *pages* (`.mld` files) that have *children* which are either *further pages* (`.mld` files) or *modules* (from `.cmti` files). This {{!page-parent_child_spec} parent/child relationship} is specified on the command line. Parent pages must be *compiled* by `odoc` before their children. Then compiling a page `mypage.mld` will produce the file `page-mypage.odoc`.

In the example below, there will be a file `odoc.mld` that corresponds with the top-level directory `odoc/`. It will be compiled as follows:

<!-- $MDX skip -->
```sh
odoc compile odoc.mld --child page-odoc_model --child deps ...
```

The file `deps.mld` which corresponds with the sub-directory `odoc/deps/`, will be compiled as follows:

<!-- $MDX skip -->
```sh
odoc compile deps.mld -I . --parent `odoc` --child page-stdlib --child page-astring ...
```

The file `odoc_model.mld` will have a child module `Odoc_model`. It will be compiled as follows:

<!-- $MDX skip -->
```sh
odoc compile odoc_model.mld -I . --parent `odoc` --child module-Odoc_model
```

When compiling any `.mld` file, the parent and all children must be specified. Parents can only be pages from other `.mld` files, and children may be pages (from `.mld` files) or modules (from `.cmti`/`.cmt` or `.cmi` files).

The parent page must exist before the child page is created, and it must have had the child specified when it was initially compiled.

## Document Generation Phases

Using `odoc` is a three-phase process:

1. Compilation: `odoc compile`
   
This takes the output from the compiler in the form of `.cmti`, `.cmt`, or `.cmi` files (in order of preference), translates it into `odoc`'s internal format, and performs some initial expansion and resolution operations. For a given input `/path/to/file.cmti` it will output the file `/path/to/file.odoc` unless the `-o` option is used to override the output file. If there were `.cmi` dependencies required for OCaml to compile these files, then there will be equivalent `.odoc` dependencies needed for the `odoc compile` step. `odoc` will search for these dependencies in the paths specified with the `-I` directive on compilation. `odoc` provides a command to help with this: `odoc compile-deps`:

As an example we can run `odoc compile-deps` on the file `../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Compile.cmti`:

<!-- $MDX non-deterministic=output -->
```sh
$ `odoc` compile-deps ../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Compile.cmti | tail -n 5
Stdlib__result 2ba42445465981713146b97d5e185dd5
Stdlib__seq d6a8de25c9eecf5ae9420a9f3f8b2e88
Stdlib__set 5d365647a10f75c22f2b045a867b4d3e
Stdlib__uchar ab6f1df93abf9e800a3e0d1543523c96
Odoc_xref2__Compile e0d620d652a724705f7ed620dfe07be0
```

so we can see we will need to run `odoc compile` against several `Stdlib` modules before we can compile `odoc_xref2__Compile.cmti`

1. Linking: `odoc link`

This takes the `odoc` files produced during the compilation step and performs the final steps of expansion and resolution. It is during this phase that all the references in the documentation comments are resolved. In order for these to be resolved, everything that is referenced must have been compiled already, and their `odoc` files must be on the
include path as specified by the `-I` arguments to `odoc link`. In this example, we achieve that by compiling all modules and `.mld` files before linking anything. The output of the
link step is an `odocl` file, which is in the same path as the original `odoc` file by default.

Please note: it's only necessary to link the non-hidden modules (i.e., without a double underscore).

3. Generation: `odoc html-generate`

Once the compile and link phases are complete, the resulting `odocl` files may be rendered in a variety of formats. In this example we output HTML.


## `odoc` Documentation

In this section `odoc` is used to generate the documentation of `odoc` and some of its dependent packages. We can make a few simplifying assumptions here:

1. Since we're working with one leaf package, we can assume that there can be no module name clashes in the dependencies. As such, we can afford to put all of our `.odoc` files into one directory and then hard-code the include path to be this directory. When using `odoc` in a context where there may be module name clashes, it requires more careful partitioning of output directories.
2. We'll do all of the compiling before any linking.

Let's start with some functions to execute the three phases of `odoc`.

Compiling a file with `odoc` requires a few arguments: the file to compile, an
optional parent, a list of include paths, a list of children for `.mld` files,
optional source implementation and interface file, and an output path. Include
paths can be just `'.'`, and we can calculate the output file from the input
because all of the files are going into the same directory.

Linking a file with `odoc` requires the input file and a list of include paths. As
for compile, we will hard-code the include path.

Generating the HTML requires the input `odocl` file and an output path. We will hard-code the output path to be `html`.

In all of these, we'll capture `stdout` and `stderr` so we can check it later.

```ocaml env=e1
let odoc = Cmd.v "../src/odoc/bin/main.exe"

let compile_output = ref [ "" ]

let link_output = ref [ "" ]

let generate_output = ref [ "" ]

let add_prefixed_output cmd list prefix lines =
  if List.length lines > 0 then
    list :=
      !list
      @ Bos.Cmd.to_string cmd :: List.map (fun l -> prefix ^ ": " ^ l) lines

let compile file ?parent ?(ignore_output = false) ?impl children =
  let output_file =
    let ext = Fpath.get_ext file in
    let basename = Fpath.basename (Fpath.rem_ext file) in
    match ext with
    | ".mld" -> "page-" ^ basename ^ ".odoc"
    | ".cmt" | ".cmti" | ".cmi" -> basename ^ ".odoc"
    | _ -> failwith ("bad extension: " ^ ext)
  in
  let open Cmd in
  let impl =
    match impl with
    | None -> Cmd.empty
    | Some source_relpath ->
        let source_parent =
          "page-" ^ (source_relpath |> Fpath.parent |> Fpath.basename)
        in
        Cmd.(
          v "--impl"
          % p (relativize source_relpath)
          % "--source-parent" % source_parent)
  in
  let cmd =
    odoc % "compile" % Fpath.to_string file %% impl % "-I" % "." % "-o"
    % output_file
    |> List.fold_right (fun child cmd -> cmd % "--child" % child) children
  in
  let cmd =
    match parent with
    | Some p -> cmd % "--parent" % ("page-\"" ^ p ^ "\"")
    | None -> cmd
  in
  let lines = OS.Cmd.(run_out ~err:err_run_out cmd |> to_lines) |> get_ok in
  if not ignore_output then
    add_prefixed_output cmd compile_output (Fpath.to_string file) lines

let link ?(ignore_output = false) file =
  let open Cmd in
  let cmd = odoc % "link" % p file % "-I" % "." in
  let cmd = if Fpath.to_string file = "stdlib.odoc" then cmd % "--open=\"\"" else cmd in
  Format.printf "%a" pp cmd;
  let lines = OS.Cmd.(run_out ~err:err_run_out cmd |> to_lines) |> get_ok in
  if not ignore_output then
    add_prefixed_output cmd link_output (Fpath.to_string file) lines

let html_generate ?(ignore_output = false) file =
  let open Cmd in
  let cmd =
    odoc % "html-generate" % p file % "-o" % "html" % "--theme-uri" % "odoc"
    % "--support-uri" % "odoc"
  in
  let lines = OS.Cmd.(run_out cmd ~err:err_run_out |> to_lines) |> get_ok in
  if not ignore_output then
    add_prefixed_output cmd generate_output (Fpath.to_string file) lines

let support_files () =
  let open Cmd in
  let cmd = odoc % "support-files" % "-o" % "html/odoc" in
  OS.Cmd.(run_out cmd |> to_lines) |> get_ok
```

We'll now make some library lists. We have not only external dependency
libraries, but `odoc` itself is also separated into libraries too. These two
sets of libraries will be documented in different sections, so we'll keep them
in separate lists. Moreover, `odoc` libraries will include the source code, via
a hardcoded path.

Additionally we'll also construct a list containing the extra documentation pages. Finally let's create a list mapping the section to its parent, which matches
the hierarchy declared above.

```ocaml env=e1
let dep_libraries_core = [
    "odoc-parser";
    "astring";
    "cmdliner";
    "fpath";
    "result";
    "tyxml";
    "fmt";
    "stdlib";
    "yojson";
    "biniou";
];;

let extra_deps = [
    "base";
    "core_kernel";
    "bin_prot";
    "sexplib";
    "sexplib0";
    "base_quickcheck";
    "ppx_sexp_conv";
    "ppx_hash";
]

let dep_libraries =
    match Sys.getenv_opt "ODOC_BENCHMARK" with
    | Some "true" -> dep_libraries_core @ extra_deps
    | _ -> dep_libraries_core

let odoc_libraries = [
    "odoc_xref_test"; "odoc_xref2"; "odoc_odoc";
    "odoc_model_desc"; "odoc_model"; "odoc_manpage"; "odoc_loader";
    "odoc_latex"; "odoc_html"; "odoc_document"; "odoc_examples" ];;

let all_libraries = dep_libraries @ odoc_libraries;;

let extra_docs = [
    "interface";
    "contributing";
    "driver";
    "parent_child_spec";
    "features";
    "interface";
    "odoc_for_authors";
    "dune";
    "ocamldoc_differences";
]

let parents =
    let add_parent p l = List.map (fun lib -> (lib, p)) l in
    (add_parent "deps" dep_libraries) @ (add_parent "odoc" odoc_libraries);;

```

`odoc` operates on the compiler outputs. We need to find them for both the files compiled by Dune within this project and those in libraries we compile against.
The following uses `ocamlfind` to locate the library paths for our dependencies:

```ocaml env=e1
let ocamlfind = Cmd.v "ocamlfind"

let lib_path lib =
  let cmd = Cmd.(ocamlfind % "query" % lib) in
  OS.Cmd.(run_out cmd |> to_lines >>|= List.hd)

let lib_paths =
  List.fold_right
    (fun lib acc ->
      acc >>= fun acc ->
      lib_path lib >>|= fun l -> (lib, l) :: acc)
    dep_libraries (Ok [])
  |> get_ok
```

We need a function to find `odoc` inputs given a search path. `odoc`
operates on [.cmti], [.cmt] or [.cmi] files, in order of preference, and the following
function finds all matching files given a search path. Then it returns an `Fpath.Set.t`
that contains the `Fpath.t` values representing the absolute file path, without its extension.

```ocaml env=e1
let find_units p =
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
  List.fold_right Fpath.Set.add l Fpath.Set.empty;;
```

Since the units returned by this function have their extension stripped, we need
function to find the best file to use with this basename.

```ocaml env=e1
let best_file base =
  List.map (fun ext -> Fpath.add_ext ext base) [ "cmti"; "cmt"; "cmi" ]
  |> List.find (fun f -> Bos.OS.File.exists f |> get_ok)
```

Many of the units will be 'hidden' -- that is, their name will be mangled by Dune
in order to namespace them. This is achieved by prefixing the namespace module and
a double underscore, so we can tell by the existence of a double underscore that
a module is intended to be hidden. The following predicate tests for that condition:

```ocaml env=e1
let is_hidden path = Astring.String.is_infix ~affix:"__" (Fpath.to_string path)
```


To build the documentation, we start with these files. With the following function, we'll call `odoc compile-deps` on the file to
find all other compilation units upon which it depends:

```ocaml env=e1
type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

let compile_deps f =
  let cmd = Cmd.(odoc % "compile-deps" % Fpath.to_string f) in
  OS.Cmd.(run_out cmd |> to_lines)
  >>|= List.filter_map (Astring.String.cut ~sep:" ")
  >>= fun l ->
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")
```

For `odoc` libraries, we infer the implementation and interface source file path
from the library name. We generate a hierarchy of mld file, for each directory
in the hierarchy. The mld contains links, using html specific backend as there
are no references to source.

Mld files are compiled, with parents and children that correspond to the
hierarchy. If a page has no children, we artificially add one to make it render
as `name/index.html`.

```ocaml env=e1
let source_dir_of_odoc_lib lib =
  match String.split_on_char '_' lib with
  | "odoc" :: s ->
      let libname = Fpath.(v (String.concat "_" s)) in
      Some Fpath.(v "src" // libname)
  | _ -> None

let source_files_of_odoc_module lib module_ =
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
      find_by_extension relpath [ "pp.ml"; "ml" ]

let source_mld title filenames =
  let title = Format.asprintf "{0 %s}" title in
  let filenames =
    Fpath.Set.fold
      (fun filename acc ->
        if Fpath.is_dir_path filename then
          let name = Fpath.basename filename ^ "/index.html" in
          Format.asprintf
            {html|- {%%html: <svg aria-label="Directory" height="16" viewBox="0 0 16 16" version="1.1" width="16" data-view-component="true"><path d="M1.75 1A1.75 1.75 0 000 2.75v10.5C0 14.216.784 15 1.75 15h12.5A1.75 1.75 0 0016 13.25v-8.5A1.75 1.75 0 0014.25 3H7.5a.25.25 0 01-.2-.1l-.9-1.2C6.07 1.26 5.55 1 5 1H1.75z"></path></svg> <a href="%s">%s</a>%%}|html}
            name (Fpath.basename filename)
          :: acc
        else
          let name = Fpath.basename filename ^ ".html" in
          Format.asprintf
            {html|- {%%html: <svg height="16" viewBox="0 0 16 16" version="1.1" width="16" data-view-component="true"><path fill-rule="evenodd" d="M3.75 1.5a.25.25 0 00-.25.25v12.5c0 .138.112.25.25.25h9.5a.25.25 0 00.25-.25V6h-2.75A1.75 1.75 0 019 4.25V1.5H3.75zm6.75.062V4.25c0 .138.112.25.25.25h2.688a.252.252 0 00-.011-.013l-2.914-2.914a.272.272 0 00-.013-.011zM2 1.75C2 .784 2.784 0 3.75 0h6.586c.464 0 .909.184 1.237.513l2.914 2.914c.329.328.513.773.513 1.237v9.586A1.75 1.75 0 0113.25 16h-9.5A1.75 1.75 0 012 14.25V1.75z"></path></svg> <a href="%s">%s</a>%%}|html}
            name (Fpath.basename filename)
          :: acc)
      filenames []
  in
  Format.sprintf "%s\n\n%s\n" title (String.concat "\n" filenames)

let source_mlds_of_units units =
  let module M = Map.Make (Fpath) in
  let multi_map_add key v map =
    M.update key
      (fun set ->
        let set = match set with None -> Fpath.Set.empty | Some set -> set in
        Some (Fpath.Set.add v set))
      map
  in
  let rec add_path map path =
    if Fpath.is_current_dir path then map
    else
      let parent, _ = Fpath.split_base path in
      let map = multi_map_add parent path map in
      add_path map parent
  in
  List.fold_left
    (fun map (_, _, _, file) ->
      match file with None -> map | Some file -> add_path map file)
    M.empty units

let compile_src_mlds units =
  let mlds = source_mlds_of_units units in
  let module M = Map.Make (Fpath) in
  let mld_name lib =
    if Fpath.is_current_dir lib then "source" else Fpath.basename lib
  in
  let title lib =
    if Fpath.is_current_dir lib then "source" else Fpath.to_string lib
  in
  let rec traverse_parent parent lib acc =
    if Fpath.is_dir_path lib then
      match M.find_opt lib mlds with
      | None -> []
      | Some set ->
          let path = Fpath.v @@ mld_name lib ^ ".mld" in
          let mld_content = source_mld (title lib) set in
          let () = Bos.OS.File.write path mld_content |> get_ok in
          let () =
            let children =
              match
                set |> Fpath.Set.to_seq |> List.of_seq
                |> List.filter_map (fun p ->
                       if Fpath.is_dir_path p then Some (mld_name p) else None)
              with
              | [] ->
                  [ "dummy" ]
                  (* Needed to have the mld rendered as [name/index.html] *)
              | a -> a
            in
            compile ?parent path children
          in
          let acc =
            [ (Fpath.v ("page-" ^ mld_name lib ^ ".odoc"), false) ] :: acc
          in
          Fpath.Set.fold
            (fun path acc ->
              traverse_parent (Some (mld_name lib)) path [] :: acc)
            set acc
          |> List.flatten
    else []
  in
  traverse_parent (Some "odoc") (Fpath.v "./") []
```

Let's now put together a list of all possible modules. We'll keep track of
which library they're in, and whether that library is a part of `odoc` or a dependency
library.

```ocaml env=e1
let odoc_all_unit_paths = find_units ".." |> get_ok

let odoc_units =
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
    odoc_libraries
```

```ocaml env=e1
let all_units =
  let lib_units =
    List.map
      (fun (lib, p) ->
        Fpath.Set.fold
          (fun p acc -> ("deps", lib, p, None) :: acc)
          (find_units p |> get_ok)
          [])
      lib_paths in
  odoc_units @ lib_units |> List.flatten
```

Now we'll compile all of the parent `.mld` files. To ensure that the parents are compiled before the children, we start with `odoc.mld`, then `deps.mld`, and so on. The result of this file is a list of the resulting `odoc` files.

```ocaml env=e1
let compile_mlds () =
  let mkpage x = "page-\"" ^ x ^ "\"" in
  let mkmod x = "module-" ^ String.capitalize_ascii x in
  let mkmld x = Fpath.(add_ext "mld" (v x)) in
  ignore
    (compile (mkmld "odoc")
       ("page-source" :: "page-deps" :: List.map mkpage (odoc_libraries @ extra_docs)));
  ignore (compile (mkmld "deps") ~parent:"odoc" (List.map mkpage dep_libraries));
  let extra_odocs =
    List.map
      (fun p ->
        ignore (compile (mkmld p) ~parent:"odoc" []);
        "page-" ^ p ^ ".odoc")
      extra_docs
  in
  let odocs =
    List.map
      (fun library ->
        let parent = List.assoc library parents in
        let children =
          List.filter_map
            (fun (parent, lib, child, _) ->
              if lib = library then Some (Fpath.basename child |> mkmod)
              else None)
            all_units
        in
        ignore (compile (mkmld ("library_mlds/"^library)) ~parent children);
        "page-" ^ library ^ ".odoc")
      all_libraries
  in
  List.map
    (fun f -> (Fpath.v f, false))
    ("page-odoc.odoc" :: "page-deps.odoc" :: odocs @ extra_odocs)
```

Now we get to the compilation phase. For each unit, we query its dependencies, then recursively call to compile these dependencies. Once this is done we compile the unit itself. If the unit has already been compiled we don't do anything. Note that we aren't checking the hashes of the dependencies which a build system should do to ensure that the module being compiled is the correct one. Again we benefit from the fact that we're creating the docs for one leaf package and that there must be no module name clashes in its dependencies. The result of this function is a list of the resulting `odoc` files.

```ocaml env=e1
let compile_all () =
  let mld_odocs = compile_mlds () in
  let src_mlds = compile_src_mlds all_units in
  let rec rec_compile ?impl parent lib file =
    let output = Fpath.(base (set_ext "odoc" file)) in
    if OS.File.exists output |> get_ok then []
    else
      let deps = compile_deps file |> get_ok in
      let files =
        List.fold_left
          (fun acc (dep_name, digest) ->
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
      ignore (compile file ~parent:lib ?impl ~ignore_output []);
      (output, ignore_output) :: files
  in
  List.fold_left
    (fun acc (parent, lib, dep, impl) ->
      acc @ rec_compile ?impl parent lib (best_file dep))
    [] all_units
  @ mld_odocs @ src_mlds
```

Linking is now straightforward. We link all `odoc` files.

```ocaml env=e1
let link_all odoc_files =
  List.map
    (fun (odoc_file, ignore_output) ->
      ignore (link ~ignore_output odoc_file);
      Fpath.set_ext "odocl" odoc_file)
    odoc_files
```

Now we simply run `odoc html-generate` over all of the resulting `odocl` files.
This will generate sources, as well as documentation for non-hidden units.

```ocaml env=e1
let generate_all odocl_files =
  List.iter (fun f -> ignore(html_generate f)) odocl_files;
  support_files ()
```

The following code actually executes all of the above, and we're done!

```ocaml env=e1
let compiled = compile_all () in
let linked = link_all compiled in
generate_all linked
```

Let's see if there was any output from the `odoc` invocations:
```ocaml env=e1
# #print_length 655360;;
# !compile_output;;
- : string list = [""]
# !link_output;;
- : string list =
[""; "'../src/odoc/bin/main.exe' 'link' 'page-deps.odoc' '-I' '.'";
 "page-deps.odoc: File \"src/fmt.mli\", line 6, characters 4-13:";
 "page-deps.odoc: Warning: Failed to resolve reference unresolvedroot(Format) Couldn't find \"Format\"";
 "page-deps.odoc: File \"src/fpath.mli\", line 8, characters 8-20:";
 "page-deps.odoc: Warning: Failed to resolve reference unresolvedroot(Map) Couldn't find \"Map\"";
 "page-deps.odoc: File \"src/fpath.mli\", line 7, characters 59-71:";
 "page-deps.odoc: Warning: Failed to resolve reference unresolvedroot(Set) Couldn't find \"Set\"";
 "page-deps.odoc: File \"src/fpath.mli\", line 7, characters 28-52:";
 "page-deps.odoc: Warning: Failed to resolve reference unresolvedroot(file_exts) Couldn't find \"file_exts\"";
 "'../src/odoc/bin/main.exe' 'link' 'page-stdlib.odoc' '-I' '.'";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 199, characters 0-29:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Reg_with_debug_info) Parent_module: Lookup failure (root module): Reg_with_debug_info";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 198, characters 0-30:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Reg_availability_set) Parent_module: Lookup failure (root module): Reg_availability_set";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 194, characters 0-15:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Ratio) Parent_module: Lookup failure (root module): Ratio";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 159, characters 0-13:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Num) Parent_module: Lookup failure (root module): Num";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 158, characters 0-13:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Nat) Parent_module: Lookup failure (root module): Nat";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 76, characters 0-29:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Compute_ranges_intf) Parent_module: Lookup failure (root module): Compute_ranges_intf";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 75, characters 0-24:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Compute_ranges) Parent_module: Lookup failure (root module): Compute_ranges";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 24, characters 0-17:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Big_int) Parent_module: Lookup failure (root module): Big_int";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 21, characters 0-24:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Available_regs) Parent_module: Lookup failure (root module): Available_regs";
 "page-stdlib.odoc: File \"library_mlds/stdlib.mld\", line 9, characters 0-22:";
 "page-stdlib.odoc: Warning: Failed to resolve reference unresolvedroot(Arith_status) Parent_module: Lookup failure (root module): Arith_status"]
# !generate_output;;
- : string list =
["";
 "'../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'";
 "odoc_examples.odocl: Warning, resolved hidden path: Odoc_examples__.Unexposed.t"]
```

We can have a look at the produced hierarchy of files, which matches the desired output. Note that source files with a `.ml.html` extension are generated for modules compiled with the `--impl` option.
```sh
$ ls html/odoc
contributing.html
deps
driver.html
dune.html
features.html
highlight.pack.js
index.html
interface.html
ocamldoc_differences.html
odoc.css
odoc_document
odoc_examples
odoc_for_authors.html
odoc_html
odoc_latex
odoc_loader
odoc_manpage
odoc_model
odoc_model_desc
odoc_odoc
odoc_xref2
odoc_xref_test
parent_child_spec.html
$ ls html/odoc/deps
astring
biniou
cmdliner
fmt
fpath
index.html
odoc-parser
result
stdlib
tyxml
yojson
$ find html/odoc/deps | tail -n 20
html/odoc/deps/stdlib/Pparse
html/odoc/deps/stdlib/Pparse/index.html
html/odoc/deps/stdlib/Typetexp
html/odoc/deps/stdlib/Typetexp/index.html
html/odoc/deps/stdlib/Build_export_info
html/odoc/deps/stdlib/Build_export_info/index.html
html/odoc/deps/stdlib/Typedecl_properties
html/odoc/deps/stdlib/Typedecl_properties/index.html
html/odoc/deps/stdlib/Comballoc
html/odoc/deps/stdlib/Comballoc/index.html
html/odoc/deps/stdlib/Printclambda_primitives
html/odoc/deps/stdlib/Printclambda_primitives/index.html
html/odoc/deps/stdlib/Pass_wrapper
html/odoc/deps/stdlib/Pass_wrapper/index.html
html/odoc/deps/stdlib/Ctype
html/odoc/deps/stdlib/Ctype/index.html
html/odoc/deps/stdlib/Toploop
html/odoc/deps/stdlib/Toploop/index.html
html/odoc/deps/stdlib/Envaux
html/odoc/deps/stdlib/Envaux/index.html
$ find html/odoc/odoc_html
html/odoc/odoc_html
html/odoc/odoc_html/index.html
html/odoc/odoc_html/Odoc_html
html/odoc/odoc_html/Odoc_html/Generator
html/odoc/odoc_html/Odoc_html/Generator/Generator.ml.html
html/odoc/odoc_html/Odoc_html/Generator/index.html
html/odoc/odoc_html/Odoc_html/Types
html/odoc/odoc_html/Odoc_html/Types/Types.ml.html
html/odoc/odoc_html/Odoc_html/Types/index.html
html/odoc/odoc_html/Odoc_html/Link
html/odoc/odoc_html/Odoc_html/Link/Path
html/odoc/odoc_html/Odoc_html/Link/Path/index.html
html/odoc/odoc_html/Odoc_html/Link/index.html
html/odoc/odoc_html/Odoc_html/Link/Link.ml.html
html/odoc/odoc_html/Odoc_html/Odoc_html.ml.html
html/odoc/odoc_html/Odoc_html/index.html
html/odoc/odoc_html/Odoc_html/Config
html/odoc/odoc_html/Odoc_html/Config/index.html
html/odoc/odoc_html/Odoc_html/Config/Config.ml.html
html/odoc/odoc_html/Odoc_html/Tree
html/odoc/odoc_html/Odoc_html/Tree/Tree.ml.html
html/odoc/odoc_html/Odoc_html/Tree/index.html
```
