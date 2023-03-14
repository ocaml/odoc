# How to Drive `odoc`

This 'live' document describes how to use `odoc` to produce the documentation of `odoc` itself. The aim is
to show a short, simple example of how `odoc` can be used, covering most of the important features.
The document built here includes not only the documentation of `odoc` itself, but it also builds the
docs for a subset of `odoc`'s dependent libraries to show how this may be done. For a much more
complete and comprehensive use of `odoc`, see the [Voodoo project](https://github.com/ocaml-doc/voodoo), the tool that is being used to build
the package docs for
[ocaml.org/packages](https://ocaml.org/packages). The information in this page is specific to
odoc version 2.3 or later.  For earlier
versions see the `driver.md` or `driver.mld` files in the corresponding source distribution.


First, we need to initialise MDX with some libraries and helpful values:

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
let relativize p = Fpath.(v ".." // p) (* this driver is run from the [doc] dir *)
```

## Desired Output

`odoc` produces output files (HTML or others) in a structured directory tree, so before running `odoc`, the structure of the output must be decided. For these docs, we want the following structure:

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
- `odoc/source/...` : rendered source files

The `odoc` model for achieving this is that we have *pages* (`.mld` files) that have *children* which are either *further pages* (`.mld` files), *modules* (from `.cmti` files), or
*source trees*. This {{!page-parent_child_spec} parent/child relationship} is specified on the command line. Parent pages must be *compiled* by `odoc` before their children. Then compiling a page `mypage.mld` will produce the file `page-mypage.odoc`.

In the example below, there will be a file `odoc.mld` that corresponds with the top-level directory `odoc/`. It will be compiled as follows:

<!-- $MDX skip -->
```sh
odoc compile odoc.mld --child page-odoc_model --child deps 
  --child src-source ...
```

The file `deps.mld` which corresponds with the sub-directory `odoc/deps/`, will be compiled as follows:

<!-- $MDX skip -->
```sh
odoc compile deps.mld -I . --parent page-odoc --child page-stdlib --child page-astring ...
```

The file `odoc_model.mld` will have a child module `Odoc_model`. It will be compiled as follows:

<!-- $MDX skip -->
```sh
odoc compile odoc_model.mld -I . --parent page-odoc --child module-Odoc_model
```

The last type of page contains a list of paths to the source files that should be rendered as HTML. The output will be found as a tree underneath this page. This will be compiled in the following way:

<!-- $MDX skip -->
```sh
odoc source-tree source.map -I . --parent page-odoc
```

where the first few lines of `source.map` are:

```
src/xref2/utils.ml
src/xref2/type_of.ml
src/xref2/tools.ml
```

indicating the desire for the rendered source of `utils.ml` to be found as the file `odoc/source/src/xref2/utils.ml.html`.

When compiling any `.mld` file, the parent and all children must be specified. Parents can only be pages from other `.mld` files, and children may be pages (from `.mld` files) or modules (from `.cmti`,`.cmt`, or `.cmi` files).

The parent page must exist before the child page is created, and it must have had the child specified when it was initially compiled.

## Document Generation Phases

Using `odoc` is a three-phase process:

1. Compilation: `odoc compile`
   
This takes as input either `.mld` files containing pure odoc markup, or the output from the compiler in the form of `.cmti`, `.cmt`, or `.cmi` files (in order of preference). For `.mld` files, this step simply translates them into `odoc`'s internal format and writes the corresponding file. For example, given the input `foobar.mld`, `odoc` will output `page-foobar.odoc`. There are no dependencies for compiling `.mld` files beyond the parent as outlined above.

For modules, compilation is the point where `odoc` performs some initial expansion and resolution operations, a process that usually introduces dependencies. For a given input `/path/to/file.cmti` it will output the file `/path/to/file.odoc` unless the `-o` option is used to override the output file. If there were `.cmi` dependencies required for OCaml to compile a particular module, then there will be equivalent `.odoc` dependencies needed for the `odoc compile` step. `odoc` will search for these dependencies in the paths specified with the `-I` directive on compilation. `odoc` provides a command to help with this: `odoc compile-deps`.

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

From this, we see it's necessary to run `odoc compile` against several `Stdlib` modules before we can compile `odoc_xref2__Compile.cmti`

1. Linking: `odoc link`

This takes the `odoc` files produced during the compilation step and performs the final steps of resolution for both pages and modules, and expansion for modules only. It is during this phase that all the references in the documentation comments are resolved. In order for these to be resolved, everything that is referenced must have been compiled already, and their `odoc` files must be on the
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
optional parent and name for source implementation, and an output path. Include
paths can be just `'.'`, and we can calculate the output file from the input
because all of the files are going into the same directory.

Linking a file with `odoc` requires the input file and a list of include paths. As
for compile, we will hard-code the include path.

Generating the HTML requires the input `odocl` file, an optional implementation
source file (passed via the `--source` argument), and an output path. We will
hard-code the output path to be `html/`.

Using the `--source` argument with an `.odocl` file that was not compiled with
`--source-parent-file` and `--source-name` will result in an error, as will omitting `--source` when generating HTML of an `odocl` that was
compiled with `--source-parent-file` and `--source-name`.

In all of these, we'll capture `stdout` and `stderr` so we can check it later.

```ocaml env=e1
let odoc = Cmd.v "../src/odoc/bin/main.exe" (* This is the just-built odoc binary *)

let compile_output = ref [ "" ]

let link_output = ref [ "" ]

let generate_output = ref [ "" ]

let commands = ref [ ]

let run cmd =
  let cmd_str = Cmd.to_string cmd in
  commands := cmd_str :: !commands;
  OS.Cmd.(run_out ~err:OS.Cmd.err_run_out cmd |> to_lines) |> get_ok

let add_prefixed_output cmd list prefix lines =
  if List.length lines > 0 then
    list :=
      !list
      @ Bos.Cmd.to_string cmd :: List.map (fun l -> prefix ^ ": " ^ l) lines

let compile file ?parent ?(output_dir = Fpath.v "./")
    ?(ignore_output = false) ?source_args children =
  let output_file =
    let ext = Fpath.get_ext file in
    let basename = Fpath.basename (Fpath.rem_ext file) in
    match ext with
    | ".mld" -> "page-" ^ basename ^ ".odoc"
    | ".cmt" | ".cmti" | ".cmi" -> basename ^ ".odoc"
    | _ -> failwith ("bad extension: " ^ ext)
  in
  let open Cmd in
  let source_args =
    match source_args with
    | None -> Cmd.empty
    | Some (source_name, source_parent_file) ->
        Cmd.(
          v "--source-name" % p source_name % "--source-parent-file"
          % p source_parent_file)
  in
  let cmd =
    odoc % "compile" % Fpath.to_string file %% source_args % "-I" % "."
    % "-o"
    % p (Fpath.( / ) output_dir output_file)
    |> List.fold_right (fun child cmd -> cmd % "--child" % child) children
  in
  let cmd =
    match parent with
    | Some p -> cmd % "--parent" % ("page-\"" ^ p ^ "\"")
    | None -> cmd
  in
  let lines = run cmd in
  if not ignore_output then
    add_prefixed_output cmd compile_output (Fpath.to_string file) lines

let link ?(ignore_output = false) file =
  let open Cmd in
  let cmd = odoc % "link" % p file % "-I" % "." in
  let cmd = if Fpath.to_string file = "stdlib.odoc" then cmd % "--open=\"\"" else cmd in
  let lines = run cmd in
  if not ignore_output then
    add_prefixed_output cmd link_output (Fpath.to_string file) lines

let html_generate ?(ignore_output = false) file source =
  let open Cmd in
  let source = match source with None -> empty | Some source -> v "--source" % p source in
  let cmd =
    odoc % "html-generate" %% source % p file % "-o" % "html" % "--theme-uri" % "odoc"
    % "--support-uri" % "odoc"
  in
  let lines = run cmd in
  if not ignore_output then
    add_prefixed_output cmd generate_output (Fpath.to_string file) lines

let support_files () =
  let open Cmd in
  let cmd = odoc % "support-files" % "-o" % "html/odoc" in
  run cmd
```

We'll now make some library lists. We have not only external dependency
libraries, but `odoc` itself is also separated into libraries. These two
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
    "odoc_xref_test"; "odoc_xref2"; "odoc_odoc"; "odoc_html_support_files";
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
The following uses `ocamlfind` to locate the library paths for our dependencies. Since `ocamlfind` gives
us the absolute path, we also have a short function here to relativize it based on our current working
directory to ensure the log of commands we collect is as reproducible as possible.

```ocaml env=e1
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

let lib_path lib =
  let cmd = Cmd.(ocamlfind % "query" % lib) in
  run cmd |> List.hd |> relativize_path

let lib_paths =
  List.fold_right
    (fun lib acc ->
      (lib, lib_path lib) :: acc)
    dep_libraries []
```

We need a function to find `odoc` inputs from the given search path. `odoc`
operates on `.cmti`, `.cmt`, or `.cmi` files, in order of preference, and the following
function finds all matching files starting from the given path. Then it returns an `Fpath.Set.t`
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

Many of the units will be 'hidden', meaning that Dune will mangle their name
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
  let deps = run cmd in
  let l = List.filter_map (Astring.String.cut ~sep:" ") deps in
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")
```

For `odoc` libraries, we infer the implementation and interface source file path
from the library name. We list them in a file, passed to `odoc source-tree`, to
generate `src-source.odoc`. This file contains the source hierarchy, and will be
linked and passed to `html-generate` just as other pages and compilation units.

It is used as the `source-parent` for all units for which we could provide
sources.

```ocaml env=e1
let source_tree_output = ref [ "" ]

let source_tree ?(ignore_output = false) ~parent ~output file =
  let open Cmd in
  let parent = v "--parent" % ("page-\"" ^ parent ^ "\"") in
  let cmd = odoc % "source-tree" % "-I" % "." %% parent % "-o" % p output % p file in
  let lines = run cmd in
  if not ignore_output then
    add_prefixed_output cmd source_tree_output (Fpath.to_string file) lines

let odoc_source_tree = Fpath.v "src-source.odoc"

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

let compile_source_tree units =
  let sources =
    List.filter_map (fun (_, _, _, file) -> Option.map Fpath.to_string file) units
  in
  let source_map = Fpath.v "source.map" in
  let () = Bos.OS.File.write_lines source_map sources |> get_ok in
  let () = source_tree ~parent:"odoc" ~output:odoc_source_tree source_map in
  (odoc_source_tree, false, None)

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
       ("src-source" :: "page-deps" :: List.map mkpage (odoc_libraries @ extra_docs)));
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
    (fun f -> (Fpath.v f, false, None))
    ("page-odoc.odoc" :: "page-deps.odoc" :: odocs @ extra_odocs)
```

Now we get to the compilation phase. For each unit, we query its dependencies, then recursively call to compile these dependencies. Once this is done we compile the unit itself. If the unit has already been compiled we don't do anything. Note that we aren't checking the hashes of the dependencies which a build system should do to ensure that the module being compiled is the correct one. Again we benefit from the fact that we're creating the docs for one leaf package and that there must be no module name clashes in its dependencies. The result of this function is a list of the resulting `odoc` files.

```ocaml env=e1
let compile_all () =
  let mld_odocs = compile_mlds () in
  let source_tree = compile_source_tree all_units in
  let source_args =
    Option.map (fun source_relpath -> (source_relpath, odoc_source_tree))
  in
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
      let source_args = source_args impl in
      compile file ~parent:lib ?source_args ~ignore_output [];
      (output, ignore_output, impl) :: files
  in
  source_tree
  :: List.fold_left
    (fun acc (parent, lib, dep, impl) ->
      acc @ rec_compile ?impl parent lib (best_file dep))
    [] all_units
  @ mld_odocs
```

Linking is now straightforward. We link all `odoc` files.

```ocaml env=e1
let link_all odoc_files =
  List.map
    (fun (odoc_file, ignore_output, source) ->
      ignore (link ~ignore_output odoc_file);
      Fpath.set_ext "odocl" odoc_file, source)
    odoc_files
```

Now we simply run `odoc html-generate` over all of the resulting `odocl` files.
This will generate sources, as well as documentation for non-hidden units.

```ocaml env=e1
let generate_all odocl_files =
  let relativize_opt = function None -> None | Some file -> Some (relativize file) in
  List.iter (fun (f, source) -> ignore(html_generate f (relativize_opt source))) odocl_files;
  support_files ()
```

The following code executes all of the above, and we're done!

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
 "page-deps.odoc: Warning: Failed to resolve reference unresolvedroot(file_exts) Couldn't find \"file_exts\""]
# !source_tree_output;;
- : string list = [""]
# !generate_output;;
- : string list =
["";
 "'../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/odoc_model.ml' 'odoc_model.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'";
 "odoc_model.odocl: Warning, resolved hidden path: Odoc_model__.Paths_types.Identifier.source_dir_pv";
 "odoc_model.odocl: Warning, resolved hidden path: Odoc_model__.Paths_types.Identifier.source_dir";
 "odoc_model.odocl: Warning, resolved hidden path: Odoc_model__.Paths_types.Identifier.source_dir_pv";
 "'../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'";
 "odoc_examples.odocl: Warning, resolved hidden path: Odoc_examples__.Unexposed.t"]
```

We can have a look at the produced hierarchy of files, which matches the desired output. Note that source files with a `.ml.html` extension are generated for modules compiled with the `--source` option.
```sh
$ ls html/odoc
contributing.html
deps
driver.html
dune.html
features.html
fonts
highlight.pack.js
index.html
interface.html
katex.min.css
katex.min.js
ocamldoc_differences.html
odoc.css
odoc_document
odoc_examples
odoc_for_authors.html
odoc_html
odoc_html_support_files
odoc_latex
odoc_loader
odoc_manpage
odoc_model
odoc_model_desc
odoc_odoc
odoc_xref2
odoc_xref_test
parent_child_spec.html
source
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
$ find html/odoc/deps | sort | tail -n 20
html/odoc/deps/tyxml/Xml_wrap/module-type-T/index.html
html/odoc/deps/tyxml/index.html
html/odoc/deps/yojson
html/odoc/deps/yojson/Yojson
html/odoc/deps/yojson/Yojson/Basic
html/odoc/deps/yojson/Yojson/Basic/Util
html/odoc/deps/yojson/Yojson/Basic/Util/index.html
html/odoc/deps/yojson/Yojson/Basic/index.html
html/odoc/deps/yojson/Yojson/Lexer_state
html/odoc/deps/yojson/Yojson/Lexer_state/index.html
html/odoc/deps/yojson/Yojson/Raw
html/odoc/deps/yojson/Yojson/Raw/index.html
html/odoc/deps/yojson/Yojson/Safe
html/odoc/deps/yojson/Yojson/Safe/Util
html/odoc/deps/yojson/Yojson/Safe/Util/index.html
html/odoc/deps/yojson/Yojson/Safe/index.html
html/odoc/deps/yojson/Yojson/index.html
html/odoc/deps/yojson/Yojson_biniou
html/odoc/deps/yojson/Yojson_biniou/index.html
html/odoc/deps/yojson/index.html
$ find html/odoc/odoc_html | sort
html/odoc/odoc_html
html/odoc/odoc_html/Odoc_html
html/odoc/odoc_html/Odoc_html/Config
html/odoc/odoc_html/Odoc_html/Config/index.html
html/odoc/odoc_html/Odoc_html/Generator
html/odoc/odoc_html/Odoc_html/Generator/index.html
html/odoc/odoc_html/Odoc_html/Html_fragment_json
html/odoc/odoc_html/Odoc_html/Html_fragment_json/index.html
html/odoc/odoc_html/Odoc_html/Html_page
html/odoc/odoc_html/Odoc_html/Html_page/index.html
html/odoc/odoc_html/Odoc_html/Link
html/odoc/odoc_html/Odoc_html/Link/Path
html/odoc/odoc_html/Odoc_html/Link/Path/index.html
html/odoc/odoc_html/Odoc_html/Link/index.html
html/odoc/odoc_html/Odoc_html/Types
html/odoc/odoc_html/Odoc_html/Types/index.html
html/odoc/odoc_html/Odoc_html/index.html
html/odoc/odoc_html/index.html
```

Finally, let's have a list of all of the commands executed during the execution of this process:

```ocaml env=e1
# List.iter (Printf.printf "$ %s\n") (List.rev !commands);;
$ 'ocamlfind' 'query' 'biniou'
$ 'ocamlfind' 'query' 'yojson'
$ 'ocamlfind' 'query' 'stdlib'
$ 'ocamlfind' 'query' 'fmt'
$ 'ocamlfind' 'query' 'tyxml'
$ 'ocamlfind' 'query' 'result'
$ 'ocamlfind' 'query' 'fpath'
$ 'ocamlfind' 'query' 'cmdliner'
$ 'ocamlfind' 'query' 'astring'
$ 'ocamlfind' 'query' 'odoc-parser'
$ '../src/odoc/bin/main.exe' 'compile' 'odoc.mld' '-I' '.' '-o' './page-odoc.odoc' '--child' 'page-"ocamldoc_differences"' '--child' 'page-"dune"' '--child' 'page-"odoc_for_authors"' '--child' 'page-"interface"' '--child' 'page-"features"' '--child' 'page-"parent_child_spec"' '--child' 'page-"driver"' '--child' 'page-"contributing"' '--child' 'page-"interface"' '--child' 'page-"odoc_examples"' '--child' 'page-"odoc_document"' '--child' 'page-"odoc_html"' '--child' 'page-"odoc_latex"' '--child' 'page-"odoc_loader"' '--child' 'page-"odoc_manpage"' '--child' 'page-"odoc_model"' '--child' 'page-"odoc_model_desc"' '--child' 'page-"odoc_html_support_files"' '--child' 'page-"odoc_odoc"' '--child' 'page-"odoc_xref2"' '--child' 'page-"odoc_xref_test"' '--child' 'page-deps' '--child' 'src-source'
$ '../src/odoc/bin/main.exe' 'compile' 'deps.mld' '-I' '.' '-o' './page-deps.odoc' '--child' 'page-"biniou"' '--child' 'page-"yojson"' '--child' 'page-"stdlib"' '--child' 'page-"fmt"' '--child' 'page-"tyxml"' '--child' 'page-"result"' '--child' 'page-"fpath"' '--child' 'page-"cmdliner"' '--child' 'page-"astring"' '--child' 'page-"odoc-parser"' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'interface.mld' '-I' '.' '-o' './page-interface.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'contributing.mld' '-I' '.' '-o' './page-contributing.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'driver.mld' '-I' '.' '-o' './page-driver.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'parent_child_spec.mld' '-I' '.' '-o' './page-parent_child_spec.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'features.mld' '-I' '.' '-o' './page-features.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'interface.mld' '-I' '.' '-o' './page-interface.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'odoc_for_authors.mld' '-I' '.' '-o' './page-odoc_for_authors.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'dune.mld' '-I' '.' '-o' './page-dune.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'ocamldoc_differences.mld' '-I' '.' '-o' './page-ocamldoc_differences.odoc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc-parser.mld' '-I' '.' '-o' './page-odoc-parser.odoc' '--child' 'module-Odoc_parser' '--child' 'module-Odoc_parser__' '--child' 'module-Odoc_parser__Ast' '--child' 'module-Odoc_parser__Lexer' '--child' 'module-Odoc_parser__Loc' '--child' 'module-Odoc_parser__Parse_error' '--child' 'module-Odoc_parser__Syntax' '--child' 'module-Odoc_parser__Token' '--child' 'module-Odoc_parser__Warning' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/astring.mld' '-I' '.' '-o' './page-astring.odoc' '--child' 'module-Astring' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/cmdliner.mld' '-I' '.' '-o' './page-cmdliner.odoc' '--child' 'module-Cmdliner' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/fpath.mld' '-I' '.' '-o' './page-fpath.odoc' '--child' 'module-Fpath' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/result.mld' '-I' '.' '-o' './page-result.odoc' '--child' 'module-Result' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/tyxml.mld' '-I' '.' '-o' './page-tyxml.odoc' '--child' 'module-Html_f' '--child' 'module-Html_sigs' '--child' 'module-Html_types' '--child' 'module-Svg_f' '--child' 'module-Svg_sigs' '--child' 'module-Svg_types' '--child' 'module-Xml_iter' '--child' 'module-Xml_print' '--child' 'module-Xml_sigs' '--child' 'module-Xml_stream' '--child' 'module-Xml_wrap' '--child' 'module-Tyxml' '--child' 'module-Tyxml_html' '--child' 'module-Tyxml_svg' '--child' 'module-Tyxml_xml' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/fmt.mld' '-I' '.' '-o' './page-fmt.odoc' '--child' 'module-Fmt' '--child' 'module-Fmt_cli' '--child' 'module-Fmt_tty' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/stdlib.mld' '-I' '.' '-o' './page-stdlib.odoc' '--child' 'module-CamlinternalFormat' '--child' 'module-CamlinternalFormatBasics' '--child' 'module-CamlinternalLazy' '--child' 'module-CamlinternalMod' '--child' 'module-CamlinternalOO' '--child' 'module-CSE' '--child' 'module-CSEgen' '--child' 'module-Afl_instrument' '--child' 'module-Alias_analysis' '--child' 'module-Allocated_const' '--child' 'module-Annot' '--child' 'module-Arch' '--child' 'module-Arg_helper' '--child' 'module-Asmgen' '--child' 'module-Asmlibrarian' '--child' 'module-Asmlink' '--child' 'module-Asmpackager' '--child' 'module-Ast_helper' '--child' 'module-Ast_invariants' '--child' 'module-Ast_iterator' '--child' 'module-Ast_mapper' '--child' 'module-Asttypes' '--child' 'module-Attr_helper' '--child' 'module-Augment_specialised_args' '--child' 'module-Backend_intf' '--child' 'module-Backend_var' '--child' 'module-Binutils' '--child' 'module-Branch_relaxation' '--child' 'module-Branch_relaxation_intf' '--child' 'module-Btype' '--child' 'module-Build_export_info' '--child' 'module-Build_path_prefix_map' '--child' 'module-Builtin_attributes' '--child' 'module-Bytegen' '--child' 'module-Bytelibrarian' '--child' 'module-Bytelink' '--child' 'module-Bytepackager' '--child' 'module-Bytesections' '--child' 'module-CamlinternalMenhirLib' '--child' 'module-Ccomp' '--child' 'module-Clambda' '--child' 'module-Clambda_primitives' '--child' 'module-Clflags' '--child' 'module-Closure' '--child' 'module-Closure_conversion' '--child' 'module-Closure_conversion_aux' '--child' 'module-Closure_element' '--child' 'module-Closure_id' '--child' 'module-Closure_middle_end' '--child' 'module-Closure_offsets' '--child' 'module-Closure_origin' '--child' 'module-Cmi_format' '--child' 'module-Cmm' '--child' 'module-Cmm_helpers' '--child' 'module-Cmm_invariants' '--child' 'module-Cmmgen' '--child' 'module-Cmmgen_state' '--child' 'module-Cmo_format' '--child' 'module-Cmt2annot' '--child' 'module-Cmt_format' '--child' 'module-Cmx_format' '--child' 'module-Cmxs_format' '--child' 'module-Coloring' '--child' 'module-Comballoc' '--child' 'module-Compenv' '--child' 'module-Compilation_unit' '--child' 'module-Compile' '--child' 'module-Compile_common' '--child' 'module-Compilenv' '--child' 'module-Compmisc' '--child' 'module-Config' '--child' 'module-Config_boot' '--child' 'module-Config_main' '--child' 'module-Consistbl' '--child' 'module-Convert_primitives' '--child' 'module-Ctype' '--child' 'module-Dataflow' '--child' 'module-Datarepr' '--child' 'module-Deadcode' '--child' 'module-Debuginfo' '--child' 'module-Depend' '--child' 'module-Diffing' '--child' 'module-Diffing_with_keys' '--child' 'module-Dll' '--child' 'module-Docstrings' '--child' 'module-Domainstate' '--child' 'module-Effect_analysis' '--child' 'module-Emit' '--child' 'module-Emitaux' '--child' 'module-Emitcode' '--child' 'module-Emitenv' '--child' 'module-Env' '--child' 'module-Envaux' '--child' 'module-Errors' '--child' 'module-Errortrace' '--child' 'module-Export_id' '--child' 'module-Export_info' '--child' 'module-Export_info_for_pack' '--child' 'module-Expunge' '--child' 'module-Extract_projections' '--child' 'module-Find_recursive_functions' '--child' 'module-Flambda' '--child' 'module-Flambda_invariants' '--child' 'module-Flambda_iterators' '--child' 'module-Flambda_middle_end' '--child' 'module-Flambda_to_clambda' '--child' 'module-Flambda_utils' '--child' 'module-Freshening' '--child' 'module-Genprintval' '--child' 'module-Id_types' '--child' 'module-Ident' '--child' 'module-Identifiable' '--child' 'module-Import_approx' '--child' 'module-Includeclass' '--child' 'module-Includecore' '--child' 'module-Includemod' '--child' 'module-Includemod_errorprinter' '--child' 'module-Inconstant_idents' '--child' 'module-Initialize_symbol_to_let_symbol' '--child' 'module-Inline_and_simplify' '--child' 'module-Inline_and_simplify_aux' '--child' 'module-Inlining_cost' '--child' 'module-Inlining_decision' '--child' 'module-Inlining_decision_intf' '--child' 'module-Inlining_stats' '--child' 'module-Inlining_stats_types' '--child' 'module-Inlining_transforms' '--child' 'module-Instruct' '--child' 'module-Int_replace_polymorphic_compare' '--child' 'module-Interf' '--child' 'module-Internal_variable_names' '--child' 'module-Interval' '--child' 'module-Invariant_params' '--child' 'module-Lambda' '--child' 'module-Lazy_backtrack' '--child' 'module-Lexer' '--child' 'module-Lift_code' '--child' 'module-Lift_constants' '--child' 'module-Lift_let_to_initialize_symbol' '--child' 'module-Linear' '--child' 'module-Linear_format' '--child' 'module-Linearize' '--child' 'module-Linkage_name' '--child' 'module-Linscan' '--child' 'module-Liveness' '--child' 'module-Load_path' '--child' 'module-Local_store' '--child' 'module-Location' '--child' 'module-Longident' '--child' 'module-Mach' '--child' 'module-Main' '--child' 'module-Main_args' '--child' 'module-Maindriver' '--child' 'module-Makedepend' '--child' 'module-Matching' '--child' 'module-Meta' '--child' 'module-Misc' '--child' 'module-Mtype' '--child' 'module-Mutable_variable' '--child' 'module-Numbers' '--child' 'module-Opcodes' '--child' 'module-Oprint' '--child' 'module-Optcompile' '--child' 'module-Opterrors' '--child' 'module-Optmain' '--child' 'module-Optmaindriver' '--child' 'module-Outcometree' '--child' 'module-Parameter' '--child' 'module-Parmatch' '--child' 'module-Parse' '--child' 'module-Parser' '--child' 'module-Parsetree' '--child' 'module-Pass_wrapper' '--child' 'module-Path' '--child' 'module-Patterns' '--child' 'module-Persistent_env' '--child' 'module-Polling' '--child' 'module-Pparse' '--child' 'module-Pprintast' '--child' 'module-Predef' '--child' 'module-Primitive' '--child' 'module-Printast' '--child' 'module-Printclambda' '--child' 'module-Printclambda_primitives' '--child' 'module-Printcmm' '--child' 'module-Printinstr' '--child' 'module-Printlambda' '--child' 'module-Printlinear' '--child' 'module-Printmach' '--child' 'module-Printpat' '--child' 'module-Printtyp' '--child' 'module-Printtyped' '--child' 'module-Proc' '--child' 'module-Profile' '--child' 'module-Projection' '--child' 'module-Rec_check' '--child' 'module-Ref_to_variables' '--child' 'module-Reg' '--child' 'module-Reload' '--child' 'module-Reloadgen' '--child' 'module-Remove_free_vars_equal_to_args' '--child' 'module-Remove_unused_arguments' '--child' 'module-Remove_unused_closure_vars' '--child' 'module-Remove_unused_program_constructs' '--child' 'module-Runtimedef' '--child' 'module-Schedgen' '--child' 'module-Scheduling' '--child' 'module-Selectgen' '--child' 'module-Selection' '--child' 'module-Semantics_of_primitives' '--child' 'module-Set_of_closures_id' '--child' 'module-Set_of_closures_origin' '--child' 'module-Shape' '--child' 'module-Share_constants' '--child' 'module-Signature_group' '--child' 'module-Simple_value_approx' '--child' 'module-Simplif' '--child' 'module-Simplify_boxed_integer_ops' '--child' 'module-Simplify_boxed_integer_ops_intf' '--child' 'module-Simplify_common' '--child' 'module-Simplify_primitives' '--child' 'module-Spill' '--child' 'module-Split' '--child' 'module-Static_exception' '--child' 'module-Strmatch' '--child' 'module-Strongly_connected_components' '--child' 'module-Stypes' '--child' 'module-Subst' '--child' 'module-Switch' '--child' 'module-Symbol' '--child' 'module-Symtable' '--child' 'module-Syntaxerr' '--child' 'module-Tag' '--child' 'module-Targetint' '--child' 'module-Tast_iterator' '--child' 'module-Tast_mapper' '--child' 'module-Terminfo' '--child' 'module-Tmc' '--child' 'module-Topcommon' '--child' 'module-Topdirs' '--child' 'module-Topeval' '--child' 'module-Tophooks' '--child' 'module-Toploop' '--child' 'module-Topmain' '--child' 'module-Topstart' '--child' 'module-Trace' '--child' 'module-Translattribute' '--child' 'module-Translclass' '--child' 'module-Translcore' '--child' 'module-Translmod' '--child' 'module-Translobj' '--child' 'module-Translprim' '--child' 'module-Traverse_for_exported_symbols' '--child' 'module-Type_immediacy' '--child' 'module-Typeclass' '--child' 'module-Typecore' '--child' 'module-Typedecl' '--child' 'module-Typedecl_immediacy' '--child' 'module-Typedecl_properties' '--child' 'module-Typedecl_separability' '--child' 'module-Typedecl_unboxed' '--child' 'module-Typedecl_variance' '--child' 'module-Typedtree' '--child' 'module-Typemod' '--child' 'module-Typeopt' '--child' 'module-Types' '--child' 'module-Typetexp' '--child' 'module-Un_anf' '--child' 'module-Unbox_closures' '--child' 'module-Unbox_free_vars_of_closures' '--child' 'module-Unbox_specialised_args' '--child' 'module-Untypeast' '--child' 'module-Var_within_closure' '--child' 'module-Variable' '--child' 'module-Warnings' '--child' 'module-X86_ast' '--child' 'module-X86_dsl' '--child' 'module-X86_gas' '--child' 'module-X86_masm' '--child' 'module-X86_proc' '--child' 'module-Dynlink' '--child' 'module-Ocamlmktop_init' '--child' 'module-Profiling' '--child' 'module-Runtime_events' '--child' 'module-Std_exit' '--child' 'module-Stdlib' '--child' 'module-Stdlib__Arg' '--child' 'module-Stdlib__Array' '--child' 'module-Stdlib__ArrayLabels' '--child' 'module-Stdlib__Atomic' '--child' 'module-Stdlib__Bigarray' '--child' 'module-Stdlib__Bool' '--child' 'module-Stdlib__Buffer' '--child' 'module-Stdlib__Bytes' '--child' 'module-Stdlib__BytesLabels' '--child' 'module-Stdlib__Callback' '--child' 'module-Stdlib__Char' '--child' 'module-Stdlib__Complex' '--child' 'module-Stdlib__Condition' '--child' 'module-Stdlib__Digest' '--child' 'module-Stdlib__Domain' '--child' 'module-Stdlib__Effect' '--child' 'module-Stdlib__Either' '--child' 'module-Stdlib__Ephemeron' '--child' 'module-Stdlib__Filename' '--child' 'module-Stdlib__Float' '--child' 'module-Stdlib__Format' '--child' 'module-Stdlib__Fun' '--child' 'module-Stdlib__Gc' '--child' 'module-Stdlib__Hashtbl' '--child' 'module-Stdlib__In_channel' '--child' 'module-Stdlib__Int' '--child' 'module-Stdlib__Int32' '--child' 'module-Stdlib__Int64' '--child' 'module-Stdlib__Lazy' '--child' 'module-Stdlib__Lexing' '--child' 'module-Stdlib__List' '--child' 'module-Stdlib__ListLabels' '--child' 'module-Stdlib__Map' '--child' 'module-Stdlib__Marshal' '--child' 'module-Stdlib__MoreLabels' '--child' 'module-Stdlib__Mutex' '--child' 'module-Stdlib__Nativeint' '--child' 'module-Stdlib__Obj' '--child' 'module-Stdlib__Oo' '--child' 'module-Stdlib__Option' '--child' 'module-Stdlib__Out_channel' '--child' 'module-Stdlib__Parsing' '--child' 'module-Stdlib__Printexc' '--child' 'module-Stdlib__Printf' '--child' 'module-Stdlib__Queue' '--child' 'module-Stdlib__Random' '--child' 'module-Stdlib__Result' '--child' 'module-Stdlib__Scanf' '--child' 'module-Stdlib__Semaphore' '--child' 'module-Stdlib__Seq' '--child' 'module-Stdlib__Set' '--child' 'module-Stdlib__Stack' '--child' 'module-Stdlib__StdLabels' '--child' 'module-Stdlib__String' '--child' 'module-Stdlib__StringLabels' '--child' 'module-Stdlib__Sys' '--child' 'module-Stdlib__Uchar' '--child' 'module-Stdlib__Unit' '--child' 'module-Stdlib__Weak' '--child' 'module-Str' '--child' 'module-Event' '--child' 'module-Thread' '--child' 'module-Unix' '--child' 'module-UnixLabels' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/yojson.mld' '-I' '.' '-o' './page-yojson.odoc' '--child' 'module-Yojson' '--child' 'module-Yojson_biniou' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/biniou.mld' '-I' '.' '-o' './page-biniou.odoc' '--child' 'module-Bi_dump' '--child' 'module-Bi_inbuf' '--child' 'module-Bi_io' '--child' 'module-Bi_outbuf' '--child' 'module-Bi_share' '--child' 'module-Bi_stream' '--child' 'module-Bi_util' '--child' 'module-Bi_vint' '--parent' 'page-"deps"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_xref_test.mld' '-I' '.' '-o' './page-odoc_xref_test.odoc' '--child' 'module-Odoc_xref_test' '--child' 'module-Odoc_xref_test__Common' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_xref2.mld' '-I' '.' '-o' './page-odoc_xref2.odoc' '--child' 'module-Odoc_xref2' '--child' 'module-Odoc_xref2__Cfrag' '--child' 'module-Odoc_xref2__Compile' '--child' 'module-Odoc_xref2__Component' '--child' 'module-Odoc_xref2__Cpath' '--child' 'module-Odoc_xref2__Env' '--child' 'module-Odoc_xref2__Errors' '--child' 'module-Odoc_xref2__Expand_tools' '--child' 'module-Odoc_xref2__Find' '--child' 'module-Odoc_xref2__Ident' '--child' 'module-Odoc_xref2__Lang_of' '--child' 'module-Odoc_xref2__Link' '--child' 'module-Odoc_xref2__Lookup_failures' '--child' 'module-Odoc_xref2__Ref_tools' '--child' 'module-Odoc_xref2__Strengthen' '--child' 'module-Odoc_xref2__Subst' '--child' 'module-Odoc_xref2__Tools' '--child' 'module-Odoc_xref2__Type_of' '--child' 'module-Odoc_xref2__Utils' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_odoc.mld' '-I' '.' '-o' './page-odoc_odoc.odoc' '--child' 'module-Odoc_odoc' '--child' 'module-Odoc_odoc__Compile' '--child' 'module-Odoc_odoc__Depends' '--child' 'module-Odoc_odoc__Fs' '--child' 'module-Odoc_odoc__Html_fragment' '--child' 'module-Odoc_odoc__Html_page' '--child' 'module-Odoc_odoc__Latex' '--child' 'module-Odoc_odoc__Man_page' '--child' 'module-Odoc_odoc__Odoc_file' '--child' 'module-Odoc_odoc__Odoc_link' '--child' 'module-Odoc_odoc__Or_error' '--child' 'module-Odoc_odoc__Rendering' '--child' 'module-Odoc_odoc__Resolver' '--child' 'module-Odoc_odoc__Source_tree' '--child' 'module-Odoc_odoc__Support_files' '--child' 'module-Odoc_odoc__Url' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_html_support_files.mld' '-I' '.' '-o' './page-odoc_html_support_files.odoc' '--child' 'module-Odoc_html_support_files' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_model_desc.mld' '-I' '.' '-o' './page-odoc_model_desc.odoc' '--child' 'module-Odoc_model_desc' '--child' 'module-Odoc_model_desc__Comment_desc' '--child' 'module-Odoc_model_desc__Lang_desc' '--child' 'module-Odoc_model_desc__Paths_desc' '--child' 'module-Odoc_model_desc__Type_desc' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_model.mld' '-I' '.' '-o' './page-odoc_model.odoc' '--child' 'module-Odoc_model' '--child' 'module-Odoc_model__' '--child' 'module-Odoc_model__Comment' '--child' 'module-Odoc_model__Compat' '--child' 'module-Odoc_model__Error' '--child' 'module-Odoc_model__Lang' '--child' 'module-Odoc_model__Location_' '--child' 'module-Odoc_model__Names' '--child' 'module-Odoc_model__Paths' '--child' 'module-Odoc_model__Paths_types' '--child' 'module-Odoc_model__Predefined' '--child' 'module-Odoc_model__Reference' '--child' 'module-Odoc_model__Root' '--child' 'module-Odoc_model__Semantics' '--child' 'module-Odoc_model_desc' '--child' 'module-Odoc_model_desc__Comment_desc' '--child' 'module-Odoc_model_desc__Lang_desc' '--child' 'module-Odoc_model_desc__Paths_desc' '--child' 'module-Odoc_model_desc__Type_desc' '--child' 'module-Odoc_model_semantics_test' '--child' 'module-Odoc_model_semantics_test__Test' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_manpage.mld' '-I' '.' '-o' './page-odoc_manpage.odoc' '--child' 'module-Odoc_manpage' '--child' 'module-Odoc_manpage__Generator' '--child' 'module-Odoc_manpage__Link' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_loader.mld' '-I' '.' '-o' './page-odoc_loader.odoc' '--child' 'module-Odoc_loader' '--child' 'module-Odoc_loader__' '--child' 'module-Odoc_loader__Cmi' '--child' 'module-Odoc_loader__Cmt' '--child' 'module-Odoc_loader__Cmti' '--child' 'module-Odoc_loader__Doc_attr' '--child' 'module-Odoc_loader__Ident_env' '--child' 'module-Odoc_loader__Local_jmp' '--child' 'module-Odoc_loader__Lookup_def' '--child' 'module-Odoc_loader__Source_info' '--child' 'module-Odoc_loader__Uid' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_latex.mld' '-I' '.' '-o' './page-odoc_latex.odoc' '--child' 'module-Odoc_latex' '--child' 'module-Odoc_latex__Generator' '--child' 'module-Odoc_latex__Raw' '--child' 'module-Odoc_latex__Types' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_html.mld' '-I' '.' '-o' './page-odoc_html.odoc' '--child' 'module-Odoc_html' '--child' 'module-Odoc_html__' '--child' 'module-Odoc_html__Config' '--child' 'module-Odoc_html__Generator' '--child' 'module-Odoc_html__Html_fragment_json' '--child' 'module-Odoc_html__Html_page' '--child' 'module-Odoc_html__Html_source' '--child' 'module-Odoc_html__Link' '--child' 'module-Odoc_html__Types' '--child' 'module-Odoc_html__Utils' '--child' 'module-Odoc_html_support_files' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_document.mld' '-I' '.' '-o' './page-odoc_document.odoc' '--child' 'module-Odoc_document' '--child' 'module-Odoc_document__Codefmt' '--child' 'module-Odoc_document__Comment' '--child' 'module-Odoc_document__Compat' '--child' 'module-Odoc_document__Doctree' '--child' 'module-Odoc_document__Generator' '--child' 'module-Odoc_document__Generator_signatures' '--child' 'module-Odoc_document__ML' '--child' 'module-Odoc_document__Reason' '--child' 'module-Odoc_document__Renderer' '--child' 'module-Odoc_document__Targets' '--child' 'module-Odoc_document__Types' '--child' 'module-Odoc_document__Url' '--child' 'module-Odoc_document__Utils' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'compile' 'library_mlds/odoc_examples.mld' '-I' '.' '-o' './page-odoc_examples.odoc' '--child' 'module-Odoc_examples' '--child' 'module-Odoc_examples__' '--child' 'module-Odoc_examples__Expansion' '--child' 'module-Odoc_examples__Markup' '--child' 'module-Odoc_examples__Resolution' '--child' 'module-Odoc_examples__Unexposed' '--child' 'module-Odoc_examples__Wrapping' '--parent' 'page-"odoc"'
$ '../src/odoc/bin/main.exe' 'source-tree' '-I' '.' '--parent' 'page-"odoc"' '-o' 'src-source.odoc' 'source.map'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../test/xref2/lib/.odoc_xref_test.objs/byte/odoc_xref_test__Common.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/warnings.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/camlinternalFormatBasics.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/camlinternalFormatBasics.cmti' '-I' '.' '-o' './camlinternalFormatBasics.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/camlinternalLazy.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib.cmti' '-I' '.' '-o' './stdlib.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/camlinternalLazy.cmti' '-I' '.' '-o' './camlinternalLazy.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Lazy.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Lazy.cmti' '-I' '.' '-o' './stdlib__Lazy.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Lexing.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Lexing.cmti' '-I' '.' '-o' './stdlib__Lexing.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Sys.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Sys.cmti' '-I' '.' '-o' './stdlib__Sys.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/warnings.cmti' '-I' '.' '-o' './warnings.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/types.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/asttypes.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/location.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Buffer.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Either.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Either.cmti' '-I' '.' '-o' './stdlib__Either.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Seq.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Seq.cmti' '-I' '.' '-o' './stdlib__Seq.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Uchar.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Uchar.cmti' '-I' '.' '-o' './stdlib__Uchar.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Buffer.cmti' '-I' '.' '-o' './stdlib__Buffer.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Domain.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Domain.cmti' '-I' '.' '-o' './stdlib__Domain.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Format.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Format.cmti' '-I' '.' '-o' './stdlib__Format.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/location.cmti' '-I' '.' '-o' './location.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/asttypes.cmti' '-I' '.' '-o' './asttypes.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ident.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/identifiable.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Hashtbl.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Hashtbl.cmti' '-I' '.' '-o' './stdlib__Hashtbl.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Map.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Map.cmti' '-I' '.' '-o' './stdlib__Map.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Set.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Set.cmti' '-I' '.' '-o' './stdlib__Set.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/identifiable.cmti' '-I' '.' '-o' './identifiable.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ident.cmti' '-I' '.' '-o' './ident.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/longident.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/longident.cmti' '-I' '.' '-o' './longident.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/outcometree.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/parsetree.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/parsetree.cmti' '-I' '.' '-o' './parsetree.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/type_immediacy.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/type_immediacy.cmti' '-I' '.' '-o' './type_immediacy.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/outcometree.cmti' '-I' '.' '-o' './outcometree.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/path.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/path.cmti' '-I' '.' '-o' './path.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/primitive.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/primitive.cmti' '-I' '.' '-o' './primitive.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/shape.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/shape.cmti' '-I' '.' '-o' './shape.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/types.cmti' '-I' '.' '-o' './types.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typemod.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/btype.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/btype.cmti' '-I' '.' '-o' './btype.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/build_path_prefix_map.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/build_path_prefix_map.cmti' '-I' '.' '-o' './build_path_prefix_map.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmi_format.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/misc.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Digest.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Digest.cmti' '-I' '.' '-o' './stdlib__Digest.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__String.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__String.cmti' '-I' '.' '-o' './stdlib__String.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/misc.cmti' '-I' '.' '-o' './misc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmi_format.cmti' '-I' '.' '-o' './cmi_format.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ctype.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/env.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/load_path.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/load_path.cmti' '-I' '.' '-o' './load_path.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/subst.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/subst.cmti' '-I' '.' '-o' './subst.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/env.cmti' '-I' '.' '-o' './env.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/errortrace.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/errortrace.cmti' '-I' '.' '-o' './errortrace.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ctype.cmti' '-I' '.' '-o' './ctype.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/diffing.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/diffing.cmti' '-I' '.' '-o' './diffing.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/diffing_with_keys.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/diffing_with_keys.cmti' '-I' '.' '-o' './diffing_with_keys.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/includecore.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typedtree.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typedtree.cmti' '-I' '.' '-o' './typedtree.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/includecore.cmti' '-I' '.' '-o' './includecore.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/includemod.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/includemod.cmti' '-I' '.' '-o' './includemod.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typedecl.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typedecl_immediacy.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typedecl_properties.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typedecl_properties.cmti' '-I' '.' '-o' './typedecl_properties.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typedecl_immediacy.cmti' '-I' '.' '-o' './typedecl_immediacy.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typedecl_separability.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typedecl_separability.cmti' '-I' '.' '-o' './typedecl_separability.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typedecl_variance.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typedecl_variance.cmti' '-I' '.' '-o' './typedecl_variance.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typedecl.cmti' '-I' '.' '-o' './typedecl.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typemod.cmti' '-I' '.' '-o' './typemod.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/toploop.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Int32.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Int32.cmti' '-I' '.' '-o' './stdlib__Int32.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Obj.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Obj.cmti' '-I' '.' '-o' './stdlib__Obj.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/toploop.cmti' '-I' '.' '-o' './toploop.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Result.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Result.cmti' '-I' '.' '-o' './stdlib__Result.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Printf.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Printf.cmti' '-I' '.' '-o' './stdlib__Printf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__List.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__List.cmti' '-I' '.' '-o' './stdlib__List.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Array.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Array.cmti' '-I' '.' '-o' './stdlib__Array.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Arg.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Arg.cmti' '-I' '.' '-o' './stdlib__Arg.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/result/result.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/result/result.cmt' '-I' '.' '-o' './result.odoc' '--parent' 'page-"result"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/profile.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/profile.cmti' '-I' '.' '-o' './profile.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/parse.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/parse.cmti' '-I' '.' '-o' './parse.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../test/xref2/lib/.odoc_xref_test.objs/byte/odoc_xref_test.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../test/xref2/lib/.odoc_xref_test.objs/byte/odoc_xref_test.cmt' '-I' '.' '-o' './odoc_xref_test.odoc' '--parent' 'page-"odoc_xref_test"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Tools.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/astring/astring.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/astring/astring.cmti' '-I' '.' '-o' './astring.odoc' '--parent' 'page-"astring"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__.cmt' '-I' '.' '-o' './odoc_model__.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model.cmt' '--source-name' 'src/model/odoc_model.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Comment.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__Warning.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__Loc.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__.cmt' '-I' '.' '-o' './odoc_parser__.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__Loc.cmti' '-I' '.' '-o' './odoc_parser__Loc.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__Warning.cmt' '-I' '.' '-o' './odoc_parser__Warning.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__Ast.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__Ast.cmt' '-I' '.' '-o' './odoc_parser__Ast.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser.cmti' '-I' '.' '-o' './odoc_parser.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Paths_types.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Names.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Names.cmti' '--source-name' 'src/model/names.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Names.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Paths_types.cmt' '--source-name' 'src/model/paths_types.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Paths_types.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Paths.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Paths.cmti' '--source-name' 'src/model/paths.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Paths.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Location_.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Location_.cmti' '--source-name' 'src/model/location_.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Location_.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Comment.cmt' '--source-name' 'src/model/comment.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Comment.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Error.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Error.cmti' '--source-name' 'src/model/error.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Error.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Lang.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Root.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Root.cmti' '--source-name' 'src/model/root.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Root.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Lang.cmt' '--source-name' 'src/model/lang.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Lang.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2.cmt' '-I' '.' '-o' './odoc_xref2.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Cfrag.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Ident.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Ident.cmt' '--source-name' 'src/xref2/ident.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Ident.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Cpath.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Cpath.cmt' '--source-name' 'src/xref2/cpath.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Cpath.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Cfrag.cmt' '--source-name' 'src/xref2/cfrag.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Cfrag.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Component.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Component.cmti' '--source-name' 'src/xref2/component.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Component.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Env.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Env.cmti' '--source-name' 'src/xref2/env.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Env.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Errors.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Lookup_failures.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Lookup_failures.cmti' '--source-name' 'src/xref2/lookup_failures.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Lookup_failures.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Errors.cmt' '--source-name' 'src/xref2/errors.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Errors.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Find.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Find.cmti' '--source-name' 'src/xref2/find.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Find.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Tools.cmti' '--source-name' 'src/xref2/tools.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Tools.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Compile.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Compile.cmti' '--source-name' 'src/xref2/compile.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Compile.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Resolver.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmt_format.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmt_format.cmti' '-I' '.' '-o' './cmt_format.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/fpath/fpath.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/fpath/fpath.cmti' '-I' '.' '-o' './fpath.odoc' '--parent' 'page-"fpath"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__.cmt' '-I' '.' '-o' './odoc_loader__.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Lookup_def.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Lookup_def.cmti' '--source-name' 'src/loader/lookup_def.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Lookup_def.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Source_info.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Source_info.cmti' '--source-name' 'src/loader/source_info.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Source_info.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader.cmti' '--source-name' 'src/loader/odoc_loader.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc.cmt' '-I' '.' '-o' './odoc_odoc.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Fs.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Or_error.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Or_error.cmti' '--source-name' 'src/odoc/or_error.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Or_error.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Fs.cmti' '--source-name' 'src/odoc/fs.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Fs.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Resolver.cmti' '--source-name' 'src/odoc/resolver.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Resolver.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Compat.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Compat.cmt' '--source-name' 'src/model/compat.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Compat.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Ident_env.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Ident_env.cmti' '--source-name' 'src/loader/ident_env.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Ident_env.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Cmti.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Cmti.cmti' '--source-name' 'src/loader/cmti.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Cmti.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Cmt.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Cmt.cmti' '--source-name' 'src/loader/cmt.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Cmt.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/compmisc.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/clflags.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/clflags.cmti' '-I' '.' '-o' './clflags.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/compmisc.cmti' '-I' '.' '-o' './compmisc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../test/xref2/lib/.odoc_xref_test.objs/byte/odoc_xref_test__Common.cmt' '-I' '.' '-o' './odoc_xref_test__Common.odoc' '--parent' 'page-"odoc_xref_test"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Utils.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Utils.cmt' '--source-name' 'src/xref2/utils.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Utils.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Type_of.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Type_of.cmti' '--source-name' 'src/xref2/type_of.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Type_of.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Subst.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Subst.cmti' '--source-name' 'src/xref2/subst.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Subst.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Strengthen.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Strengthen.cmt' '--source-name' 'src/xref2/strengthen.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Strengthen.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Ref_tools.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Ref_tools.cmti' '--source-name' 'src/xref2/ref_tools.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Ref_tools.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Link.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Link.cmti' '--source-name' 'src/xref2/link.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Link.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Lang_of.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Lang_of.cmti' '--source-name' 'src/xref2/lang_of.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Lang_of.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Expand_tools.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/xref2/.odoc_xref2.objs/byte/odoc_xref2__Expand_tools.cmt' '--source-name' 'src/xref2/expand_tools.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_xref2__Expand_tools.odoc' '--parent' 'page-"odoc_xref2"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Url.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/xml_wrap.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/xml_wrap.cmti' '-I' '.' '-o' './xml_wrap.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/xml_stream.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/xml_sigs.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/xml_sigs.cmti' '-I' '.' '-o' './xml_sigs.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/xml_stream.cmti' '-I' '.' '-o' './xml_stream.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/tyxml_xml.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/tyxml_xml.cmti' '-I' '.' '-o' './tyxml_xml.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/tyxml_svg.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/svg_sigs.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/svg_types.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/svg_types.cmti' '-I' '.' '-o' './svg_types.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/svg_sigs.cmti' '-I' '.' '-o' './svg_sigs.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/tyxml_svg.cmti' '-I' '.' '-o' './tyxml_svg.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/tyxml_html.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/html_sigs.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/html_types.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/html_types.cmti' '-I' '.' '-o' './html_types.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/html_sigs.cmti' '-I' '.' '-o' './html_sigs.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/tyxml_html.cmti' '-I' '.' '-o' './tyxml_html.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/tyxml.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/tyxml.cmt' '-I' '.' '-o' './tyxml.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Html_page.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document.cmt' '-I' '.' '-o' './odoc_document.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__ML.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Types.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Url.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Url.cmti' '--source-name' 'src/document/url.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Url.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Types.cmt' '--source-name' 'src/document/types.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Types.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__ML.cmti' '--source-name' 'src/document/ML.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__ML.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Reason.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Reason.cmti' '--source-name' 'src/document/reason.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Reason.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Renderer.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Renderer.cmt' '--source-name' 'src/document/renderer.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Renderer.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__.cmt' '-I' '.' '-o' './odoc_html__.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html.cmt' '--source-name' 'src/html/odoc_html.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Config.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Types.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Types.cmt' '--source-name' 'src/html/types.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Types.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Config.cmti' '--source-name' 'src/html/config.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Config.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Html_page.cmti' '--source-name' 'src/odoc/html_page.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Html_page.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Semantics.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Semantics.cmti' '--source-name' 'src/model/semantics.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Semantics.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/latex/.odoc_latex.objs/byte/odoc_latex__Generator.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/latex/.odoc_latex.objs/byte/odoc_latex.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/latex/.odoc_latex.objs/byte/odoc_latex.cmt' '-I' '.' '-o' './odoc_latex.odoc' '--parent' 'page-"odoc_latex"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/latex/.odoc_latex.objs/byte/odoc_latex__Generator.cmti' '--source-name' 'src/latex/generator.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_latex__Generator.odoc' '--parent' 'page-"odoc_latex"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Link.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Link.cmti' '--source-name' 'src/html/link.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Link.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Url.cmt' '--source-name' 'src/odoc/url.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Url.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Support_files.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Support_files.cmti' '--source-name' 'src/odoc/support_files.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Support_files.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Source_tree.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Source_tree.cmti' '--source-name' 'src/odoc/source_tree.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Source_tree.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Rendering.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Rendering.cmti' '--source-name' 'src/odoc/rendering.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Rendering.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Odoc_link.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Odoc_file.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Odoc_file.cmti' '--source-name' 'src/odoc/odoc_file.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Odoc_file.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Odoc_link.cmt' '--source-name' 'src/odoc/odoc_link.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Odoc_link.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Man_page.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/manpage/.odoc_manpage.objs/byte/odoc_manpage__Generator.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/manpage/.odoc_manpage.objs/byte/odoc_manpage.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/manpage/.odoc_manpage.objs/byte/odoc_manpage.cmt' '-I' '.' '-o' './odoc_manpage.odoc' '--parent' 'page-"odoc_manpage"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/manpage/.odoc_manpage.objs/byte/odoc_manpage__Generator.cmti' '--source-name' 'src/manpage/generator.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_manpage__Generator.odoc' '--parent' 'page-"odoc_manpage"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Man_page.cmt' '--source-name' 'src/odoc/man_page.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Man_page.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Latex.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Latex.cmt' '--source-name' 'src/odoc/latex.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Latex.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Html_fragment.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Html_fragment.cmti' '--source-name' 'src/odoc/html_fragment.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Html_fragment.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Depends.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Depends.cmti' '--source-name' 'src/odoc/depends.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Depends.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Compile.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/odoc/.odoc_odoc.objs/byte/odoc_odoc__Compile.cmti' '--source-name' 'src/odoc/compile.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_odoc__Compile.odoc' '--parent' 'page-"odoc_odoc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html_support_files/.odoc_html_support_files.objs/byte/odoc_html_support_files.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html_support_files/.odoc_html_support_files.objs/byte/odoc_html_support_files.cmt' '--source-name' 'src/html_support_files/odoc_html_support_files.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html_support_files.odoc' '--parent' 'page-"odoc_html_support_files"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Type_desc.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc.cmt' '-I' '.' '-o' './odoc_model_desc.odoc' '--parent' 'page-"odoc_model_desc"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Type_desc.cmt' '--source-name' 'src/model_desc/type_desc.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model_desc__Type_desc.odoc' '--parent' 'page-"odoc_model_desc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Paths_desc.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Paths_desc.cmti' '--source-name' 'src/model_desc/paths_desc.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model_desc__Paths_desc.odoc' '--parent' 'page-"odoc_model_desc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Lang_desc.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Comment_desc.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Comment_desc.cmti' '--source-name' 'src/model_desc/comment_desc.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model_desc__Comment_desc.odoc' '--parent' 'page-"odoc_model_desc"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model_desc/.odoc_model_desc.objs/byte/odoc_model_desc__Lang_desc.cmt' '--source-name' 'src/model_desc/lang_desc.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model_desc__Lang_desc.odoc' '--parent' 'page-"odoc_model_desc"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../test/model/semantics/.odoc_model_semantics_test.objs/byte/odoc_model_semantics_test__Test.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/yojson/yojson.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_outbuf.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_share.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_share.cmti' '-I' '.' '-o' './bi_share.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_outbuf.cmti' '-I' '.' '-o' './bi_outbuf.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/yojson/yojson.cmti' '-I' '.' '-o' './yojson.odoc' '--parent' 'page-"yojson"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__StringLabels.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__StringLabels.cmti' '-I' '.' '-o' './stdlib__StringLabels.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__StdLabels.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__StdLabels.cmti' '-I' '.' '-o' './stdlib__StdLabels.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Printexc.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Printexc.cmti' '-I' '.' '-o' './stdlib__Printexc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Nativeint.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Nativeint.cmti' '-I' '.' '-o' './stdlib__Nativeint.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__MoreLabels.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__MoreLabels.cmti' '-I' '.' '-o' './stdlib__MoreLabels.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__ListLabels.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__ListLabels.cmti' '-I' '.' '-o' './stdlib__ListLabels.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Int64.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Int64.cmti' '-I' '.' '-o' './stdlib__Int64.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__BytesLabels.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__BytesLabels.cmti' '-I' '.' '-o' './stdlib__BytesLabels.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../test/model/semantics/.odoc_model_semantics_test.objs/byte/odoc_model_semantics_test.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../test/model/semantics/.odoc_model_semantics_test.objs/byte/odoc_model_semantics_test.cmt' '-I' '.' '-o' './odoc_model_semantics_test.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile' '../test/model/semantics/.odoc_model_semantics_test.objs/byte/odoc_model_semantics_test__Test.cmt' '-I' '.' '-o' './odoc_model_semantics_test__Test.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Reference.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Reference.cmti' '--source-name' 'src/model/reference.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Reference.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/model/.odoc_model.objs/byte/odoc_model__Predefined.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/model/.odoc_model.objs/byte/odoc_model__Predefined.cmti' '--source-name' 'src/model/predefined.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_model__Predefined.odoc' '--parent' 'page-"odoc_model"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/manpage/.odoc_manpage.objs/byte/odoc_manpage__Link.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/manpage/.odoc_manpage.objs/byte/odoc_manpage__Link.cmt' '--source-name' 'src/manpage/link.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_manpage__Link.odoc' '--parent' 'page-"odoc_manpage"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Uid.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Uid.cmti' '--source-name' 'src/loader/uid.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Uid.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Local_jmp.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Local_jmp.cmti' '--source-name' 'src/loader/local_jmp.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Local_jmp.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Doc_attr.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Doc_attr.cmti' '--source-name' 'src/loader/doc_attr.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Doc_attr.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Cmi.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/loader/.odoc_loader.objs/byte/odoc_loader__Cmi.cmti' '--source-name' 'src/loader/cmi.pp.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_loader__Cmi.odoc' '--parent' 'page-"odoc_loader"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/latex/.odoc_latex.objs/byte/odoc_latex__Types.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/latex/.odoc_latex.objs/byte/odoc_latex__Types.cmt' '--source-name' 'src/latex/types.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_latex__Types.odoc' '--parent' 'page-"odoc_latex"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/latex/.odoc_latex.objs/byte/odoc_latex__Raw.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/fmt/fmt.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Queue.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Queue.cmti' '-I' '.' '-o' './stdlib__Queue.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Stack.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Stack.cmti' '-I' '.' '-o' './stdlib__Stack.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/fmt/fmt.cmti' '-I' '.' '-o' './fmt.odoc' '--parent' 'page-"fmt"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/latex/.odoc_latex.objs/byte/odoc_latex__Raw.cmti' '--source-name' 'src/latex/raw.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_latex__Raw.odoc' '--parent' 'page-"odoc_latex"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Utils.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Char.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Char.cmti' '-I' '.' '-o' './stdlib__Char.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Utils.cmt' '--source-name' 'src/html/utils.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Utils.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Html_source.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Html_source.cmti' '--source-name' 'src/html/html_source.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Html_source.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Html_page.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Html_page.cmti' '--source-name' 'src/html/html_page.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Html_page.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Html_fragment_json.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Html_fragment_json.cmti' '--source-name' 'src/html/html_fragment_json.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Html_fragment_json.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/html/.odoc_html.objs/byte/odoc_html__Generator.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/html/.odoc_html.objs/byte/odoc_html__Generator.cmti' '--source-name' 'src/html/generator.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_html__Generator.odoc' '--parent' 'page-"odoc_html"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Utils.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Utils.cmti' '--source-name' 'src/document/utils.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Utils.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Targets.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Targets.cmti' '--source-name' 'src/document/targets.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Targets.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Generator_signatures.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Codefmt.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Codefmt.cmti' '--source-name' 'src/document/codefmt.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Codefmt.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Generator_signatures.cmt' '--source-name' 'src/document/generator_signatures.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Generator_signatures.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Generator.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Generator.cmti' '--source-name' 'src/document/generator.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Generator.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Doctree.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Doctree.cmt' '--source-name' 'src/document/doctree.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Doctree.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Compat.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Compat.cmt' '--source-name' 'src/document/compat.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Compat.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../src/document/.odoc_document.objs/byte/odoc_document__Comment.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../src/document/.odoc_document.objs/byte/odoc_document__Comment.cmt' '--source-name' 'src/document/comment.ml' '--source-parent-file' 'src-source.odoc' '-I' '.' '-o' './odoc_document__Comment.odoc' '--parent' 'page-"odoc_document"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Wrapping.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__.cmt' '-I' '.' '-o' './odoc_examples__.odoc' '--parent' 'page-"odoc_examples"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Unexposed.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Unexposed.cmti' '-I' '.' '-o' './odoc_examples__Unexposed.odoc' '--parent' 'page-"odoc_examples"'
$ '../src/odoc/bin/main.exe' 'compile' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Wrapping.cmti' '-I' '.' '-o' './odoc_examples__Wrapping.odoc' '--parent' 'page-"odoc_examples"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Resolution.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Resolution.cmti' '-I' '.' '-o' './odoc_examples__Resolution.odoc' '--parent' 'page-"odoc_examples"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Markup.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Markup.cmti' '-I' '.' '-o' './odoc_examples__Markup.odoc' '--parent' 'page-"odoc_examples"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Expansion.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../doc/examples/.odoc_examples.objs/byte/odoc_examples__Expansion.cmti' '-I' '.' '-o' './odoc_examples__Expansion.odoc' '--parent' 'page-"odoc_examples"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../doc/examples/.odoc_examples.objs/byte/odoc_examples.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../doc/examples/.odoc_examples.objs/byte/odoc_examples.cmt' '-I' '.' '-o' './odoc_examples.odoc' '--parent' 'page-"odoc_examples"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__Token.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__Token.cmt' '-I' '.' '-o' './odoc_parser__Token.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__Syntax.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__Syntax.cmti' '-I' '.' '-o' './odoc_parser__Syntax.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__Parse_error.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__Parse_error.cmt' '-I' '.' '-o' './odoc_parser__Parse_error.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/odoc-parser/odoc_parser__Lexer.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/odoc-parser/odoc_parser__Lexer.cmti' '-I' '.' '-o' './odoc_parser__Lexer.odoc' '--parent' 'page-"odoc-parser"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/cmdliner/cmdliner.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/cmdliner/cmdliner.cmti' '-I' '.' '-o' './cmdliner.odoc' '--parent' 'page-"cmdliner"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/xml_print.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/xml_print.cmti' '-I' '.' '-o' './xml_print.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/xml_iter.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/xml_iter.cmti' '-I' '.' '-o' './xml_iter.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/svg_f.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/svg_f.cmti' '-I' '.' '-o' './svg_f.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/tyxml/functor/html_f.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/tyxml/functor/html_f.cmti' '-I' '.' '-o' './html_f.odoc' '--parent' 'page-"tyxml"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/fmt/fmt_tty.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/fmt/fmt_tty.cmti' '-I' '.' '-o' './fmt_tty.odoc' '--parent' 'page-"fmt"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/fmt/fmt_cli.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/fmt/fmt_cli.cmti' '-I' '.' '-o' './fmt_cli.odoc' '--parent' 'page-"fmt"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/unix/unixLabels.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Bigarray.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Complex.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Complex.cmti' '-I' '.' '-o' './stdlib__Complex.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Bigarray.cmti' '-I' '.' '-o' './stdlib__Bigarray.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/unix/unix.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/unix/unix.cmti' '-I' '.' '-o' './unix.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/unix/unixLabels.cmti' '-I' '.' '-o' './unixLabels.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/threads/thread.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/threads/thread.cmti' '-I' '.' '-o' './thread.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/threads/event.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/threads/event.cmti' '-I' '.' '-o' './event.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/str/str.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/str/str.cmti' '-I' '.' '-o' './str.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Weak.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Weak.cmti' '-I' '.' '-o' './stdlib__Weak.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Unit.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Unit.cmti' '-I' '.' '-o' './stdlib__Unit.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Semaphore.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Semaphore.cmti' '-I' '.' '-o' './stdlib__Semaphore.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Scanf.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Scanf.cmti' '-I' '.' '-o' './stdlib__Scanf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Random.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Random.cmti' '-I' '.' '-o' './stdlib__Random.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Parsing.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Parsing.cmti' '-I' '.' '-o' './stdlib__Parsing.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Out_channel.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Out_channel.cmti' '-I' '.' '-o' './stdlib__Out_channel.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Option.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Option.cmti' '-I' '.' '-o' './stdlib__Option.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Oo.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/camlinternalOO.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/camlinternalOO.cmti' '-I' '.' '-o' './camlinternalOO.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Oo.cmti' '-I' '.' '-o' './stdlib__Oo.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Mutex.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Mutex.cmti' '-I' '.' '-o' './stdlib__Mutex.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Marshal.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Marshal.cmti' '-I' '.' '-o' './stdlib__Marshal.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Int.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Int.cmti' '-I' '.' '-o' './stdlib__Int.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__In_channel.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__In_channel.cmti' '-I' '.' '-o' './stdlib__In_channel.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Gc.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Gc.cmti' '-I' '.' '-o' './stdlib__Gc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Fun.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Fun.cmti' '-I' '.' '-o' './stdlib__Fun.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Float.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Float.cmti' '-I' '.' '-o' './stdlib__Float.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Filename.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Filename.cmti' '-I' '.' '-o' './stdlib__Filename.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Ephemeron.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Ephemeron.cmti' '-I' '.' '-o' './stdlib__Ephemeron.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Effect.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Effect.cmti' '-I' '.' '-o' './stdlib__Effect.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Condition.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Condition.cmti' '-I' '.' '-o' './stdlib__Condition.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Callback.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Callback.cmti' '-I' '.' '-o' './stdlib__Callback.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Bytes.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Bytes.cmti' '-I' '.' '-o' './stdlib__Bytes.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Bool.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Bool.cmti' '-I' '.' '-o' './stdlib__Bool.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__Atomic.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__Atomic.cmti' '-I' '.' '-o' './stdlib__Atomic.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/stdlib__ArrayLabels.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/stdlib__ArrayLabels.cmti' '-I' '.' '-o' './stdlib__ArrayLabels.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/std_exit.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/std_exit.cmt' '-I' '.' '-o' './std_exit.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/runtime_events/runtime_events.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/runtime_events/runtime_events.cmti' '-I' '.' '-o' './runtime_events.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/profiling/profiling.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/profiling/profiling.cmti' '-I' '.' '-o' './profiling.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/ocamlmktop/ocamlmktop_init.cmi'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/genprintval.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/genprintval.cmti' '-I' '.' '-o' './genprintval.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/topcommon.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/topcommon.cmti' '-I' '.' '-o' './topcommon.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/ocamlmktop/ocamlmktop_init.cmi' '-I' '.' '-o' './ocamlmktop_init.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/dynlink/dynlink.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/dynlink/dynlink.cmti' '-I' '.' '-o' './dynlink.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/x86_proc.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/x86_ast.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/x86_ast.cmti' '-I' '.' '-o' './x86_ast.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/x86_proc.cmti' '-I' '.' '-o' './x86_proc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/x86_masm.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/x86_masm.cmti' '-I' '.' '-o' './x86_masm.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/x86_gas.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/x86_gas.cmti' '-I' '.' '-o' './x86_gas.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/x86_dsl.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/x86_dsl.cmti' '-I' '.' '-o' './x86_dsl.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/variable.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/compilation_unit.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/linkage_name.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/linkage_name.cmti' '-I' '.' '-o' './linkage_name.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/compilation_unit.cmti' '-I' '.' '-o' './compilation_unit.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/debuginfo.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/debuginfo.cmti' '-I' '.' '-o' './debuginfo.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/internal_variable_names.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/lambda.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/lambda.cmti' '-I' '.' '-o' './lambda.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/internal_variable_names.cmti' '-I' '.' '-o' './internal_variable_names.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/variable.cmti' '-I' '.' '-o' './variable.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/var_within_closure.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure_element.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure_element.cmti' '-I' '.' '-o' './closure_element.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/var_within_closure.cmti' '-I' '.' '-o' './var_within_closure.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/untypeast.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/untypeast.cmti' '-I' '.' '-o' './untypeast.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/unbox_specialised_args.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/allocated_const.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/allocated_const.cmti' '-I' '.' '-o' './allocated_const.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/backend_intf.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/clambda_primitives.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/clambda_primitives.cmti' '-I' '.' '-o' './clambda_primitives.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure_id.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure_id.cmti' '-I' '.' '-o' './closure_id.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure_origin.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure_origin.cmti' '-I' '.' '-o' './closure_origin.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/export_id.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/export_id.cmti' '-I' '.' '-o' './export_id.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/flambda.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/mutable_variable.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/mutable_variable.cmti' '-I' '.' '-o' './mutable_variable.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/numbers.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/numbers.cmti' '-I' '.' '-o' './numbers.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/parameter.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/parameter.cmti' '-I' '.' '-o' './parameter.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/projection.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/projection.cmti' '-I' '.' '-o' './projection.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/set_of_closures_id.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/set_of_closures_id.cmti' '-I' '.' '-o' './set_of_closures_id.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/set_of_closures_origin.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/set_of_closures_origin.cmti' '-I' '.' '-o' './set_of_closures_origin.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/static_exception.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/static_exception.cmti' '-I' '.' '-o' './static_exception.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/symbol.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/symbol.cmti' '-I' '.' '-o' './symbol.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/tag.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/tag.cmti' '-I' '.' '-o' './tag.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/flambda.cmti' '-I' '.' '-o' './flambda.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/freshening.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/freshening.cmti' '-I' '.' '-o' './freshening.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/simple_value_approx.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/simple_value_approx.cmti' '-I' '.' '-o' './simple_value_approx.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/backend_intf.cmti' '-I' '.' '-o' './backend_intf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inline_and_simplify_aux.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inlining_cost.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inlining_cost.cmti' '-I' '.' '-o' './inlining_cost.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inlining_stats_types.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inlining_stats_types.cmti' '-I' '.' '-o' './inlining_stats_types.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inline_and_simplify_aux.cmti' '-I' '.' '-o' './inline_and_simplify_aux.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/unbox_specialised_args.cmti' '-I' '.' '-o' './unbox_specialised_args.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/unbox_free_vars_of_closures.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/unbox_free_vars_of_closures.cmti' '-I' '.' '-o' './unbox_free_vars_of_closures.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/unbox_closures.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/unbox_closures.cmti' '-I' '.' '-o' './unbox_closures.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/un_anf.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/backend_var.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/backend_var.cmti' '-I' '.' '-o' './backend_var.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/clambda.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/clambda.cmti' '-I' '.' '-o' './clambda.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/un_anf.cmti' '-I' '.' '-o' './un_anf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typetexp.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typetexp.cmti' '-I' '.' '-o' './typetexp.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typeopt.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typeopt.cmti' '-I' '.' '-o' './typeopt.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typedecl_unboxed.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typedecl_unboxed.cmti' '-I' '.' '-o' './typedecl_unboxed.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typecore.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typecore.cmti' '-I' '.' '-o' './typecore.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/typeclass.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/typeclass.cmti' '-I' '.' '-o' './typeclass.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/traverse_for_exported_symbols.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/export_info.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/export_info.cmti' '-I' '.' '-o' './export_info.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/traverse_for_exported_symbols.cmti' '-I' '.' '-o' './traverse_for_exported_symbols.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/translprim.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/translprim.cmti' '-I' '.' '-o' './translprim.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/translobj.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/translobj.cmti' '-I' '.' '-o' './translobj.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/translmod.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/translmod.cmti' '-I' '.' '-o' './translmod.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/translcore.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/translcore.cmti' '-I' '.' '-o' './translcore.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/translclass.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/translclass.cmti' '-I' '.' '-o' './translclass.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/translattribute.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/translattribute.cmti' '-I' '.' '-o' './translattribute.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/trace.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/trace.cmti' '-I' '.' '-o' './trace.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/topstart.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/topmain.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/topmain.cmti' '-I' '.' '-o' './topmain.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/topstart.cmt' '-I' '.' '-o' './topstart.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/tophooks.cmi'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/tophooks.cmi' '-I' '.' '-o' './tophooks.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/topeval.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/topeval.cmti' '-I' '.' '-o' './topeval.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/topdirs.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/topdirs.cmti' '-I' '.' '-o' './topdirs.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/tmc.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/tmc.cmti' '-I' '.' '-o' './tmc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/terminfo.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/terminfo.cmti' '-I' '.' '-o' './terminfo.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/tast_mapper.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/tast_mapper.cmti' '-I' '.' '-o' './tast_mapper.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/tast_iterator.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/tast_iterator.cmti' '-I' '.' '-o' './tast_iterator.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/targetint.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/targetint.cmti' '-I' '.' '-o' './targetint.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/syntaxerr.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/syntaxerr.cmti' '-I' '.' '-o' './syntaxerr.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/symtable.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmo_format.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmo_format.cmti' '-I' '.' '-o' './cmo_format.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/symtable.cmti' '-I' '.' '-o' './symtable.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/switch.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/switch.cmti' '-I' '.' '-o' './switch.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/stypes.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/annot.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/annot.cmti' '-I' '.' '-o' './annot.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/stypes.cmti' '-I' '.' '-o' './stypes.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/strongly_connected_components.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/strongly_connected_components.cmti' '-I' '.' '-o' './strongly_connected_components.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/strmatch.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmm.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmm.cmti' '-I' '.' '-o' './cmm.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/strmatch.cmti' '-I' '.' '-o' './strmatch.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/split.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/arch.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/config.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/config.cmti' '-I' '.' '-o' './config.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/arch.cmt' '-I' '.' '-o' './arch.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/mach.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/reg.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/reg.cmti' '-I' '.' '-o' './reg.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/mach.cmti' '-I' '.' '-o' './mach.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/split.cmti' '-I' '.' '-o' './split.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/spill.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/spill.cmti' '-I' '.' '-o' './spill.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/simplify_primitives.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/simplify_primitives.cmti' '-I' '.' '-o' './simplify_primitives.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/simplify_common.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/simplify_common.cmti' '-I' '.' '-o' './simplify_common.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/simplify_boxed_integer_ops_intf.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/simplify_boxed_integer_ops_intf.cmti' '-I' '.' '-o' './simplify_boxed_integer_ops_intf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/simplify_boxed_integer_ops.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/simplify_boxed_integer_ops.cmti' '-I' '.' '-o' './simplify_boxed_integer_ops.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/simplif.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/simplif.cmti' '-I' '.' '-o' './simplif.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/signature_group.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/signature_group.cmti' '-I' '.' '-o' './signature_group.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/share_constants.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/share_constants.cmti' '-I' '.' '-o' './share_constants.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/semantics_of_primitives.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/semantics_of_primitives.cmti' '-I' '.' '-o' './semantics_of_primitives.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/selection.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/selection.cmti' '-I' '.' '-o' './selection.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/selectgen.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/selectgen.cmti' '-I' '.' '-o' './selectgen.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/scheduling.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/linear.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/linear.cmti' '-I' '.' '-o' './linear.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/scheduling.cmti' '-I' '.' '-o' './scheduling.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/schedgen.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/schedgen.cmti' '-I' '.' '-o' './schedgen.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/runtimedef.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/runtimedef.cmti' '-I' '.' '-o' './runtimedef.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/remove_unused_program_constructs.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/remove_unused_program_constructs.cmti' '-I' '.' '-o' './remove_unused_program_constructs.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/remove_unused_closure_vars.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/remove_unused_closure_vars.cmti' '-I' '.' '-o' './remove_unused_closure_vars.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/remove_unused_arguments.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/remove_unused_arguments.cmti' '-I' '.' '-o' './remove_unused_arguments.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/remove_free_vars_equal_to_args.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/remove_free_vars_equal_to_args.cmti' '-I' '.' '-o' './remove_free_vars_equal_to_args.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/reloadgen.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/reloadgen.cmti' '-I' '.' '-o' './reloadgen.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/reload.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/reload.cmti' '-I' '.' '-o' './reload.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ref_to_variables.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ref_to_variables.cmti' '-I' '.' '-o' './ref_to_variables.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/rec_check.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/rec_check.cmti' '-I' '.' '-o' './rec_check.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/proc.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/proc.cmti' '-I' '.' '-o' './proc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printtyped.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printtyped.cmti' '-I' '.' '-o' './printtyped.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printtyp.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printtyp.cmti' '-I' '.' '-o' './printtyp.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printpat.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printpat.cmti' '-I' '.' '-o' './printpat.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printmach.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printmach.cmti' '-I' '.' '-o' './printmach.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printlinear.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printlinear.cmti' '-I' '.' '-o' './printlinear.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printlambda.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printlambda.cmti' '-I' '.' '-o' './printlambda.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printinstr.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/instruct.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/instruct.cmti' '-I' '.' '-o' './instruct.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printinstr.cmti' '-I' '.' '-o' './printinstr.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printcmm.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printcmm.cmti' '-I' '.' '-o' './printcmm.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printclambda_primitives.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printclambda_primitives.cmti' '-I' '.' '-o' './printclambda_primitives.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printclambda.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printclambda.cmti' '-I' '.' '-o' './printclambda.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/printast.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/printast.cmti' '-I' '.' '-o' './printast.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/predef.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/predef.cmti' '-I' '.' '-o' './predef.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/pprintast.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/pprintast.cmti' '-I' '.' '-o' './pprintast.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/pparse.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/pparse.cmti' '-I' '.' '-o' './pparse.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/polling.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/polling.cmti' '-I' '.' '-o' './polling.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/persistent_env.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/consistbl.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/consistbl.cmti' '-I' '.' '-o' './consistbl.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/lazy_backtrack.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/lazy_backtrack.cmti' '-I' '.' '-o' './lazy_backtrack.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/persistent_env.cmti' '-I' '.' '-o' './persistent_env.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/patterns.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/patterns.cmti' '-I' '.' '-o' './patterns.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/pass_wrapper.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/pass_wrapper.cmti' '-I' '.' '-o' './pass_wrapper.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/parser.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/camlinternalMenhirLib.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/camlinternalMenhirLib.cmti' '-I' '.' '-o' './camlinternalMenhirLib.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/docstrings.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/docstrings.cmti' '-I' '.' '-o' './docstrings.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/parser.cmti' '-I' '.' '-o' './parser.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/parmatch.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/parmatch.cmti' '-I' '.' '-o' './parmatch.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/optmaindriver.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/optmaindriver.cmti' '-I' '.' '-o' './optmaindriver.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/optmain.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/optmain.cmt' '-I' '.' '-o' './optmain.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/opterrors.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/opterrors.cmti' '-I' '.' '-o' './opterrors.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/optcompile.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/compile_common.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/compile_common.cmti' '-I' '.' '-o' './compile_common.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/optcompile.cmti' '-I' '.' '-o' './optcompile.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/oprint.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/oprint.cmti' '-I' '.' '-o' './oprint.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/opcodes.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/opcodes.cmti' '-I' '.' '-o' './opcodes.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/mtype.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/mtype.cmti' '-I' '.' '-o' './mtype.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/meta.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/meta.cmti' '-I' '.' '-o' './meta.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/matching.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/matching.cmti' '-I' '.' '-o' './matching.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/makedepend.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/makedepend.cmti' '-I' '.' '-o' './makedepend.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/maindriver.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/maindriver.cmti' '-I' '.' '-o' './maindriver.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/main_args.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/main_args.cmti' '-I' '.' '-o' './main_args.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/main.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/main.cmt' '-I' '.' '-o' './main.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/local_store.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/local_store.cmti' '-I' '.' '-o' './local_store.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/liveness.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/liveness.cmti' '-I' '.' '-o' './liveness.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/linscan.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/linscan.cmti' '-I' '.' '-o' './linscan.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/linearize.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/linearize.cmti' '-I' '.' '-o' './linearize.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/linear_format.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/linear_format.cmti' '-I' '.' '-o' './linear_format.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/lift_let_to_initialize_symbol.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/lift_let_to_initialize_symbol.cmti' '-I' '.' '-o' './lift_let_to_initialize_symbol.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/lift_constants.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/lift_constants.cmti' '-I' '.' '-o' './lift_constants.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/lift_code.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/lift_code.cmti' '-I' '.' '-o' './lift_code.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/lexer.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/lexer.cmti' '-I' '.' '-o' './lexer.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/invariant_params.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/invariant_params.cmti' '-I' '.' '-o' './invariant_params.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/interval.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/interval.cmti' '-I' '.' '-o' './interval.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/interf.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/interf.cmti' '-I' '.' '-o' './interf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/int_replace_polymorphic_compare.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/int_replace_polymorphic_compare.cmti' '-I' '.' '-o' './int_replace_polymorphic_compare.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inlining_transforms.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inlining_decision_intf.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inlining_decision_intf.cmti' '-I' '.' '-o' './inlining_decision_intf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inlining_transforms.cmti' '-I' '.' '-o' './inlining_transforms.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inlining_stats.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inlining_stats.cmti' '-I' '.' '-o' './inlining_stats.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inlining_decision.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inlining_decision.cmti' '-I' '.' '-o' './inlining_decision.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inline_and_simplify.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inline_and_simplify.cmti' '-I' '.' '-o' './inline_and_simplify.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/initialize_symbol_to_let_symbol.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/initialize_symbol_to_let_symbol.cmti' '-I' '.' '-o' './initialize_symbol_to_let_symbol.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/inconstant_idents.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/inconstant_idents.cmti' '-I' '.' '-o' './inconstant_idents.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/includemod_errorprinter.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/includemod_errorprinter.cmti' '-I' '.' '-o' './includemod_errorprinter.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/includeclass.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/includeclass.cmti' '-I' '.' '-o' './includeclass.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/import_approx.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/import_approx.cmti' '-I' '.' '-o' './import_approx.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/id_types.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/id_types.cmti' '-I' '.' '-o' './id_types.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/flambda_utils.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/flambda_utils.cmti' '-I' '.' '-o' './flambda_utils.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/flambda_to_clambda.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/flambda_to_clambda.cmti' '-I' '.' '-o' './flambda_to_clambda.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/flambda_middle_end.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/flambda_middle_end.cmti' '-I' '.' '-o' './flambda_middle_end.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/flambda_iterators.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/flambda_iterators.cmti' '-I' '.' '-o' './flambda_iterators.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/flambda_invariants.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/flambda_invariants.cmti' '-I' '.' '-o' './flambda_invariants.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/find_recursive_functions.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/find_recursive_functions.cmti' '-I' '.' '-o' './find_recursive_functions.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/extract_projections.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/extract_projections.cmti' '-I' '.' '-o' './extract_projections.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/expunge.cmt'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/bytesections.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/bytesections.cmti' '-I' '.' '-o' './bytesections.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/expunge.cmt' '-I' '.' '-o' './expunge.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/export_info_for_pack.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/export_info_for_pack.cmti' '-I' '.' '-o' './export_info_for_pack.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/errors.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/errors.cmti' '-I' '.' '-o' './errors.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/envaux.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/envaux.cmti' '-I' '.' '-o' './envaux.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/emitenv.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/emitenv.cmti' '-I' '.' '-o' './emitenv.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/emitcode.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/emitcode.cmti' '-I' '.' '-o' './emitcode.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/emitaux.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/emitaux.cmti' '-I' '.' '-o' './emitaux.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/emit.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/emit.cmti' '-I' '.' '-o' './emit.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/effect_analysis.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/effect_analysis.cmti' '-I' '.' '-o' './effect_analysis.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/domainstate.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/domainstate.cmti' '-I' '.' '-o' './domainstate.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/dll.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/dll.cmti' '-I' '.' '-o' './dll.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/depend.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/depend.cmti' '-I' '.' '-o' './depend.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/deadcode.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/deadcode.cmti' '-I' '.' '-o' './deadcode.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/datarepr.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/datarepr.cmti' '-I' '.' '-o' './datarepr.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/dataflow.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/dataflow.cmti' '-I' '.' '-o' './dataflow.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/convert_primitives.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/convert_primitives.cmti' '-I' '.' '-o' './convert_primitives.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/config_main.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/config_main.cmti' '-I' '.' '-o' './config_main.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/config_boot.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/config_boot.cmti' '-I' '.' '-o' './config_boot.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/compilenv.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmx_format.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmx_format.cmti' '-I' '.' '-o' './cmx_format.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/compilenv.cmti' '-I' '.' '-o' './compilenv.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/compile.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/compile.cmti' '-I' '.' '-o' './compile.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/compenv.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/compenv.cmti' '-I' '.' '-o' './compenv.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/comballoc.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/comballoc.cmti' '-I' '.' '-o' './comballoc.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/coloring.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/coloring.cmti' '-I' '.' '-o' './coloring.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmxs_format.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmxs_format.cmti' '-I' '.' '-o' './cmxs_format.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmt2annot.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmt2annot.cmt' '-I' '.' '-o' './cmt2annot.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmmgen_state.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmmgen_state.cmti' '-I' '.' '-o' './cmmgen_state.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmmgen.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmmgen.cmti' '-I' '.' '-o' './cmmgen.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmm_invariants.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmm_invariants.cmti' '-I' '.' '-o' './cmm_invariants.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/cmm_helpers.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/cmm_helpers.cmti' '-I' '.' '-o' './cmm_helpers.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure_offsets.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure_offsets.cmti' '-I' '.' '-o' './closure_offsets.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure_middle_end.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure_middle_end.cmti' '-I' '.' '-o' './closure_middle_end.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure_conversion_aux.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure_conversion_aux.cmti' '-I' '.' '-o' './closure_conversion_aux.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure_conversion.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure_conversion.cmti' '-I' '.' '-o' './closure_conversion.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/closure.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/closure.cmti' '-I' '.' '-o' './closure.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ccomp.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ccomp.cmti' '-I' '.' '-o' './ccomp.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/bytepackager.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/bytepackager.cmti' '-I' '.' '-o' './bytepackager.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/bytelink.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/bytelink.cmti' '-I' '.' '-o' './bytelink.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/bytelibrarian.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/bytelibrarian.cmti' '-I' '.' '-o' './bytelibrarian.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/bytegen.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/bytegen.cmti' '-I' '.' '-o' './bytegen.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/builtin_attributes.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/builtin_attributes.cmti' '-I' '.' '-o' './builtin_attributes.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/build_export_info.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/build_export_info.cmti' '-I' '.' '-o' './build_export_info.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/branch_relaxation_intf.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/branch_relaxation_intf.cmt' '-I' '.' '-o' './branch_relaxation_intf.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/branch_relaxation.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/branch_relaxation.cmti' '-I' '.' '-o' './branch_relaxation.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/binutils.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/binutils.cmti' '-I' '.' '-o' './binutils.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/augment_specialised_args.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/augment_specialised_args.cmti' '-I' '.' '-o' './augment_specialised_args.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/attr_helper.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/attr_helper.cmti' '-I' '.' '-o' './attr_helper.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ast_mapper.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ast_mapper.cmti' '-I' '.' '-o' './ast_mapper.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ast_iterator.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ast_iterator.cmti' '-I' '.' '-o' './ast_iterator.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ast_invariants.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ast_invariants.cmti' '-I' '.' '-o' './ast_invariants.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/ast_helper.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/ast_helper.cmti' '-I' '.' '-o' './ast_helper.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/asmpackager.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/asmpackager.cmti' '-I' '.' '-o' './asmpackager.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/asmlink.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/asmlink.cmti' '-I' '.' '-o' './asmlink.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/asmlibrarian.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/asmlibrarian.cmti' '-I' '.' '-o' './asmlibrarian.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/asmgen.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/asmgen.cmti' '-I' '.' '-o' './asmgen.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/arg_helper.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/arg_helper.cmti' '-I' '.' '-o' './arg_helper.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/alias_analysis.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/alias_analysis.cmti' '-I' '.' '-o' './alias_analysis.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/afl_instrument.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/afl_instrument.cmti' '-I' '.' '-o' './afl_instrument.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/CSEgen.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/CSEgen.cmti' '-I' '.' '-o' './CSEgen.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/compiler-libs/CSE.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/compiler-libs/CSE.cmt' '-I' '.' '-o' './CSE.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/camlinternalMod.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/camlinternalMod.cmti' '-I' '.' '-o' './camlinternalMod.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/ocaml/camlinternalFormat.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/ocaml/camlinternalFormat.cmti' '-I' '.' '-o' './camlinternalFormat.odoc' '--parent' 'page-"stdlib"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/yojson/yojson_biniou.cmti'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_inbuf.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_inbuf.cmti' '-I' '.' '-o' './bi_inbuf.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_io.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_io.cmti' '-I' '.' '-o' './bi_io.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/yojson/yojson_biniou.cmti' '-I' '.' '-o' './yojson_biniou.odoc' '--parent' 'page-"yojson"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_vint.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_vint.cmti' '-I' '.' '-o' './bi_vint.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_util.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_util.cmti' '-I' '.' '-o' './bi_util.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_stream.cmti'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_stream.cmti' '-I' '.' '-o' './bi_stream.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'compile-deps' '../../../_opam/lib/biniou/bi_dump.cmt'
$ '../src/odoc/bin/main.exe' 'compile' '../../../_opam/lib/biniou/bi_dump.cmt' '-I' '.' '-o' './bi_dump.odoc' '--parent' 'page-"biniou"'
$ '../src/odoc/bin/main.exe' 'link' 'src-source.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref_test__Common.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'compmisc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'clflags.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Cmt.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Cmti.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Ident_env.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Compat.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Resolver.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Fs.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Or_error.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Source_info.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Lookup_def.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'fpath.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmt_format.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Compile.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Tools.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Find.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Errors.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Lookup_failures.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Env.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Component.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Cfrag.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Cpath.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Ident.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Lang.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Root.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Error.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Comment.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Location_.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Paths.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Paths_types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Names.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__Ast.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__Warning.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__Loc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'astring.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref_test.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'parse.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'profile.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'result.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Arg.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Array.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__List.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Printf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Result.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'toploop.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Obj.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Int32.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typemod.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typedecl.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typedecl_variance.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typedecl_separability.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typedecl_immediacy.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typedecl_properties.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'includemod.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'includecore.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typedtree.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'diffing_with_keys.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'diffing.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ctype.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'errortrace.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'env.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'subst.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'load_path.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmi_format.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'misc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__String.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Digest.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'build_path_prefix_map.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'btype.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'shape.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'primitive.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'path.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'outcometree.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'type_immediacy.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'parsetree.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'longident.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ident.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'identifiable.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Set.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Map.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Hashtbl.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'asttypes.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'location.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Format.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Domain.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Buffer.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Uchar.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Seq.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Either.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'warnings.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Sys.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Lexing.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Lazy.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'camlinternalLazy.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib.odoc' '-I' '.' '--open=""'
$ '../src/odoc/bin/main.exe' 'link' 'camlinternalFormatBasics.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Utils.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Type_of.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Subst.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Strengthen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Ref_tools.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Link.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Lang_of.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_xref2__Expand_tools.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Url.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Link.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_latex__Generator.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_latex.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Semantics.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Html_page.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Config.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Renderer.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Reason.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__ML.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Url.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tyxml.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tyxml_html.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'html_sigs.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'html_types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tyxml_svg.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'svg_sigs.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'svg_types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tyxml_xml.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'xml_stream.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'xml_sigs.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'xml_wrap.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Support_files.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Source_tree.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Rendering.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Odoc_link.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Odoc_file.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Man_page.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_manpage__Generator.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_manpage.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Latex.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Html_fragment.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Depends.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_odoc__Compile.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html_support_files.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model_desc__Type_desc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model_desc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model_desc__Paths_desc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model_desc__Lang_desc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model_desc__Comment_desc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model_semantics_test__Test.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model_semantics_test.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__BytesLabels.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Int64.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__ListLabels.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__MoreLabels.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Nativeint.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Printexc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__StdLabels.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__StringLabels.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'yojson.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_outbuf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_share.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Reference.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_model__Predefined.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_manpage__Link.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Uid.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Local_jmp.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Doc_attr.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_loader__Cmi.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_latex__Types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_latex__Raw.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'fmt.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Stack.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Queue.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Utils.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Char.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Html_source.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Html_page.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Html_fragment_json.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_html__Generator.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Utils.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Targets.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Generator_signatures.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Codefmt.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Generator.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Doctree.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Compat.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_document__Comment.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_examples__Wrapping.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_examples__Unexposed.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_examples__.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_examples__Resolution.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_examples__Markup.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_examples__Expansion.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_examples.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__Token.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__Syntax.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__Parse_error.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'odoc_parser__Lexer.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmdliner.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'xml_print.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'xml_iter.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'svg_f.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'html_f.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'fmt_tty.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'fmt_cli.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'unixLabels.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'unix.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Bigarray.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Complex.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'thread.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'event.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'str.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Weak.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Unit.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Semaphore.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Scanf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Random.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Parsing.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Out_channel.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Option.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Oo.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'camlinternalOO.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Mutex.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Marshal.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Int.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__In_channel.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Gc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Fun.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Float.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Filename.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Ephemeron.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Effect.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Condition.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Callback.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Bytes.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Bool.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__Atomic.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stdlib__ArrayLabels.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'std_exit.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'runtime_events.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'profiling.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ocamlmktop_init.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'topcommon.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'genprintval.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'dynlink.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'x86_proc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'x86_ast.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'x86_masm.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'x86_gas.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'x86_dsl.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'variable.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'internal_variable_names.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'lambda.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'debuginfo.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'compilation_unit.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'linkage_name.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'var_within_closure.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure_element.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'untypeast.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'unbox_specialised_args.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inline_and_simplify_aux.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inlining_stats_types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inlining_cost.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'backend_intf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'simple_value_approx.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'freshening.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'flambda.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tag.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'symbol.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'static_exception.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'set_of_closures_origin.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'set_of_closures_id.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'projection.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'parameter.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'numbers.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'mutable_variable.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'export_id.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure_origin.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure_id.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'clambda_primitives.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'allocated_const.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'unbox_free_vars_of_closures.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'unbox_closures.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'un_anf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'clambda.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'backend_var.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typetexp.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typeopt.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typedecl_unboxed.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typecore.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'typeclass.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'traverse_for_exported_symbols.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'export_info.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'translprim.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'translobj.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'translmod.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'translcore.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'translclass.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'translattribute.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'trace.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'topstart.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'topmain.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tophooks.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'topeval.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'topdirs.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tmc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'terminfo.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tast_mapper.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'tast_iterator.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'targetint.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'syntaxerr.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'symtable.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmo_format.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'switch.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'stypes.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'annot.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'strongly_connected_components.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'strmatch.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmm.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'split.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'mach.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'reg.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'arch.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'config.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'spill.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'simplify_primitives.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'simplify_common.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'simplify_boxed_integer_ops_intf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'simplify_boxed_integer_ops.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'simplif.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'signature_group.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'share_constants.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'semantics_of_primitives.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'selection.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'selectgen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'scheduling.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'linear.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'schedgen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'runtimedef.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'remove_unused_program_constructs.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'remove_unused_closure_vars.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'remove_unused_arguments.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'remove_free_vars_equal_to_args.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'reloadgen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'reload.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ref_to_variables.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'rec_check.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'proc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printtyped.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printtyp.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printpat.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printmach.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printlinear.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printlambda.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printinstr.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'instruct.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printcmm.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printclambda_primitives.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printclambda.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'printast.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'predef.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'pprintast.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'pparse.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'polling.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'persistent_env.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'lazy_backtrack.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'consistbl.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'patterns.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'pass_wrapper.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'parser.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'docstrings.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'camlinternalMenhirLib.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'parmatch.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'optmaindriver.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'optmain.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'opterrors.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'optcompile.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'compile_common.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'oprint.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'opcodes.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'mtype.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'meta.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'matching.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'makedepend.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'maindriver.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'main_args.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'main.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'local_store.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'liveness.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'linscan.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'linearize.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'linear_format.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'lift_let_to_initialize_symbol.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'lift_constants.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'lift_code.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'lexer.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'invariant_params.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'interval.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'interf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'int_replace_polymorphic_compare.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inlining_transforms.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inlining_decision_intf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inlining_stats.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inlining_decision.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inline_and_simplify.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'initialize_symbol_to_let_symbol.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'inconstant_idents.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'includemod_errorprinter.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'includeclass.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'import_approx.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'id_types.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'flambda_utils.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'flambda_to_clambda.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'flambda_middle_end.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'flambda_iterators.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'flambda_invariants.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'find_recursive_functions.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'extract_projections.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'expunge.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bytesections.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'export_info_for_pack.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'errors.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'envaux.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'emitenv.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'emitcode.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'emitaux.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'emit.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'effect_analysis.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'domainstate.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'dll.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'depend.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'deadcode.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'datarepr.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'dataflow.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'convert_primitives.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'config_main.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'config_boot.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'compilenv.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmx_format.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'compile.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'compenv.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'comballoc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'coloring.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmxs_format.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmt2annot.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmmgen_state.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmmgen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmm_invariants.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'cmm_helpers.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure_offsets.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure_middle_end.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure_conversion_aux.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure_conversion.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'closure.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ccomp.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bytepackager.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bytelink.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bytelibrarian.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bytegen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'builtin_attributes.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'build_export_info.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'branch_relaxation_intf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'branch_relaxation.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'binutils.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'augment_specialised_args.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'attr_helper.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ast_mapper.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ast_iterator.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ast_invariants.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'ast_helper.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'asmpackager.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'asmlink.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'asmlibrarian.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'asmgen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'arg_helper.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'alias_analysis.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'afl_instrument.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'CSEgen.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'CSE.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'camlinternalMod.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'camlinternalFormat.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'yojson_biniou.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_io.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_inbuf.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_vint.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_util.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_stream.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'bi_dump.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-deps.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc-parser.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-astring.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-cmdliner.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-fpath.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-result.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-tyxml.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-fmt.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-stdlib.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-yojson.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-biniou.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_xref_test.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_xref2.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_odoc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_html_support_files.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_model_desc.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_model.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_manpage.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_loader.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_latex.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_html.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_document.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_examples.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-interface.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-contributing.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-driver.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-parent_child_spec.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-features.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-interface.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-odoc_for_authors.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-dune.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'link' 'page-ocamldoc_differences.odoc' '-I' '.'
$ '../src/odoc/bin/main.exe' 'html-generate' 'src-source.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_xref_test__Common.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'compmisc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'clflags.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/cmt.pp.ml' 'odoc_loader__Cmt.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/cmti.pp.ml' 'odoc_loader__Cmti.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/ident_env.pp.ml' 'odoc_loader__Ident_env.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/compat.ml' 'odoc_model__Compat.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/resolver.ml' 'odoc_odoc__Resolver.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/fs.ml' 'odoc_odoc__Fs.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/or_error.ml' 'odoc_odoc__Or_error.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_odoc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/odoc_loader.pp.ml' 'odoc_loader.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/source_info.pp.ml' 'odoc_loader__Source_info.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/lookup_def.pp.ml' 'odoc_loader__Lookup_def.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_loader__.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'fpath.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmt_format.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/compile.ml' 'odoc_xref2__Compile.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/tools.ml' 'odoc_xref2__Tools.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/find.ml' 'odoc_xref2__Find.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/errors.ml' 'odoc_xref2__Errors.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/lookup_failures.ml' 'odoc_xref2__Lookup_failures.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/env.ml' 'odoc_xref2__Env.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/component.ml' 'odoc_xref2__Component.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/cfrag.ml' 'odoc_xref2__Cfrag.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/cpath.ml' 'odoc_xref2__Cpath.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/ident.ml' 'odoc_xref2__Ident.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_xref2.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/lang.ml' 'odoc_model__Lang.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/root.ml' 'odoc_model__Root.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/error.ml' 'odoc_model__Error.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/comment.ml' 'odoc_model__Comment.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/location_.ml' 'odoc_model__Location_.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/paths.ml' 'odoc_model__Paths.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/paths_types.ml' 'odoc_model__Paths_types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/names.ml' 'odoc_model__Names.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__Ast.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__Warning.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__Loc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/odoc_model.ml' 'odoc_model.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_model__.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'astring.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_xref_test.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'parse.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'profile.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'result.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Arg.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Array.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__List.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Printf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Result.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'toploop.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Obj.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Int32.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typemod.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typedecl.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typedecl_variance.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typedecl_separability.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typedecl_immediacy.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typedecl_properties.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'includemod.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'includecore.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typedtree.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'diffing_with_keys.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'diffing.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ctype.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'errortrace.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'env.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'subst.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'load_path.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmi_format.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'misc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__String.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Digest.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'build_path_prefix_map.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'btype.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'shape.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'primitive.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'path.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'outcometree.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'type_immediacy.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'parsetree.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'longident.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ident.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'identifiable.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Set.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Map.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Hashtbl.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'asttypes.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'location.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Format.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Domain.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Buffer.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Uchar.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Seq.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Either.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'warnings.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Sys.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Lexing.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Lazy.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'camlinternalLazy.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'camlinternalFormatBasics.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/utils.ml' 'odoc_xref2__Utils.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/type_of.ml' 'odoc_xref2__Type_of.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/subst.ml' 'odoc_xref2__Subst.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/strengthen.ml' 'odoc_xref2__Strengthen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/ref_tools.ml' 'odoc_xref2__Ref_tools.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/link.ml' 'odoc_xref2__Link.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/lang_of.ml' 'odoc_xref2__Lang_of.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/xref2/expand_tools.ml' 'odoc_xref2__Expand_tools.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/url.ml' 'odoc_odoc__Url.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/link.ml' 'odoc_html__Link.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/latex/generator.ml' 'odoc_latex__Generator.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_latex.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/semantics.ml' 'odoc_model__Semantics.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/html_page.ml' 'odoc_odoc__Html_page.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/config.ml' 'odoc_html__Config.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/types.ml' 'odoc_html__Types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/odoc_html.ml' 'odoc_html.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_html__.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/renderer.ml' 'odoc_document__Renderer.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/reason.ml' 'odoc_document__Reason.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/ML.ml' 'odoc_document__ML.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/types.ml' 'odoc_document__Types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/url.ml' 'odoc_document__Url.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_document.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tyxml.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tyxml_html.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'html_sigs.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'html_types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tyxml_svg.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'svg_sigs.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'svg_types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tyxml_xml.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'xml_stream.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'xml_sigs.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'xml_wrap.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/support_files.ml' 'odoc_odoc__Support_files.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/source_tree.ml' 'odoc_odoc__Source_tree.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/rendering.ml' 'odoc_odoc__Rendering.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/odoc_link.ml' 'odoc_odoc__Odoc_link.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/odoc_file.ml' 'odoc_odoc__Odoc_file.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/man_page.ml' 'odoc_odoc__Man_page.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/manpage/generator.ml' 'odoc_manpage__Generator.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_manpage.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/latex.ml' 'odoc_odoc__Latex.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/html_fragment.ml' 'odoc_odoc__Html_fragment.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/depends.ml' 'odoc_odoc__Depends.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/odoc/compile.ml' 'odoc_odoc__Compile.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html_support_files/odoc_html_support_files.ml' 'odoc_html_support_files.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model_desc/type_desc.ml' 'odoc_model_desc__Type_desc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_model_desc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model_desc/paths_desc.ml' 'odoc_model_desc__Paths_desc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model_desc/lang_desc.ml' 'odoc_model_desc__Lang_desc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model_desc/comment_desc.ml' 'odoc_model_desc__Comment_desc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_model_semantics_test__Test.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_model_semantics_test.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__BytesLabels.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Int64.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__ListLabels.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__MoreLabels.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Nativeint.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Printexc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__StdLabels.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__StringLabels.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'yojson.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_outbuf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_share.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/reference.ml' 'odoc_model__Reference.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/model/predefined.ml' 'odoc_model__Predefined.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/manpage/link.ml' 'odoc_manpage__Link.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/uid.pp.ml' 'odoc_loader__Uid.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/local_jmp.pp.ml' 'odoc_loader__Local_jmp.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/doc_attr.pp.ml' 'odoc_loader__Doc_attr.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/loader/cmi.pp.ml' 'odoc_loader__Cmi.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/latex/types.ml' 'odoc_latex__Types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/latex/raw.ml' 'odoc_latex__Raw.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'fmt.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Stack.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Queue.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/utils.ml' 'odoc_html__Utils.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Char.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/html_source.ml' 'odoc_html__Html_source.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/html_page.ml' 'odoc_html__Html_page.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/html_fragment_json.ml' 'odoc_html__Html_fragment_json.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/html/generator.ml' 'odoc_html__Generator.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/utils.ml' 'odoc_document__Utils.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/targets.ml' 'odoc_document__Targets.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/generator_signatures.ml' 'odoc_document__Generator_signatures.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/codefmt.ml' 'odoc_document__Codefmt.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/generator.ml' 'odoc_document__Generator.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/doctree.ml' 'odoc_document__Doctree.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/compat.ml' 'odoc_document__Compat.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' '--source' '../src/document/comment.ml' 'odoc_document__Comment.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples__Wrapping.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples__Unexposed.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples__.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples__Resolution.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples__Markup.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples__Expansion.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__Token.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__Syntax.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__Parse_error.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'odoc_parser__Lexer.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmdliner.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'xml_print.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'xml_iter.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'svg_f.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'html_f.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'fmt_tty.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'fmt_cli.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'unixLabels.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'unix.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Bigarray.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Complex.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'thread.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'event.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'str.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Weak.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Unit.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Semaphore.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Scanf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Random.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Parsing.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Out_channel.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Option.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Oo.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'camlinternalOO.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Mutex.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Marshal.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Int.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__In_channel.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Gc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Fun.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Float.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Filename.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Ephemeron.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Effect.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Condition.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Callback.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Bytes.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Bool.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__Atomic.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stdlib__ArrayLabels.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'std_exit.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'runtime_events.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'profiling.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ocamlmktop_init.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'topcommon.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'genprintval.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'dynlink.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'x86_proc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'x86_ast.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'x86_masm.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'x86_gas.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'x86_dsl.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'variable.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'internal_variable_names.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'lambda.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'debuginfo.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'compilation_unit.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'linkage_name.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'var_within_closure.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure_element.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'untypeast.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'unbox_specialised_args.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inline_and_simplify_aux.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inlining_stats_types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inlining_cost.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'backend_intf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'simple_value_approx.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'freshening.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'flambda.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tag.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'symbol.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'static_exception.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'set_of_closures_origin.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'set_of_closures_id.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'projection.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'parameter.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'numbers.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'mutable_variable.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'export_id.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure_origin.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure_id.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'clambda_primitives.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'allocated_const.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'unbox_free_vars_of_closures.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'unbox_closures.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'un_anf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'clambda.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'backend_var.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typetexp.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typeopt.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typedecl_unboxed.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typecore.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'typeclass.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'traverse_for_exported_symbols.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'export_info.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'translprim.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'translobj.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'translmod.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'translcore.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'translclass.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'translattribute.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'trace.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'topstart.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'topmain.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tophooks.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'topeval.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'topdirs.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tmc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'terminfo.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tast_mapper.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'tast_iterator.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'targetint.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'syntaxerr.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'symtable.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmo_format.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'switch.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'stypes.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'annot.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'strongly_connected_components.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'strmatch.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmm.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'split.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'mach.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'reg.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'arch.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'config.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'spill.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'simplify_primitives.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'simplify_common.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'simplify_boxed_integer_ops_intf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'simplify_boxed_integer_ops.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'simplif.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'signature_group.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'share_constants.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'semantics_of_primitives.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'selection.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'selectgen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'scheduling.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'linear.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'schedgen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'runtimedef.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'remove_unused_program_constructs.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'remove_unused_closure_vars.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'remove_unused_arguments.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'remove_free_vars_equal_to_args.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'reloadgen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'reload.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ref_to_variables.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'rec_check.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'proc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printtyped.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printtyp.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printpat.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printmach.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printlinear.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printlambda.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printinstr.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'instruct.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printcmm.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printclambda_primitives.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printclambda.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'printast.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'predef.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'pprintast.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'pparse.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'polling.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'persistent_env.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'lazy_backtrack.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'consistbl.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'patterns.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'pass_wrapper.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'parser.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'docstrings.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'camlinternalMenhirLib.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'parmatch.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'optmaindriver.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'optmain.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'opterrors.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'optcompile.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'compile_common.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'oprint.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'opcodes.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'mtype.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'meta.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'matching.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'makedepend.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'maindriver.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'main_args.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'main.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'local_store.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'liveness.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'linscan.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'linearize.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'linear_format.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'lift_let_to_initialize_symbol.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'lift_constants.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'lift_code.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'lexer.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'invariant_params.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'interval.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'interf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'int_replace_polymorphic_compare.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inlining_transforms.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inlining_decision_intf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inlining_stats.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inlining_decision.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inline_and_simplify.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'initialize_symbol_to_let_symbol.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'inconstant_idents.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'includemod_errorprinter.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'includeclass.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'import_approx.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'id_types.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'flambda_utils.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'flambda_to_clambda.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'flambda_middle_end.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'flambda_iterators.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'flambda_invariants.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'find_recursive_functions.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'extract_projections.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'expunge.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bytesections.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'export_info_for_pack.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'errors.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'envaux.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'emitenv.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'emitcode.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'emitaux.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'emit.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'effect_analysis.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'domainstate.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'dll.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'depend.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'deadcode.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'datarepr.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'dataflow.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'convert_primitives.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'config_main.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'config_boot.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'compilenv.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmx_format.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'compile.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'compenv.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'comballoc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'coloring.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmxs_format.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmt2annot.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmmgen_state.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmmgen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmm_invariants.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'cmm_helpers.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure_offsets.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure_middle_end.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure_conversion_aux.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure_conversion.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'closure.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ccomp.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bytepackager.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bytelink.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bytelibrarian.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bytegen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'builtin_attributes.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'build_export_info.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'branch_relaxation_intf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'branch_relaxation.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'binutils.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'augment_specialised_args.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'attr_helper.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ast_mapper.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ast_iterator.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ast_invariants.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'ast_helper.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'asmpackager.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'asmlink.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'asmlibrarian.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'asmgen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'arg_helper.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'alias_analysis.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'afl_instrument.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'CSEgen.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'CSE.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'camlinternalMod.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'camlinternalFormat.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'yojson_biniou.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_io.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_inbuf.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_vint.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_util.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_stream.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'bi_dump.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-deps.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc-parser.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-astring.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-cmdliner.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-fpath.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-result.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-tyxml.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-fmt.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-stdlib.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-yojson.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-biniou.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_xref_test.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_xref2.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_odoc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_html_support_files.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_model_desc.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_model.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_manpage.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_loader.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_latex.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_html.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_document.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_examples.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-interface.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-contributing.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-driver.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-parent_child_spec.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-features.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-interface.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-odoc_for_authors.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-dune.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'html-generate' 'page-ocamldoc_differences.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'
$ '../src/odoc/bin/main.exe' 'support-files' '-o' 'html/odoc'
- : unit = ()
```
