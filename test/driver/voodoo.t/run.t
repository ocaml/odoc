The driver's side of the library scope (doc/scope_model.mld, section 2). The
spec makes assumptions about the driver; this test runs [odoc_driver_voodoo]
on two small packages and checks them.

[pc] has a library [pc] with a module [C]. It also has a library [pc.alias]
with no archive, which requires [pc], as [threads.posix] requires [threads],
and a library [pc.user] with a module [U], whose [val x : C.t] uses [pc]; its
META says it requires [pc.alias]. [pd] has a library [pd] with no
modules, as the compatibility libraries [seq] and [bytes] have. [pa] has a
library [pa] with a module [A], whose [type t = C.t] uses [pc]; its META says
it requires [pc] and [pd].
[pb] has a library [pb] with a module [B], whose [val w : A.t] uses [pa];
its META says it requires [pa]. [pb] also has a page, [index.mld], with the
references [{!B.w}] and [{!A.t}].

  $ cat src/pb/META
  version = "1.0"
  requires = "pa"
  archive(byte) = "b.cma"
  $ cat src/pb/b.mli
  (** The module [B] of library [pb], which uses [pa]. *)
  
  val w : A.t
  $ cat src/pb/index.mld
  {0 pb}
  
  See {!B.w} and {!A.t}.

The packages are built as opam would install them. The voodoo driver reads
each package from [prep/universes/<universe>/<package>/<version>], as
ocaml.org's builder lays it out. It finds the libraries a package requires
with findlib, so they are also installed in a dune-style install tree, which
[OCAMLPATH] names; the driver takes a library's [.odoc] directory from where
it is installed below that tree, as it would below an opam switch.

  $ (cd src/pc && ocamlc -bin-annot -c c.mli c.ml && ocamlc -a -o c.cma c.cmo)
  $ (cd src/pc/user && ocamlc -bin-annot -I .. -c u.mli u.ml && ocamlc -a -o u.cma u.cmo)
  $ (cd src/pa && ocamlc -bin-annot -I ../pc -c a.mli a.ml && ocamlc -a -o a.cma a.cmo)
  $ (cd src/pe && ocamlc -bin-annot -I ../pc -c e.mli e.ml && ocamlc -a -o e.cma e.cmo)
  $ (cd src/pb && ocamlc -bin-annot -I ../pa -I ../pc -c b.mli b.ml && ocamlc -a -o b.cma b.cmo)
  $ for p in pd pc pa pb pe; do
  >   l=prep/universes/u/$p/1.0/lib/$p
  >   mkdir -p $l && cp -r src/$p/* $l/ && rm -f $l/*.cmo $l/*/*.cmo $l/*.mld
  >   mkdir -p inst/_build/install/default/lib && cp -r $l inst/_build/install/default/lib/
  > done
  $ mkdir -p prep/universes/u/pb/1.0/doc/pb/odoc-pages
  $ cp src/pb/index.mld prep/universes/u/pb/1.0/doc/pb/odoc-pages/
  $ export OCAMLPATH=$PWD/inst/_build/install/default/lib

The driver documents one package per run, dependencies first. The
standard library, which every library is compiled against, would be
documented by an earlier run for the package [ocaml]. Nothing here uses it,
so its directory is made empty instead.

  $ mkdir -p _odoc/lib/ocaml
  $ odoc_driver_voodoo --odoc-dir _odoc pd
  process_meta_file: prep/universes/u/pd/1.0/lib/pd/META
  process_meta_file: prep/universes/u/pd/1.0/lib/pd/META

Case 1 (D6). At the link of a unit, every library the unit recorded should
be given with [-L]. [U] recorded [pc.alias]: it was compiled against it.
[pc.alias] has no modules, and no [.odoc] directory of its own: its
directory is [pc]'s, which is built in the same run. So documenting [pc]
should give no warning that a library is missing.

A "Library pc.alias ... is not given with -L" warning would be wrong.

  $ odoc_driver_voodoo --odoc-dir _odoc pc
  process_meta_file: prep/universes/u/pc/1.0/lib/pc/META
  process_meta_file: prep/universes/u/pc/1.0/lib/pc/META
  $ odoc_driver_voodoo --odoc-dir _odoc pa
  process_meta_file: prep/universes/u/pa/1.0/lib/pa/META
  process_meta_file: prep/universes/u/pa/1.0/lib/pa/META

Case 2 (D6). At the link of a unit or a page, every library of each unit
whose paths are resolved should be given with [-L]. [pb]'s page has
references that reach [B] and [A], so its link should be given the
libraries they recorded, [pc] and [pd] among them, and documenting [pb]
should give no warning that a library is missing. [pd] has no modules, but
[A] still recorded it: it was compiled against it.

A "Library ... is not given with -L" warning would be wrong.

  $ odoc_driver_voodoo --odoc-dir _odoc pb
  process_meta_file: prep/universes/u/pb/1.0/lib/pb/META
  process_meta_file: prep/universes/u/pb/1.0/lib/pb/META
  _odoc/u/u/pb/1.0/doc/page-index.odoc: File "prep/universes/u/pb/1.0/doc/pb/odoc-pages/index.mld", line 3, characters 15-21:
  _odoc/u/u/pb/1.0/doc/page-index.odoc: Warning: Failed to resolve reference unresolvedroot(A).t Couldn't find "A"
  _odoc/u/u/pb/1.0/doc/page-index.odoc: 

Case 3 (D1, D2). A unit records the libraries it was compiled with: its own,
and the closure of what that library requires. So [B] should record [pa], [pb],
[pc] and [pd], and [stdlib].

A list without [pa], [pc] or [pd] would be wrong.

  $ odoc_print _odoc/lib/pb/b.odoc | jq -c .libraries
  odoc_print: PATH argument: no '_odoc/lib/pb/b.odoc' file or directory
  Usage: odoc_print [OPTION]… PATH
  Try 'odoc_print --help' for more information.

Case 4 (D1). [pe]'s META has two [requires]: [pc], and, under the predicate
[-ppx_driver], [pd]. The second is the form a ppx rewriter uses for what a
program that runs it needs, as [ppx_deriving.show] does. The compiler reads
a library's META under [ppx_driver], so the stanza does not apply: [E] was
compiled against [pc] alone. So [E] should record [pc], [pe] and [stdlib].

A list with [pd] and not [pc] would be wrong, and so would a warning that
[C] is not found.

  $ cat src/pe/META
  version = "1.0"
  requires = "pc"
  requires(-ppx_driver) = "pd"
  archive(byte) = "e.cma"
  $ odoc_driver_voodoo --odoc-dir _odoc pe
  process_meta_file: prep/universes/u/pe/1.0/lib/pe/META
  process_meta_file: prep/universes/u/pe/1.0/lib/pe/META
  _odoc/u/u/pe/1.0/doc/pe/e.odoc: File "_odoc/u/u/pe/1.0/doc/pe/e.odoc":
  _odoc/u/u/pe/1.0/doc/pe/e.odoc: Warning: Couldn't find the following modules:
  _odoc/u/u/pe/1.0/doc/pe/e.odoc:   C
  _odoc/u/u/pe/1.0/doc/pe/e.odoc: 
  $ odoc_print _odoc/lib/pe/e.odoc | jq -c .libraries
  odoc_print: PATH argument: no '_odoc/lib/pe/e.odoc' file or directory
  Usage: odoc_print [OPTION]… PATH
  Try 'odoc_print --help' for more information.

Case 5. ocaml-docs-ci documents a package in a switch holding only its
dependencies, and a switch of dune-only packages has no ocamlfind, so no
findlib configuration. The driver should find the same libraries without
one, here those that [OCAMLPATH] names. So a run with no configuration
should give no warning, and [B] should record the same libraries as in
case 3.

A warning, or a list that differs from case 3's, would be wrong.

  $ export OCAMLFIND_CONF=$PWD/no-findlib.conf
  $ mkdir -p _odoc2/lib/ocaml
  $ for p in pd pc pa pb; do odoc_driver_voodoo --odoc-dir _odoc2 $p; done
  process_meta_file: prep/universes/u/pd/1.0/lib/pd/META
  process_meta_file: prep/universes/u/pd/1.0/lib/pd/META
  process_meta_file: prep/universes/u/pc/1.0/lib/pc/META
  process_meta_file: prep/universes/u/pc/1.0/lib/pc/META
  process_meta_file: prep/universes/u/pa/1.0/lib/pa/META
  process_meta_file: prep/universes/u/pa/1.0/lib/pa/META
  process_meta_file: prep/universes/u/pb/1.0/lib/pb/META
  process_meta_file: prep/universes/u/pb/1.0/lib/pb/META
  _odoc2/u/u/pb/1.0/doc/page-index.odoc: File "prep/universes/u/pb/1.0/doc/pb/odoc-pages/index.mld", line 3, characters 15-21:
  _odoc2/u/u/pb/1.0/doc/page-index.odoc: Warning: Failed to resolve reference unresolvedroot(A).t Couldn't find "A"
  _odoc2/u/u/pb/1.0/doc/page-index.odoc: 
  $ odoc_print _odoc2/lib/pb/b.odoc | jq -c .libraries
  odoc_print: PATH argument: no '_odoc2/lib/pb/b.odoc' file or directory
  Usage: odoc_print [OPTION]… PATH
  Try 'odoc_print --help' for more information.

Case 6. A library may be installed in the compiler's library directory, as
the standard library, [unix] and [threads] are: its META says [directory =
"+"]. [ps] is such a package, with a module [S]: its [S] is in [lib/ocaml],
and its META in [lib/ocaml/ps]. Findlib takes [+] to be the standard
library of the compiler, here [CAMLLIB]. That is the installed copy, not
voodoo-prep's. The driver should document the prepared copy, and put [S]'s
[.odoc] file in [lib/ocaml], where a package that depends on [ps] would
look for it.

An [.odoc] file anywhere else, or none, would be wrong.

  $ (cd src/ps && ocamlc -bin-annot -c s.mli s.ml && ocamlc -a -o s.cma s.cmo)
  $ for root in prep/universes/u/ps/1.0 inst/_build/install/default; do
  >   mkdir -p $root/lib/ocaml/ps && cp src/ps/s.cm* src/ps/s.mli $root/lib/ocaml/ && cp src/ps/META $root/lib/ocaml/ps/
  > done
  $ CAMLLIB=$PWD/inst/_build/install/default/lib/ocaml odoc_driver_voodoo --odoc-dir _odoc6 ps
  process_meta_file: prep/universes/u/ps/1.0/lib/ocaml/ps/META
  process_meta_file: prep/universes/u/ps/1.0/lib/ocaml/ps/META
  $ find _odoc6 -name 's.odoc'
  _odoc6/u/u/ps/1.0/doc/s/s.odoc

Case 7. A package's [odoc-config.sexp] may name a package it does not
depend on, as eio's names eio_main, so that its pages can refer to it.
ocaml-docs-ci links the package after that one is compiled. [pg] has only a
page, which refers to [{!E.y}], and its config names [pe], compiled in
case 4. [pe]'s libraries should be in [pg]'s scope, so the reference should
resolve, with no warning.

A warning that [E] is not found would be wrong.

  $ mkdir -p prep/universes/u/pg/1.0/doc/pg/odoc-pages
  $ printf '{0 pg}\n\nSee {!E.y}.\n' > prep/universes/u/pg/1.0/doc/pg/odoc-pages/index.mld
  $ printf '(packages pe)\n' > prep/universes/u/pg/1.0/doc/pg/odoc-config.sexp
  $ odoc_driver_voodoo --odoc-dir _odoc --html-dir _html pg

Case 8. Two libraries may claim one archive, by its name without the
extension: [compiler-libs.toplevel] has [ocamltoplevel.cma], and
[compiler-libs.native-toplevel] has [ocamltoplevel.cmxa]. A module of that
archive belongs to one of them, and every mode should pick the same one,
whatever order it lists the libraries in. [pi]'s META gives [i.cma] to
[pi] and [i.cmxa] to [pi.native]. [I] should be documented in [pi], the
library with the bytecode archive.

[I] in [pi.native] would be wrong.

  $ cat src/pi/META
  version = "1.0"
  archive(byte) = "i.cma"
  package "native" (
    archive(native) = "i.cmxa"
  )
  $ (cd src/pi && ocamlc -bin-annot -c i.mli i.ml && ocamlc -a -o i.cma i.cmo)
  $ mkdir -p prep/universes/u/pi/1.0/lib/pi && cp src/pi/META src/pi/i.cm* src/pi/i.mli prep/universes/u/pi/1.0/lib/pi/
  $ odoc_driver_voodoo --odoc-dir _odoc --html-dir _html pi
  process_meta_file: prep/universes/u/pi/1.0/lib/pi/META
  process_meta_file: prep/universes/u/pi/1.0/lib/pi/META
  odoc_driver_voodoo: [ERROR] Multiple libraries for archive i: pi.native and pi.
  $ ls _html/u/u/pi/1.0/doc | grep '^pi'
  pi

Case 9. A toplevel add-on declares its archive for the toplevel only, as
[num-top] does: [archive(byte,toploop)]. [pt] is such a package. Its
library should be found by its META, and documented as [pt], not named
after its archive.

A library called [t_top] would be wrong.

  $ cat src/pt/META
  version = "1.0"
  archive(byte,toploop) = "t_top.cma"
  $ (cd src/pt && ocamlc -bin-annot -c t.mli t.ml && ocamlc -a -o t_top.cma t.cmo)
  $ mkdir -p prep/universes/u/pt/1.0/lib/pt && cp src/pt/META src/pt/t.cm* src/pt/t_top.cma src/pt/t.mli prep/universes/u/pt/1.0/lib/pt/
  $ odoc_driver_voodoo --odoc-dir _odoc --html-dir _html pt
  process_meta_file: prep/universes/u/pt/1.0/lib/pt/META
  process_meta_file: prep/universes/u/pt/1.0/lib/pt/META
  $ ls _html/u/u/pt/1.0/doc | grep '^\(pt\|t_top\)$'
  t_top
