The driver in opam mode, on three packages installed in a switch. [opam] is
a stand-in that answers the commands the driver runs, for a switch in
[sw]. The driver reads the rest from the switch: the opam file of each
package, and the record of the files each one installed.

[pc] has a library [pc] with a module [C], and a page whose reference
[{!A.t}] points at [pa]. [pa] has a library [pa] with a module [A], which
uses [pc]. [pb] has a library [pb] with a module [B], which uses [pa]. So
[pa] depends on [pc], and [pb] on [pa]. [pc]'s documentation names [pa], in
its [odoc-config.sexp] and in its opam file's [x-extra-doc-deps], as eio's
names eio_main.

  $ cat src/pc/opam
  opam-version: "2.0"
  depends: [ "ocaml" "conf-thing" {os = "linux" & with-dev-setup} ]
  x-extra-doc-deps: [ "pa" ]
  $ cat src/pc/odoc-config.sexp
  (packages pa)
  $ cat src/pc/index.mld
  {0 pc}
  
  [pc] is used by {!A.t}.

  $ (cd src/pc && ocamlc -bin-annot -c c.mli c.ml && ocamlc -a -o c.cma c.cmo)
  $ (cd src/pa && ocamlc -bin-annot -I ../pc -c a.mli a.ml && ocamlc -a -o a.cma a.cmo)
  $ (cd src/pu && ocamlc -bin-annot -c u.mli u.ml && ocamlc -a -o u.cma u.cmo)
  $ (cd src/pb && ocamlc -bin-annot -I ../pa -I ../pc -c b.mli b.ml && ocamlc -a -o b.cma b.cmo)
  $ install () {
  >   p=$1; l=sw/lib/$p; d=sw/doc/$p
  >   mkdir -p $l $d/odoc-pages sw/.opam-switch/packages/$p.1.0 sw/.opam-switch/install
  >   cp src/$p/META $l/ && cp src/$p/*.cm* src/$p/*.mli $l/ 2>/dev/null
  >   cp src/$p/opam sw/.opam-switch/packages/$p.1.0/opam
  >   [ -f src/$p/index.mld ] && cp src/$p/index.mld $d/odoc-pages/
  >   [ -f src/$p/odoc-config.sexp ] && cp src/$p/odoc-config.sexp $d/
  >   { echo 'opam-version: "2.0"'; echo 'added: ['
  >     (cd sw && find lib/$p doc/$p -type d | sed 's/.*/  "&" {"D"}/'
  >      find lib/$p doc/$p -type f | sed 's/.*/  "&" {"F:S0T0."}/')
  >     echo ']'; } > sw/.opam-switch/install/$p.changes
  > }
  $ for p in pc pa pb pm pu; do install $p; done
The switch is the one opam's environment selects, as [opam env] sets it.

  $ export FAKE_PREFIX=$PWD/sw OPAM_SWITCH_PREFIX=$PWD/sw PATH=$PWD/bin:$PATH
  $ mkdir bin && cp fake-opam bin/opam

There is no findlib configuration in the switch, so the driver sets findlib
up itself, with the switch's [lib] on its path. [OCAMLPATH], which dune sets
to its own install tree, is unset, so that only the switch is seen. The standard library is the
compiler's, which no package of this switch documents. Its directory below
[_odoc] is made empty instead, as an earlier run would have left it.

  $ export OCAMLFIND_CONF=$PWD/no-findlib.conf
  $ unset OCAMLPATH
  $ mkdir -p _odoc$(ocamlc -where) _odoc3$(ocamlc -where)

Case 1. Documenting [pb] should document what it depends on, [pa] and
[pc]. It should give no warning: [{!A.t}] should resolve, and every link should
be given the libraries its units recorded (D6).

A warning, or a package missing from the list, would be wrong.

  $ odoc_driver --odoc-dir _odoc --html-dir _html pb 2>&1 | grep -v '^\(Deciding\|Performing\|Starting\|Documentation\)' || true
  $ ls -d _html/p*
  _html/pa
  _html/pb
  _html/pc
  $ grep -o 'href="[^"]*A[^"]*"' _html/pc/index.html
  href="../pa/pa/A/index.html#type-t"

Case 2 (D1, D2). [B] should record its own library and the closure of what
it requires: [pa], [pb], [pc] and [stdlib].

  $ odoc_print _odoc/lib/pb/b.odoc | jq -c .libraries
  ["pa","pb","pc","stdlib"]

Case 3. Documenting [pc] alone should document [pa] too: [pc]'s opam file
names it in [x-extra-doc-deps], so ocaml-docs-ci installs it for [pc]'s
documentation, after [pc] is compiled and before [pc] is linked. So
[{!A.t}] should resolve here as well.

A warning that [A] is not found, or no [pa] in the list, would be wrong.

  $ odoc_driver --odoc-dir _odoc3 --html-dir _html3 pc 2>&1 | grep -v '^\(Deciding\|Performing\|Starting\|Documentation\)' || true
  $ ls -d _html3/p*
  _html3/pa
  _html3/pc
  $ grep -o 'href="[^"]*A[^"]*"' _html3/pc/index.html || true
  href="../pa/pa/A/index.html#type-t"

Case 4. A package's [odoc-config.sexp] may name a package that its opam
file lists neither as a dependency nor as a doc dep. ocaml-docs-ci does not
install that package for its documentation, and the driver does not wait
for it, so references into it may resolve in one build and not in another.
[pa]'s config names [pz]. Documenting [pa] should warn about it.

No warning would be wrong.

  $ cat src/pa/odoc-config.sexp
  (packages pz)
  $ odoc_driver --odoc-dir _odoc5 --html-dir _html5 pa 2>&1 | grep -i 'pz'
  [1]

Case 5. A library may have no modules, as the compatibility libraries
[seq] and [bytes] have none. [pm] is such a package: it installs a META and
nothing else. Voodoo mode writes a page for the library, which says it has
no modules, and the standard build should too.

No [pm] directory below the package's would be wrong.

  $ mkdir -p _odoc6$(ocamlc -where)
  $ odoc_driver --odoc-dir _odoc6 --html-dir _html6 pm 2>&1 | grep -v '^\(Deciding\|Performing\|Starting\|Documentation\)' || true
  $ ls _html6/pm
  index.html
  pm
  sherlodoc_db.js
  sidebar.json
  status.json

Case 6. A directory may hold an archive that no library of its META
declares, as the compiler's [ocamldoc] directory holds [odoc_info.cma].
Voodoo mode documents its modules, as a library named after the archive,
and the standard build should too. [pu]'s META declares no archive, and its
directory holds [u.cma].

No [u] directory, or [U] missing from it, would be wrong.

  $ mkdir -p _odoc7$(ocamlc -where)
  $ odoc_driver --odoc-dir _odoc7 --html-dir _html7 pu 2>&1 | grep -v '^\(Deciding\|Performing\|Starting\|Documentation\)' || true
  $ ls _html7/pu/u 2>&1
  U
  index.html

Case 7. The switch documented should be the one the environment selects,
as under [opam exec --switch], even when opam's default switch is another
and the driver was built in another. Here opam's default is [other], which
is empty, and [OCAMLFIND_CONF] is unset, so findlib's default configuration
would be that of the switch the driver was built in. Documenting [pc]
should document [pc] and [pa], with [pc]'s library, as in case 3.

An error, a package missing from the list, or no [pc] directory below the
package's would be wrong.

  $ mkdir -p _odoc8$(ocamlc -where)
  $ (unset OCAMLFIND_CONF; FAKE_DEFAULT=other odoc_driver --odoc-dir _odoc8 --html-dir _html8 pc 2>&1) | grep -v '^\(Deciding\|Performing\|Starting\|Documentation\)' || true
  odoc_driver: [ERROR] Error: Unknown/uninstalled packages: ["pc"]
  $ ls -d _html8/p*
  ls: _html8/p*: No such file or directory
  [1]
  $ ls _html8/pc/pc
  ls: _html8/pc/pc: No such file or directory
  [1]

Case 8. A switch may keep findlib's configuration in [etc] rather than
[lib], and findlib looks in [etc] first. The driver should read it, as in
case 7, and not set findlib up by hand, nor read the configuration of the
switch it was built in.

A line saying findlib is set up by hand, or no [pc] directory below the
package's, would be wrong.

  $ mkdir -p sw/etc _odoc9$(ocamlc -where)
  $ printf 'path="%s:%s"\ndestdir="%s"\n' $PWD/sw/lib $(ocamlc -where) $PWD/sw/lib > sw/etc/findlib.conf
  $ (unset OCAMLFIND_CONF; odoc_driver -v --odoc-dir _odoc9 --html-dir _html9 pc 2>&1) | grep 'by hand' || true
  $ ls -d _html9/p*
  _html9/pa
  _html9/pc
  $ ls _html9/pc/pc
  ls: _html9/pc/pc: No such file or directory
  [1]
