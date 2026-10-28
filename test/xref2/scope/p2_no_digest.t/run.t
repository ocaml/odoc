Rule P2 (doc/scope_model.mld, section 3.2): a root that its origin records
with no digest means the unit of that name among the libraries the origin
was compiled with, Λc(o). Pure aliases compiled with [-no-alias-deps] give
such roots (section 3.3). Every compile uses dune's [-no-alias-deps], and
[-L] for the unit's own library and its dependencies.

The file [../digests.sh], shared by the scope tests, defines [entry FILE
N], which reads the import table
of [FILE] with [ocamlobjinfo] and prints [digest], [no digest] or [no
entry] for the name [N], and [from FILE N CMI...], which prints each [CMI]
whose own digest is the one [FILE] records for [N]. Digests are never
printed, as they change between compiler versions.

The source files:


  $ . ../digests.sh

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [U/0] is the item [U], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path,
and [root(pkg/dep/Dep)] is the unit [Dep] of the library [dep] in the
package [pkg]. [unresolvedroot(Dep)] is a root that is not resolved.
[alias(T,P)] is the path [P], which is an alias to [T].
[identifier(root(pkg/n/Na),false)] is the path to [Na] given by its
identifier; [false] says it is not hidden.

Case 1 (P2). A pure alias to a unit of a dependency library, in the style of
[module U = Unix]. The library [dep] has the unit [Dep]. The library [w] is
built against [dep]; [w.mli] is [module U = Dep], and should record no
digest for [Dep].

  $ cat dep/dep.mli
  type t
  $ cat w/w.mli
  module U = Dep
  $ ocamlc -bin-annot -no-alias-deps -c dep/dep.mli
  $ ocamlc -bin-annot -no-alias-deps -I dep -c w/w.mli
  $ entry w/w.cmti Dep
  no digest
  $ odoc compile --output-dir h --parent-id pkg/dep -L dep:h/pkg/dep dep/dep.cmti
  $ odoc compile --output-dir h --parent-id pkg/w -L w:h/pkg/w -L dep:h/pkg/dep w/w.cmti

The spec lets odoc resolve this root at compile or leave it for link
(section 7). At link, [U] should mean [dep]'s [Dep].

  $ odoc_print --short --long-paths --show-root-parents h/pkg/w/w.odoc
  module U/0 = unresolvedroot(Dep)
  $ odoc link -L dep:h/pkg/dep -L w:h/pkg/w h/pkg/w/w.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/w/w.odocl
  module U/0 = resolved(root(pkg/dep/Dep))

Case 2 (P2). A chain of aliases across two libraries. The library [m] has [Mc],
with [type t], and [Mb], which is [module B = Mc]. The library [n] is built
against [m]; [Na] is [module A = Mb]. Neither alias records a digest. The
library [o] is built against [n] and [m]; [o.mli] is [val z : Na.A.B.t],
so [o] records a digest for each of the three units.

  $ cat m/mc.mli
  type t
  $ cat m/mb.mli
  module B = Mc
  $ cat n/na.mli
  module A = Mb
  $ cat o/o.mli
  val z : Na.A.B.t
  $ ocamlc -bin-annot -no-alias-deps -c m/mc.mli
  $ ocamlc -bin-annot -no-alias-deps -I m -c m/mb.mli
  $ ocamlc -bin-annot -no-alias-deps -I m -c n/na.mli
  $ ocamlc -bin-annot -no-alias-deps -I n -I m -c o/o.mli
  $ entry n/na.cmti Mb
  no digest
  $ entry m/mb.cmti Mc
  no digest
  $ odoc compile --output-dir h --parent-id pkg/m -L m:h/pkg/m m/mc.cmti
  $ odoc compile --output-dir h --parent-id pkg/m -L m:h/pkg/m m/mb.cmti
  $ odoc compile --output-dir h --parent-id pkg/n -L n:h/pkg/n -L m:h/pkg/m n/na.cmti
  $ odoc compile --output-dir h --parent-id pkg/o -L o:h/pkg/o -L n:h/pkg/n -L m:h/pkg/m o/o.cmti

The root [Na] of [z]'s path has a digest, so P1 gives [n]'s [Na].
Projection then meets the alias [Mb], whose origin is [Na], and the alias
[Mc], whose origin is [Mb] (section 3.2). Each should be resolved by P2 in
the scope of its own origin: [Mb] in Λc(Na), [Mc] in Λc(Mb). So [Na.A]
should mean [m]'s [Mb], and [Na.A.B] [m]'s [Mc].

  $ odoc link -L m:h/pkg/m -L n:h/pkg/n -L o:h/pkg/o h/pkg/o/o.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/o/o.odocl
  val z/0 : resolved(alias(alias(root(pkg/m/Mc),identifier(root(pkg/m/Mb),false).B),identifier(root(pkg/n/Na),false).A.B).t)

Case 3 (P2, and section 3.6). A pure alias to a unit that does not exist. The
compiler gives warning 49 for it; [-w -49] silences it. dune passes it
only for its alias modules, but here it keeps the output short. [g.mli] is [module X = Nope]. It should record no digest
for [Nope].

  $ cat g/g.mli
  module X = Nope
  $ ocamlc -bin-annot -no-alias-deps -w -49 -c g/g.mli
  $ entry g/g.cmti Nope
  no digest
  $ odoc compile --output-dir h --parent-id pkg/g -L g:h/pkg/g g/g.cmti

No unit in Λc(G) is named [Nope], so [X] should be unresolved. This is a
missing root: it should not be reported by default, and should be reported
with [--enable-missing-root-warning].

  $ odoc link -L g:h/pkg/g h/pkg/g/g.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/g/g.odocl
  module X/0 = unresolvedroot(Nope)
  $ odoc link --enable-missing-root-warning -L g:h/pkg/g h/pkg/g/g.odoc
  File "h/pkg/g/g.odoc":
  Warning: Couldn't find the following modules:
    Nope

Case 4 (P2). The example of section 3.3, which breaks D3. The libraries [ext1] and
[ext2] are [(wrapped false)], and each has a unit [Ext], with different
contents. [l] is built against [ext1]; [l.mli] is [module M = Ext]. [c]
depends on [l] and [ext2], so it is built against [l], [ext1] and [ext2],
with [ext2] first in [-I]. [c.mli] is [val x : L.M.t]. To check it, the
compiler loads the first [Ext] in [-I], so [c] should record [ext2]'s
[Ext].

  $ cat ext1/ext.mli
  type t = A
  $ cat ext2/ext.mli
  type t = B
  $ cat l/l.mli
  module M = Ext
  $ cat c/c.mli
  val x : L.M.t
  $ ocamlc -bin-annot -no-alias-deps -c ext1/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c l/l.mli
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext2 -I ext1 -c c/c.mli
  $ from c/c.cmti Ext ext1/ext.cmi ext2/ext.cmi
  ext2/ext.cmi

  $ odoc compile --output-dir h --parent-id pkg/ext1 -L ext1:h/pkg/ext1 ext1/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/ext2 -L ext2:h/pkg/ext2 ext2/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext1:h/pkg/ext1 l/l.cmti
  $ odoc compile --output-dir h --parent-id pkg/c -L c:h/pkg/c -L l:h/pkg/l -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 c/c.cmti
  $ odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L l:h/pkg/l -L c:h/pkg/c h/pkg/l/l.odoc
  File "Ext":
  Ambiguous lookup. Possible units: pkg/ext1/Ext, pkg/ext2/Ext
  $ odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L l:h/pkg/l -L c:h/pkg/c h/pkg/c/c.odoc

On [l]'s page, [M] should mean [ext1]'s [Ext], the author's target: Λc(L)
holds only that [Ext].

  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  module M/0 = resolved(root(pkg/ext2/Ext))

This is a known limit (section 3.3): Λc(C) holds two units named [Ext],
which D3 rules out, so the claims of section 3.7 do not hold for [c]. The
alias [Ext] in [x]'s path has the origin [L], so P2 gives [ext1]'s [Ext],
the author's choice, while the compiler took [ext2]'s for [c]. odoc links
[ext2]'s on [c]'s page: it resolves the path when [c] is compiled, with
[c]'s own digest for [Ext] (section 11, "Origins at link"). With D3 broken,
section 3.7 does not say which is right.

  $ odoc_print --short --long-paths --show-root-parents h/pkg/c/c.odocl
  val x/0 : resolved(alias(root(pkg/ext2/Ext),identifier(root(pkg/l/L),false).M).t)

Case 5 (P2). The example of section 3.3 again, with the alias's unit compiled
after its user. Library [x] has a module [Inner] and a main module [X] that
aliases it as [H], compiled with [-no-alias-deps]. Library [y] has a
different [Inner], with a type [u]. Library [z] depends on both, aliases
[X.H] as [M], and has [val v : M.u]. With [-no-alias-deps], the compiler
resolves [X.H] where it is used: in [z], whose load path has [y] first,
[X.H] is [y]'s [Inner]. [M.u] type-checks only for that one, so [z] should
record [y]'s [Inner].

  $ cat x/inner.mli
  type t
  val in_x : t
  $ cat x/x.mli
  module H = Inner
  $ cat y/inner.mli
  type u
  val in_y : u
  $ cat z/z.mli
  module M = X.H
  
  val v : M.u
  $ ocamlc -bin-annot -no-alias-deps -c x/inner.mli
  $ ocamlc -bin-annot -no-alias-deps -I x -c x/x.mli
  $ ocamlc -bin-annot -no-alias-deps -c y/inner.mli
  $ ocamlc -bin-annot -no-alias-deps -I y -I x -c z/z.mli
  $ from z/z.cmti Inner x/inner.cmi y/inner.cmi
  y/inner.cmi

[z] is compiled before [X], and [X] before [x]'s [Inner], so the paths are
left for link. This breaks D5 (section 2) on purpose. D5 only matters at
compile, and it is the one difference from case 4, where [l] is compiled
before [c].

  $ odoc compile --output-dir h --parent-id pkg/y -L y:h/pkg/y y/inner.cmti
  $ odoc compile --output-dir h --parent-id pkg/z -L z:h/pkg/z -L y:h/pkg/y -L x:h/pkg/x z/z.cmti
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/x.cmti
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/inner.cmti
  $ odoc link -L x:h/pkg/x -L y:h/pkg/y -L z:h/pkg/z h/pkg/z/z.odoc

Λc(z) holds two units named [Inner], which D3 rules out, so section 3.7
makes no promise for [z]. Link follows [M] into [X] and meets the root
[Inner] of [H], whose origin is [X]. P2 over Λc(X) gives [x]'s [Inner], the
author's choice. If odoc links that, [x]'s [Inner] has no type [u], so odoc
cannot find [M.u] in [v]'s type, and warns. Case 4 and this case differ
only in the order of compiling, so odoc may link different units in them.

  $ odoc_print --short --long-paths --show-root-parents h/pkg/z/z.odocl
  module M/0 =
    resolved(alias(root(pkg/y/Inner),identifier(root(pkg/x/X),false).H))
  val v/1 : resolved(alias(alias(root(pkg/y/Inner),identifier(root(pkg/x/X),false).H),local(M/0,false)).u)
