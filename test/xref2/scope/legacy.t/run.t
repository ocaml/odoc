Legacy units (doc/scope_model.mld, section 6). A unit compiled with no [-L]
is a legacy unit, and has no recorded libraries. For it, odoc should do what
it did before units recorded their libraries. The spec makes no other
promise about it.

Libraries [ext1] and [ext2] each have a module [Ext], with different
contents. [l] has [val x : Ext.t], so it records the digest of [Ext]. [a] has
[module M = Ext], and is compiled with [-no-alias-deps], so it records no
digest for [Ext]. Both are built against [ext1]. [n] has
[module X : module type of A], and is built against [a] and [ext1].

[l] and [a] are compiled the old way: no [-L], and [-I] for the directory of
[ext1]. [ext1] and [ext2] are compiled with no [-L] either.

  $ cat ext1/ext.mli
  type t = A
  $ cat ext2/ext.mli
  type t = B
  $ cat l/l.mli
  val x : Ext.t
  $ cat a/a.mli
  module M = Ext
  $ cat n/n.mli
  module X : module type of A
  $ ocamlc -bin-annot -no-alias-deps -c ext1/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c l/l.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c a/a.mli
  $ ocamlc -bin-annot -no-alias-deps -I a -I ext1 -c n/n.mli

  $ odoc compile --output-dir h --parent-id pkg/ext1 ext1/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/ext2 ext2/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/l -I h/pkg/ext1 l/l.cmti
  $ odoc compile --output-dir h --parent-id pkg/a -I h/pkg/ext1 a/a.cmti

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [x/0] is the item [x], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path, and
[root(pkg/ext1/Ext)] is the unit [Ext] of the library [ext1] in the package
[pkg]. [unresolvedroot(Ext)] is a root [Ext] that is not resolved. With
[--show-expansions], [=>] is followed by the signature odoc computed for a
module.

Case 1. A legacy unit is linked with [-I] for both [ext1] and [ext2], as a
driver that does not use [-L] would. odoc looks up a root with a digest by
that digest along the [-I] directories. So [x] should mean [ext1]'s [Ext].

  $ odoc link -I h/pkg/ext1 -I h/pkg/ext2 h/pkg/l/l.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  val x/0 : resolved(root(pkg/ext1/Ext).t)

odoc looks up a root with no digest by name, along all the [-I]
directories. Both [Ext]s match. odoc warns that the lookup is ambiguous,
and picks one of the units, as test/xref2/ambiguous_lookup.t shows for a
reference. So odoc should warn here, and resolve [M] to one of the two [Ext]s.
The spec does not fix the words of the warning, which now name the
units.

  $ odoc link -I h/pkg/ext1 -I h/pkg/ext2 h/pkg/a/a.odoc
  File "Ext":
  Ambiguous lookup. Possible units: pkg/ext1/Ext, pkg/ext2/Ext
  $ odoc_print --short --long-paths --show-root-parents h/pkg/a/a.odocl
  module M/0 = resolved(root(pkg/ext2/Ext))

Case 2. A recorded unit shows a legacy unit's expansion. [n] is compiled with
[-L] for [n], [a] and [ext1], so it is not a legacy unit. Its [X] has the
signature of [A], so the [Ext] in it has [a] as its origin, and [a] is a
legacy unit. The spec makes no claim about such a root (section 6). This
records what odoc does, at compile and at link with every library. The root
keeps its origin in [n]'s [.odoc] file, and [a] recorded no libraries, so at
link it is looked up anywhere, as in case 1: odoc warns that both [Ext]s
match, and picks one.

  $ odoc compile --output-dir h --parent-id pkg/n -L n:h/pkg/n -L a:h/pkg/a -L ext1:h/pkg/ext1 n/n.cmti
  $ odoc_print --short --long-paths --show-root-parents --show-expansions h/pkg/n/n.odoc
  module X/0 : module type of resolved(root(pkg/a/A))
    => sig module M/1 = unresolvedroot(Ext) end
  $ odoc link -L n:h/pkg/n -L a:h/pkg/a -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 h/pkg/n/n.odoc
  File "Ext":
  Ambiguous lookup. Possible units: pkg/ext1/Ext, pkg/ext2/Ext
  $ odoc_print --short --long-paths --show-root-parents --show-expansions h/pkg/n/n.odocl
  module X/0 : module type of resolved(root(pkg/a/A))
    => sig module M/1 = resolved(root(pkg/ext2/Ext)) end
