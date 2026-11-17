Rule O1 (doc/scope_model.mld, sections 3.1 and 7): a root that the compiler
made, and that odoc does not recompute, has as its origin the unit whose
compiled file holds it. An example is the inferred type of a value in a
[.cmt] with no [.mli].

Libraries [ext1] and [ext2] each have a module [Ext], with different
contents. [g] is built against [ext1], and has [f : unit -> Ext.t]. [z] is
built against [g] and [ext1]. It has only [z.ml], with [let v = G.f ()], so
the compiler infers [v : Ext.t]. [w] is built against [z], [g] and [ext1],
and has [include module type of Z]. No library is built against [ext2] (D3,
section 2), but it is given at link.

  $ cat ext1/ext.mli
  type t = A
  $ cat ext2/ext.mli
  type t = B
  $ cat g/g.mli
  val f : unit -> Ext.t
  $ cat z/z.ml
  let v = G.f ()
  $ cat w/w.mli
  include module type of Z
  $ ocamlc -bin-annot -no-alias-deps -c ext1/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -I g -I ext1 -c g/g.mli
  $ ocamlc -bin-annot -no-alias-deps -I z -I g -I ext1 -c z/z.ml
  $ ocamlc -bin-annot -no-alias-deps -I w -I z -I g -I ext1 -c w/w.mli

[z.ml] never names [Ext]. The compiler loaded [Ext]'s [.cmi] to type [v], so
[z]'s import table should have an entry for [Ext], with a digest. [entry
FILE N], from [../digests.sh], prints [digest], [no digest] or [no entry]
for [N] in [FILE]'s import table:

  $ . ../digests.sh
  $ entry z/z.cmi Ext
  digest

  $ odoc compile --output-dir h --parent-id pkg/ext1 -L ext1:h/pkg/ext1 ext1/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/ext2 -L ext2:h/pkg/ext2 ext2/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/g -L g:h/pkg/g -L ext1:h/pkg/ext1 g/g.cmti
  $ odoc compile --output-dir h --parent-id pkg/z -L z:h/pkg/z -L g:h/pkg/g -L ext1:h/pkg/ext1 z/z.cmt
  $ odoc compile --output-dir h --parent-id pkg/w -L w:h/pkg/w -L z:h/pkg/z -L g:h/pkg/g -L ext1:h/pkg/ext1 w/w.cmti

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [x/0] is the item [x], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path, and
[root(pkg/ext1/Ext)] is the unit [Ext] of the library [ext1] in the package
[pkg]. With [--show-include-expansions], [=>] is followed by the signature
odoc computed for an [include].

Case 1. The root [Ext] in [v]'s type has the origin [Z], whose import table
records [ext1]'s digest for it. So by rule P1 (section 3.2) it should mean
[ext1]'s [Ext], and as [ext1] was compiled first (D5), it should already be
resolved in [z]'s [.odoc] file:

  $ odoc_print --short --long-paths --show-root-parents h/pkg/z/z.odoc
  val v/0 : resolved(root(pkg/ext1/Ext).t)

At link, with [ext2] given too, odoc should not look the root up again,
as it is already resolved (section 3.1). So it should still mean [ext1]'s:

  $ link() { odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L g:h/pkg/g -L z:h/pkg/z -L w:h/pkg/w "$@"; }
  $ link h/pkg/z/z.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/z/z.odocl
  val v/0 : resolved(root(pkg/ext1/Ext).t)

[w] shows [v] through its [include], so [v] should still have [ext1]'s
[Ext.t] on [w]'s page, with no warning. The root was resolved when [z] was
compiled, so this case checks that the resolved root is copied; it cannot
show an unresolved root keeping its origin (origin_expand.t checks that):

  $ link h/pkg/w/w.odoc
  $ odoc_print --short --long-paths --show-root-parents --show-include-expansions h/pkg/w/w.odocl
  include module type of resolved(root(pkg/z/Z))
    => sig val v/0 : resolved(root(pkg/ext1/Ext).t) end
