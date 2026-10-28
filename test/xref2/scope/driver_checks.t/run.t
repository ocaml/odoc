The conditions on the driver (doc/scope_model.mld, section 2), and what odoc
checks of them.

The libraries:

- [ext] has [Ext], with [type t].
- [l] has [L], with [val x : Ext.t], as an interface and an implementation.
It is built against [ext].
- [n] has [module X : module type of L]. It is built against [l], and so, by
closure, [ext].
- [ext2] has [Ext2], with [type t].
- [r] has [module Y = Ext2]. It is built against [ext2], with
[-no-alias-deps], so [r] records no digest for [Ext2].
- [p] has a comment with the reference [{!R.Y.t}]. It is built against no
library: the reference is a forward reference to [r].

Every compile uses dune's [-no-alias-deps], and [-L] for the unit's own
library and its dependencies.

  $ cat ext/ext.mli
  type t
  $ cat n/n.mli
  module X : module type of L
  $ cat p/p.mli
  (** See {!R.Y.t}. *)
  val v : int
  $ cat l/l.mli
  val x : Ext.t
  $ cat l/l.ml
  let x : Ext.t = Obj.magic 0
  $ cat ext2/ext2.mli
  type t
  $ cat r/r.mli
  module Y = Ext2
  $ ocamlc -bin-annot -no-alias-deps -c ext/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext -c l/l.mli
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext -c l/l.ml
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext -c n/n.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext2.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext2 -c r/r.mli
  $ ocamlc -bin-annot -no-alias-deps -c p/p.mli

  $ odoc compile --output-dir h --parent-id pkg/ext -L ext:h/pkg/ext ext/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext l/l.cmti
  $ odoc compile-impl --output-dir h --parent-id pkg/l --source-id pkg/l/l.ml -L l:h/pkg/l -L ext:h/pkg/ext l/l.cmt
  $ odoc compile --output-dir h --parent-id pkg/n -L n:h/pkg/n -L l:h/pkg/l -L ext:h/pkg/ext n/n.cmti
  $ odoc compile --output-dir h --parent-id pkg/ext2 -L ext2:h/pkg/ext2 ext2/ext2.cmti
  $ odoc compile --output-dir h --parent-id pkg/r -L r:h/pkg/r -L ext2:h/pkg/ext2 r/r.cmti
  $ odoc compile --output-dir h --parent-id pkg/p -L p:h/pkg/p p/p.cmti

Case 1. The D6 check (section 2): at the link of a unit, every library that a
unit whose paths are resolved was compiled with should be given, and odoc
should warn about each one that is missing. [n] shows the expansion of [L],
whose origin [l] was compiled with [ext]. Without [ext] at link, odoc should
warn once that [ext] is missing.

  $ odoc link -L n:h/pkg/n -L l:h/pkg/l h/pkg/n/n.odoc

Without [l] and [ext], it should warn once about each of them.

  $ odoc link -L n:h/pkg/n h/pkg/n/n.odoc

The D6 message is a warning (section 2). So it should follow the options for
warnings, as other warnings do. With [--warn-error], it should be an error,
and odoc should fail. With [--print-warnings false], it should not be
printed.

  $ odoc link --warn-error -L n:h/pkg/n -L l:h/pkg/l h/pkg/n/n.odoc
  $ odoc link --print-warnings false -L n:h/pkg/n -L l:h/pkg/l h/pkg/n/n.odoc

The same check applies when [L]'s implementation is linked: it records the
same libraries (D8). Without [ext], odoc should warn that it is missing.

A link of [impl-l] with no warning would be wrong.

  $ odoc link -L l:h/pkg/l h/pkg/l/impl-l.odoc

Case 2. D6 for a unit reached by a reference (section 2). [p]'s reference
[{!R.Y.t}] is projected through [r], whose paths are resolved at the link of
[p]: [R.Y] is an alias to [Ext2], which [r] was compiled with. D6 covers the
units reached by projecting a reference, so without [ext2] at link, odoc
should warn that [ext2] is missing. The reference fails, so it should also
get an ordinary warning (section 4.5).

  $ odoc link -L p:h/pkg/p -L r:h/pkg/r h/pkg/p/p.odoc
  File "p/p.mli", line 1, characters 8-16:
  Warning: Failed to resolve reference unresolvedroot(R).Y.t Parent_sig: Unresolved module path unresolvedroot(Ext2) (Lookup failure (root module): Ext2)

With [ext2] given, the reference should resolve, with no warning.

  $ odoc link -L p:h/pkg/p -L r:h/pkg/r -L ext2:h/pkg/ext2 h/pkg/p/p.odoc

The same holds for a path reference, which names its library and so finds
[r] by its path, not by its name. [q] is built against no library but its
own, and its
comment has [{!/r/R.Y.t}]. Without [ext2] at link, odoc should warn that
[ext2] is missing, as for [p].

A link of [q] with no warning about [ext2] would be wrong.

  $ cat q/q.mli
  (** See {!/r/R.Y.t}. *)
  val w : int
  $ ocamlc -bin-annot -no-alias-deps -c q/q.mli
  $ odoc compile --output-dir h --parent-id pkg/q -L q:h/pkg/q q/q.cmti
  $ odoc link -L q:h/pkg/q -L r:h/pkg/r h/pkg/q/q.odoc
  File "q/q.mli", line 1, characters 8-19:
  Warning: Failed to resolve reference /r/R.Y.t Parent_sig: Unresolved module path unresolvedroot(Ext2) (Lookup failure (root module): Ext2)

A page's references reach units in the same way. The page [pg] has
[{!R.Y.t}]. Without [ext2] at link, odoc should warn that [ext2] is
missing, as for [p].

A link of [pg] with no warning about [ext2] would be wrong.

  $ cat pg.mld
  {0 Notes}
  
  See {!R.Y.t}.
  $ odoc compile --output-dir h --parent-id pkg pg.mld
  $ odoc link -L r:h/pkg/r h/pkg/page-pg.odoc
  File "pg.mld", line 3, characters 4-12:
  Warning: Failed to resolve reference unresolvedroot(R).Y.t Parent_sig: Unresolved module path unresolvedroot(Ext2) (Lookup failure (root module): Ext2)

A unit reached by its identifier, which is how a path resolved at compile
names it, is checked in the same way. No case here shows it.

Case 3. A unit should record its [Λc], the libraries given with [-L] when it
was compiled (section 1). D8 (section 2): a unit made by [odoc compile-impl]
has the same [Λc] as the interface of the same module. The driver gave [l]
and [ext] to both [odoc compile] and [odoc compile-impl] of [L], so each
unit should record [l] and [ext]. [odoc_print ... | jq -c .libraries] shows
the libraries that a unit records as its [Λc].

  $ odoc_print h/pkg/l/l.odoc | jq -c .libraries
  ["l","ext"]
  $ odoc_print h/pkg/l/impl-l.odoc | jq -c .libraries
  ["l","ext"]

Case 4. A library is a name bound to one directory (section 1), and a name
means the same directory in every run (D7). A [-L] set that binds one name
to two directories breaks this, and odoc should reject it with an error, at
compile as at link. If the compile is rejected, [l.odoc] is still the file of
an earlier compile, and the link below reads that file.

A link that accepts it would be wrong.

  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext2 l/l.cmti
  $ odoc link -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext2 h/pkg/l/l.odoc

Section 1 lets a driver bind one directory to several names. [odoc link]
checks by default that no two [-L] directories overlap, so it should reject
such a set, and accept it with [--custom-layout], which turns the check off.

A link with [--custom-layout] that rejects it would be wrong.

  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext -L other:h/pkg/ext l/l.cmti
  $ odoc link -L l:h/pkg/l -L ext:h/pkg/ext -L other:h/pkg/ext h/pkg/l/l.odoc
  ERROR: Paths given to all -L options must be disjoint
  [1]
  $ odoc link --custom-layout -L l:h/pkg/l -L ext:h/pkg/ext -L other:h/pkg/ext h/pkg/l/l.odoc

The spec does not say what odoc should do with the same [-L] given twice;
this records what odoc does. The unit should record the name once.

  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext l/l.cmti
  $ odoc_print h/pkg/l/l.odoc | jq -c .libraries
  ["l","ext","ext"]
  $ odoc link -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext h/pkg/l/l.odoc
  ERROR: Paths given to all -L options must be disjoint
  [1]

Case 5. Some commands take [-I] and no [-L], such as [odoc html-url]. They
know no library names, so they cannot limit a lookup to the libraries a
unit recorded. They should look a name up along the whole search path, as
for a legacy unit (section 6). [r]'s alias [Y] records no digest for
[Ext2], so [R.Y.t] should resolve to [ext2]'s [Ext2.t].

An error that [Ext2] is not found would be wrong.

  $ odoc html-url -I h/pkg/r -I h/pkg/ext2 R.Y.t
  pkg/ext2/Ext2/index.html#type-t

[odoc html] from a [.odoc] file that is not linked takes no [-L] either,
and links the unit first. [R]'s page should link [Y] to [ext2]'s [Ext2].
A page with no such link would be wrong.

  $ odoc html -I h/pkg/r -I h/pkg/ext2 -o html_d6 h/pkg/r/r.odoc
  $ grep -o 'href="[^"]*Ext2[^"]*"' html_d6/pkg/r/R/index.html
  href="../../ext2/Ext2/index.html"
