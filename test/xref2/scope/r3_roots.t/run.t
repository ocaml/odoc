Rule R3 (doc/scope_model.mld, section 4.2): a reference that R1 and R2 do
not find is a root. Its candidates are the units of that name in the
libraries given at link, Λl, not in the libraries the unit was compiled
with, Λc. [{!/library/N}] limits the candidates to one library.

The libraries, and what each is built against:

- [ext] has a unit [Ext]. [dup1] and [dup2] each have a unit [Dup]. [p1]
and [p2] each have a unit [P], with different contents. [w1] and [w2] each
have a unit [W]. [a] has a unit [A]. None of these is built against
anything.
- [l] is built against [ext], [dup1] and [p1]. It has one or two units for
each case below, each with one comment.
- [late] is built against [l], and so, by closure, [ext], [dup1] and [p1].
It has a unit [Late]. [l] is compiled before it.
- [c] is built against [p2]. It has a unit [C] with the pure alias
[module N = P].
- [b] is built against [a]. It has a unit [B].

Every compile uses dune's [-no-alias-deps], and [-L] for the unit's own
library and its dependencies.

  $ ocamlc -bin-annot -no-alias-deps -c ext/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c dup1/dup.mli
  $ ocamlc -bin-annot -no-alias-deps -c dup2/dup.mli
  $ ocamlc -bin-annot -no-alias-deps -c p1/p.mli
  $ ocamlc -bin-annot -no-alias-deps -c p2/p.mli
  $ ocamlc -bin-annot -no-alias-deps -c w1/w.mli
  $ ocamlc -bin-annot -no-alias-deps -c w2/w.mli
  $ ocamlc -bin-annot -no-alias-deps -c a/a.mli
  $ for u in inscope forward ambiguous ambiguous_used picked notgiven project project_used; do
  >   ocamlc -bin-annot -no-alias-deps -I ext -I dup1 -I p1 -c l/$u.mli
  > done
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext -I dup1 -I p1 -c late/late.mli
  $ ocamlc -bin-annot -no-alias-deps -I p2 -c c/c.mli
  $ ocamlc -bin-annot -no-alias-deps -I a -c b/b.mli

  $ odoc compile --output-dir h --parent-id pkg/ext -L ext:h/pkg/ext ext/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/dup1 -L dup1:h/pkg/dup1 dup1/dup.cmti
  $ odoc compile --output-dir h --parent-id pkg/dup2 -L dup2:h/pkg/dup2 dup2/dup.cmti
  $ odoc compile --output-dir h --parent-id pkg/p1 -L p1:h/pkg/p1 p1/p.cmti
  $ odoc compile --output-dir h --parent-id pkg/p2 -L p2:h/pkg/p2 p2/p.cmti
  $ odoc compile --output-dir h --parent-id pkg/w1 -L w1:h/pkg/w1 w1/w.cmti
  $ odoc compile --output-dir h --parent-id pkg/w2 -L w2:h/pkg/w2 w2/w.cmti
  $ odoc compile --output-dir h --parent-id pkg/a -L a:h/pkg/a a/a.cmti
  $ for u in inscope forward ambiguous ambiguous_used picked notgiven project project_used; do
  >   odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext -L dup1:h/pkg/dup1 -L p1:h/pkg/p1 l/$u.cmti
  > done
  $ odoc compile --output-dir h --parent-id pkg/late -L late:h/pkg/late -L l:h/pkg/l -L ext:h/pkg/ext -L dup1:h/pkg/dup1 -L p1:h/pkg/p1 late/late.cmti
  $ odoc compile --output-dir h --parent-id pkg/c -L c:h/pkg/c -L p2:h/pkg/p2 c/c.cmti
  $ odoc compile --output-dir h --parent-id pkg/b -L b:h/pkg/b -L a:h/pkg/a b/b.cmti

Unless a case says otherwise, every library is given at link. [$ALL] holds
the [-L] options for that. [$L] holds only those of [l] and its
dependencies.

  $ ALL="-L ext:h/pkg/ext -L dup1:h/pkg/dup1 -L dup2:h/pkg/dup2 -L p1:h/pkg/p1 -L p2:h/pkg/p2 -L w1:h/pkg/w1 -L w2:h/pkg/w2 -L a:h/pkg/a -L l:h/pkg/l -L late:h/pkg/late -L c:h/pkg/c -L b:h/pkg/b"
  $ L="-L l:h/pkg/l -L ext:h/pkg/ext -L dup1:h/pkg/dup1 -L p1:h/pkg/p1"

How to read [odoc_print --refs --long-paths --show-root-parents]. Each line
is one reference, in the order of the source. [r(...)] is a resolved
reference. [/late/Late] is the reference [{!/late/Late}], not resolved.
[root(pkg/ext/Ext)] is the unit [Ext] of the library [ext] in the
package [pkg]. [alias(A,B)] is a reference written through the alias [B],
which resolves to [A]. A [.t] after it is the item [t], found in it by
projection.

Case 1 (R3): [Inscope] has [{!Ext}]. [ext] is in Λc and in Λl, and no other
library has an [Ext]. [{!Ext}] should mean [ext]'s [Ext].

  $ cat l/inscope.mli
  type t
  (** See {!Ext}. *)
  $ cat ext/ext.mli
  type t
  $ odoc link $ALL h/pkg/l/inscope.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/inscope.odocl
  r(root(pkg/ext/Ext))

Case 2 (R3): a forward reference. [Forward] has [{!Late}]. [late] is not in
Λc: it is compiled after [l], and depends on it. It is in Λl. [{!Late}]
should mean [late]'s [Late].

  $ cat l/forward.mli
  type t
  (** See {!Late}. *)
  $ cat late/late.mli
  val x : Inscope.t
  $ odoc link $ALL h/pkg/l/forward.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/forward.odocl
  r(root(pkg/late/Late))

Case 3 (R3): [Ambiguous] has [{!Dup}]. [dup1] is in Λc, and [dup2] is added at
link. Both have a [Dup], so there are two candidates. [{!Dup}] should be
ambiguous: odoc should warn, and leave it unresolved. The author picks one
with [{!/dup1/Dup}] (case 4).

Resolving it to either library's [Dup] would be wrong.

  $ cat l/ambiguous.mli
  type t
  (** See {!Dup}. *)
  $ cat l/ambiguous_used.mli
  type t = Dup.t
  (** See {!Dup}. *)
  $ cat dup1/dup.mli
  type t
  $ cat dup2/dup.mli
  type t
  $ odoc link $ALL h/pkg/l/ambiguous.odoc
  File "Dup":
  Ambiguous lookup. Possible units: pkg/dup1/Dup, pkg/dup2/Dup
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/ambiguous.odocl
  r(root(pkg/dup2/Dup))

[Ambiguous] does not use [Dup] in its code. [Ambiguous_used] has the same
comment, but has [type t = Dup.t], so it records the digest of [dup1]'s
[Dup]. R3 does not look at the import table (its candidates are
[units(Λl, N)], section 4.2), so the answer
should be the same: odoc should warn, and leave [{!Dup}] unresolved.

As for [Ambiguous], resolving it to either library's [Dup] would be wrong.

  $ odoc link $ALL h/pkg/l/ambiguous_used.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/ambiguous_used.odocl
  r(root(pkg/dup1/Dup))

Case 4 (R3): [Picked] has [{!/dup1/Dup}] and [{!/dup2/Dup}]. Each should mean
the [Dup] of the library it names, with no warning.

  $ cat l/picked.mli
  type t
  (** See {!/dup1/Dup} and {!/dup2/Dup}. *)
  $ odoc link $ALL h/pkg/l/picked.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/picked.odocl
  r(root(pkg/dup1/Dup))
  r(root(pkg/dup2/Dup))

Case 5 (R3): [Notgiven] has [{!/late/Late}]. With every library given, it
should mean [late]'s [Late].

  $ cat l/notgiven.mli
  type t
  (** See {!/late/Late}. *)
  $ odoc link $ALL h/pkg/l/notgiven.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/notgiven.odocl
  r(root(pkg/late/Late))

When only [l] and its dependencies are given, [late] is not in Λl. The
reference should not resolve, and should warn (section 4.5).

  $ odoc link $L h/pkg/l/notgiven.odoc
  File "l/notgiven.mli", line 2, characters 8-21:
  Warning: Failed to resolve reference /late/Late Path '/late/Late' not found
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/notgiven.odocl
  /late/Late

A missing root in a reference is checked in [reporting.t].

Case 6 (R3, with section 3.2): projection through a forward reference into a
pure alias. [Project] has [{!C.N.t}]. [C] is found by R3, as [c] is in Λl.
[N] is the alias [module N = P], written in [C], and [C] records no digest
for [P]. So the alias is resolved by P2 over [Λc(C)], which holds [p2]
only. [{!C.N.t}] should mean the [t] of [p2]'s [P], through the alias. It
should not mean [p1]'s [P], even though [p1] is in the Λc of [Project].

  $ cat l/project.mli
  type t
  (** See {!C.N.t}. *)
  $ cat l/project_used.mli
  val x : P.t
  (** See {!C.N.t}. *)
  $ cat p1/p.mli
  type t = A
  $ cat p2/p.mli
  type t = B
  $ cat c/c.mli
  module N = P
  $ odoc link $ALL h/pkg/l/project.odoc
  File "P":
  Ambiguous lookup. Possible units: pkg/p1/P, pkg/p2/P
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/project.odocl
  r(alias(root(pkg/p2/P),root(pkg/c/C).N).t)

[Project] does not use [P] in its code. [Project_used] has the same comment,
but has [val x : P.t], so it records the digest of [p1]'s [P]. The import
table that decides between P1 and P2 is that of the alias's origin, [C],
not that of the unit being linked. So [{!C.N.t}] should again mean the [t]
of [p2]'s [P].

  $ odoc link $ALL h/pkg/l/project_used.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/l/project_used.odocl
  r(alias(root(pkg/p1/P),root(pkg/c/C).N).t)

Case 7 (R3): a comment shown on two pages. In [A], the module type [S] has a
[val v], whose comment has [{!W}]. [B] has [module X : A.S], so the comment
is shown on [X]'s page in [b] too. [A] is linked with [w1] given, and [B]
with [w2] given.

This is a known limit (section 4.2): a root reference is resolved with the
Λl of each page. So odoc may link [{!W}] to [w1]'s [W] on [S]'s page, and
to [w2]'s [W] on [X]'s page.

[odoc_print --refs] does not follow [A.S] into [B], so the links are read
from the HTML. [grep] shows the target of the link after "See" on each
page.

  $ cat w1/w.mli
  type t
  $ cat w2/w.mli
  type t
  $ cat a/a.mli
  module type S = sig
    val v : int
    (** See {!W}. *)
  end
  $ cat b/b.mli
  module X : A.S
  $ odoc link -L a:h/pkg/a -L w1:h/pkg/w1 h/pkg/a/a.odoc
  $ odoc link -L b:h/pkg/b -L a:h/pkg/a -L w2:h/pkg/w2 h/pkg/b/b.odoc
  $ odoc html-generate -o html h/pkg/a/a.odocl
  $ odoc html-generate -o html h/pkg/b/b.odocl
  $ grep -o 'See <a href="[^"]*"' html/pkg/a/A/module-type-S/index.html
  See <a href="../../../w1/W/index.html"
  $ grep -o 'See <a href="[^"]*"' html/pkg/b/B/X/index.html
  See <a href="../../../w2/W/index.html"
