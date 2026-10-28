How failures are reported (doc/scope_model.mld, section 3.6 for paths, section
4.5 for references). A path whose innermost failing root is missing, that is,
no unit of the searched libraries has its name, is reported only with
[--enable-missing-root-warning], because the D6 check already names the
missing library. Every other failure of a path gets an ordinary warning. A
reference that does not resolve always gets an ordinary warning.

The libraries:

- [ext] has [Ext], with [type t] and a value.
- [f] has [F], with a functor [Make].
- [g] has [G], with [type t] and a value.
- [l] has [module M = Ext]. It is built against [ext], with [-no-alias-deps],
so [l] records no digest for [Ext].
- [r] has a comment with the references [{!Ext.t}] and [{!Nope}]. It is built
against [ext]. No unit is called [Nope].
- [e2] has [val z : F.Make(Ext).t]. It is built against [f] and an older
build of [ext]'s [Ext], with no value, in [old_ext].
- [e3] has [val w : G.t]. It is built against an older build of [g]'s [G], with
no value, in [old_g].

So the digest that [e2] records for [Ext], and the one [e3] records for [G],
match no unit odoc is given. This is what a stale build looks like: [ext]
and [g] were rebuilt after [e2] and [e3]. It breaks D1 (section 2) on
purpose. dune rebuilds [e2] and [e3] when [ext] and [g] change, so it
stands for a driver that reuses old [.odoc] files. [old_ext] is also given to odoc,
as a library of its own, for case 1. Every compile uses dune's
[-no-alias-deps], and [-L] for the unit's own library and its dependencies.

  $ cat e2/e2.mli
  val z : F.Make(Ext).t
  $ cat e3/e3.mli
  val w : G.t
  $ cat l/l.mli
  module M = Ext
  $ cat r/r.mli
  (** See {!Ext.t} and {!Nope}. *)
  val v : int
  $ cat old_ext/ext.mli
  type t
  $ cat ext/ext.mli
  type t
  val v : int
  $ cat f/f.mli
  module Make (X : sig type t end) : sig type t end
  $ cat old_g/g.mli
  type t
  $ cat g/g.mli
  type t
  val v : int
  $ ocamlc -bin-annot -no-alias-deps -c old_ext/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c old_g/g.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c f/f.mli
  $ ocamlc -bin-annot -no-alias-deps -c g/g.mli
  $ ocamlc -bin-annot -no-alias-deps -I f -I old_ext -c e2/e2.mli
  $ ocamlc -bin-annot -no-alias-deps -I old_g -c e3/e3.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext -c l/l.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext -c r/r.mli

  $ odoc compile --output-dir h --parent-id pkg/ext -L ext:h/pkg/ext ext/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/old_ext -L old_ext:h/pkg/old_ext old_ext/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/f -L f:h/pkg/f f/f.cmti
  $ odoc compile --output-dir h --parent-id pkg/g -L g:h/pkg/g g/g.cmti
  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext l/l.cmti
  $ odoc compile --output-dir h --parent-id pkg/r -L r:h/pkg/r -L ext:h/pkg/ext r/r.cmti
  $ odoc compile --output-dir h --parent-id pkg/e2 -L e2:h/pkg/e2 -L f:h/pkg/f -L ext:h/pkg/ext e2/e2.cmti
  $ odoc compile --output-dir h --parent-id pkg/e3 -L e3:h/pkg/e3 -L g:h/pkg/g e3/e3.cmti

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [x/0] is the item [x], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path, and
[root(pkg/ext/Ext)] is the unit [Ext] of the library [ext] in the package
[pkg]. [unresolvedroot(Ext)] is a root [Ext] that is not resolved.

Case 1: a missing root (section 3.6). [l]'s alias has no digest, so P2 applies
to it, and odoc leaves it for link (section 7). [ext] is not given at link, so no unit
called [Ext] is in the searched libraries. odoc should warn once that [ext]
is missing (D6). It should not report the root itself.

  $ odoc link -L l:h/pkg/l h/pkg/l/l.odoc

With [--enable-missing-root-warning], odoc should also report that it could
not find [Ext].

  $ odoc link -L l:h/pkg/l --enable-missing-root-warning h/pkg/l/l.odoc
  File "h/pkg/l/l.odoc":
  Warning: Couldn't find the following modules:
    Ext

Either way, the alias should stay unresolved:

  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  module M/0 = unresolvedroot(Ext)

The same should hold when [ext]'s directory is given with [-I], not [-L].
[-I] does not name a library, so [ext] is still missing (D6). odoc should
warn once that [ext] is missing, not report the root, and leave the alias
unresolved.

  $ odoc link -L l:h/pkg/l -I h/pkg/ext h/pkg/l/l.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  module M/0 = resolved(root(pkg/ext/Ext))

It should also hold when another library with a unit [Ext] is given at
link. [old_ext] has one, but it is not in Λc(L), so its [Ext] is never a
candidate (P2, section 8).

  $ odoc link -L l:h/pkg/l -L old_ext:h/pkg/old_ext h/pkg/l/l.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  module M/0 = resolved(root(pkg/old_ext/Ext))

Case 2: a failure is classified by the innermost root that fails (section 3.6).
In [e2]'s [F.Make(Ext).t], the outer root [F] has a digest that matches
[f]'s [F]. The inner root [Ext] has a digest that matches no unit. The path
should stay unresolved.

With [ext] given at link, a unit called [Ext] is there, so the inner root
fails because no unit matches its digest. odoc should give an ordinary
warning, with or without the flag.

No warning would be wrong (section 11, "Digests no unit matches"), and so
would the message for a missing root.

  $ odoc link -L e2:h/pkg/e2 -L f:h/pkg/f -L ext:h/pkg/ext h/pkg/e2/e2.odoc
  $ odoc link -L e2:h/pkg/e2 -L f:h/pkg/f -L ext:h/pkg/ext --enable-missing-root-warning h/pkg/e2/e2.odoc
  File "h/pkg/e2/e2.odoc":
  Warning: Couldn't find the following modules:
    Ext
  $ odoc_print --short --long-paths --show-root-parents h/pkg/e2/e2.odocl
  val z/0 : unresolvedroot(F).Make(unresolvedroot(Ext)).t

With [ext] not given at link, the inner root is missing. Its outer root [F]
has a unit that matches, but the failure should be classified by [Ext], as
a missing root. So odoc should only warn that [ext] is missing. With the flag, it
should also say that it could not find [Ext], and nothing about [F].

  $ odoc link -L e2:h/pkg/e2 -L f:h/pkg/f h/pkg/e2/e2.odoc
  $ odoc link -L e2:h/pkg/e2 -L f:h/pkg/f --enable-missing-root-warning h/pkg/e2/e2.odoc
  File "h/pkg/e2/e2.odoc":
  Warning: Couldn't find the following modules:
    Ext

Case 3: a missing root takes precedence over a digest that no unit matches
(section 3.6). [e3]'s [G] has a digest that matches no unit.

With [g] given at link, a unit called [G] is there. So this is a digest
mismatch, and odoc should give an ordinary warning, with or without the
flag.

As in case 2, no warning would be wrong (section 11, "Digests no unit
matches"), and so would the message for a missing root.

  $ odoc link -L e3:h/pkg/e3 -L g:h/pkg/g h/pkg/e3/e3.odoc
  $ odoc link -L e3:h/pkg/e3 -L g:h/pkg/g --enable-missing-root-warning h/pkg/e3/e3.odoc
  File "h/pkg/e3/e3.odoc":
  Warning: Couldn't find the following modules:
    G

With [g] not given at link, no unit called [G] is there either. The root
should count as missing, not as a digest mismatch. So odoc should only warn
that [g] is missing, and, with the flag, that it could not find [G].

  $ odoc link -L e3:h/pkg/e3 h/pkg/e3/e3.odoc
  $ odoc link -L e3:h/pkg/e3 --enable-missing-root-warning h/pkg/e3/e3.odoc
  File "h/pkg/e3/e3.odoc":
  Warning: Couldn't find the following modules:
    G

References (section 4.5). A reference that does not resolve should always
get an ordinary warning, with or without the flag, even when its root is
missing. With [ext] not given at link, both [{!Ext.t}] and [{!Nope}] fail,
and each should get a warning. [ext] should also be reported as missing
(D6), as [r] was built against it.

  $ odoc link -L r:h/pkg/r h/pkg/r/r.odoc
  File "r/r.mli", line 1, characters 21-28:
  Warning: Failed to resolve reference unresolvedroot(Nope) Couldn't find "Nope"
  File "r/r.mli", line 1, characters 8-16:
  Warning: Failed to resolve reference unresolvedroot(Ext).t Couldn't find "Ext"

The flag should not change the reference warnings. It covers paths only.

  $ odoc link -L r:h/pkg/r --enable-missing-root-warning h/pkg/r/r.odoc
  File "r/r.mli", line 1, characters 21-28:
  Warning: Failed to resolve reference unresolvedroot(Nope) Couldn't find "Nope"
  File "r/r.mli", line 1, characters 8-16:
  Warning: Failed to resolve reference unresolvedroot(Ext).t Couldn't find "Ext"

With [ext] given, only [{!Nope}] should fail. Its root is still missing, as
no library has a unit [Nope], as for a typo. It should warn in the same way
with and without the flag.

  $ odoc link -L r:h/pkg/r -L ext:h/pkg/ext h/pkg/r/r.odoc
  File "r/r.mli", line 1, characters 21-28:
  Warning: Failed to resolve reference unresolvedroot(Nope) Couldn't find "Nope"
  $ odoc link -L r:h/pkg/r -L ext:h/pkg/ext --enable-missing-root-warning h/pkg/r/r.odoc
  File "r/r.mli", line 1, characters 21-28:
  Warning: Failed to resolve reference unresolvedroot(Nope) Couldn't find "Nope"
