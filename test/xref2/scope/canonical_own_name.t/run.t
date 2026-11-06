Rule R3 (doc/scope_model.mld, section 4.2) for a [@canonical] tag, which is
a reference (S1, section 5): if the origin of a reference is a unit named
[N], its root [N] means that unit. dune writes the tags of a wrapped library
that has no hand-written main module into the alias module [N] itself, so
their origin is [N]. A tag written in [N__] is not covered by this rule;
that is the case of s2_dune_wrapped.t.

Libraries [a1] and [a2] are two dune wrapped libraries with the same name,
[lib], for example from two projects. Each has the alias module [Lib], with
[module Foo = Lib__Foo] and the tag [@canonical Lib.Foo], and the member
[Lib__Foo]. Their [t]s differ. The library [cc] is a dune wrapped library
built against [a1] only. Its member [Cc__User] is [type t = Lib.Foo.t].
No unit's Λc holds both [Lib]s (D3). As dune does, each alias module is
compiled first, with [-nopervasives -nostdlib -w -49], and the members with
[-open] of it. Every compile uses dune's [-no-alias-deps], and [-L] for the
unit's own library and its dependencies.

  $ cat a1/lib.ml-gen
  (** @canonical Lib.Foo *)
  module Foo = Lib__Foo
  $ cat a2/lib.ml-gen
  (** @canonical Lib.Foo *)
  module Foo = Lib__Foo
  $ cat a1/foo.mli
  type t = T1
  $ cat a2/foo.mli
  type t = T2
  $ cat cc/cc.ml-gen
  (** @canonical Cc.User *)
  module User = Cc__User
  $ cat cc/user.mli
  type t = Lib.Foo.t
  $ for a in a1 a2; do
  >   ocamlc -bin-annot -no-alias-deps -nopervasives -nostdlib -w -49 -I $a -o $a/lib.cmo -c -impl $a/lib.ml-gen
  >   ocamlc -bin-annot -no-alias-deps -I $a -open Lib -o $a/lib__Foo.cmi -c -intf $a/foo.mli
  > done
  $ ocamlc -bin-annot -no-alias-deps -nopervasives -nostdlib -w -49 -I cc -o cc/cc.cmo -c -impl cc/cc.ml-gen
  $ ocamlc -bin-annot -no-alias-deps -I cc -I a1 -open Cc -o cc/cc__User.cmi -c -intf cc/user.mli

  $ for a in a1 a2; do
  >   odoc compile --output-dir h --parent-id pkg/$a -L $a:h/pkg/$a $a/lib.cmt
  >   odoc compile --output-dir h --parent-id pkg/$a -L $a:h/pkg/$a $a/lib__Foo.cmti
  > done
  $ odoc compile --output-dir h --parent-id pkg/cc -L cc:h/pkg/cc -L a1:h/pkg/a1 cc/cc.cmt
  $ odoc compile --output-dir h --parent-id pkg/cc -L cc:h/pkg/cc -L a1:h/pkg/a1 cc/cc__User.cmti

Every library is given at link. [$ALL] holds the [-L] options for that.

  $ ALL="-L a1:h/pkg/a1 -L a2:h/pkg/a2 -L cc:h/pkg/cc"
  $ for u in a1/lib a1/lib__Foo a2/lib a2/lib__Foo cc/cc__User; do odoc link $ALL h/pkg/$u.odoc; done

How to read [odoc_print --short --long-paths --show-root-parents
--show-expansions]. Each line is one item of the signature. [t/1] is the
item [t], with a number odoc adds to tell apart items of the same name.
[resolved(...)] is a resolved path, and [root(pkg/a1/Lib)] is the unit
[Lib] of the library [a1] in the package [pkg]. [hidden(...)] marks a path
to a hidden unit. [=>] is followed by the signature odoc computed for a
module. [canonical(A,resolved(B))] is the path [A] shown as its
[@canonical] target [B].

[Cc]'s page shows the expansion of the hidden [Cc__User]. Its [Lib.Foo.t]
means [a1]'s [Lib__Foo]'s [t], by its digest (P1). [Cc] records nothing for
[Lib], and the tag [Lib.Foo] is a reference, so it does not depend on what
[Cc] records. Its root [Lib] has two candidates in Λl, [a1]'s and [a2]'s. The
tag is written in [a1]'s [Lib], so by R3 it should resolve to [a1]'s
[Lib.Foo], with no warning. odoc resolves references at link, not where
they are written (section 11, "R0"), so it does not apply this rule itself.
S2's temporary rule, where odoc has it, gives the same answer.

Showing the path as, or linking it to, [a2]'s [Lib.Foo] would be wrong.

  $ odoc link $ALL h/pkg/cc/cc.odoc
  $ odoc_print --short --long-paths --show-root-parents --show-expansions h/pkg/cc/cc.odocl
  module User/0 = resolved(hidden(root(pkg/cc/Cc__User)))
    => sig
      type t/1 = resolved(canonical(root(pkg/a1/Lib).Foo,resolved(root(pkg/a1/Lib).Foo)).t)
     end

The link, in the HTML of [Cc.User]:

  $ odoc html-generate -o html h/pkg/cc/cc.odocl
  $ grep -o 'href="[^"]*Foo[^"]*"' html/pkg/cc/Cc/User/index.html
  href="../../../a1/Lib/Foo/index.html#type-t"
