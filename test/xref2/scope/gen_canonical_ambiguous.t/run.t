Rules S1 and S2 (section 5), with R3 (section 4.2): a [@canonical] tag is a
reference. If its root has two candidates, the tag is ambiguous: odoc should
warn, and show the path as it is (S2).

Libraries [p] and [q] each have a unit [Gamma] with a module [Impl]. [p]'s
[Impl] has [module type S], and [q]'s has only [type u]. Neither library
depends on the other, so no unit's Λc holds two [Gamma]s (D3). Library [r]
is built against [p]. Its unit [User] has a module [I] with
[module type S] and the tag [@canonical Gamma.Impl], and [module M : I.S].
[r] has no unit [Gamma]. Every compile uses dune's [-no-alias-deps], and
[-L] for the unit's own library and its dependencies.

  $ cat p/gamma.mli
  module Impl : sig
    module type S = sig end
  end
  $ cat q/gamma.mli
  module Impl : sig
    type u
  end
  $ cat r/user.mli
  (** @canonical Gamma.Impl *)
  module I : sig
    module type S = sig end
  end
  
  module M : I.S
  $ for l in p q; do
  >   ocamlc -bin-annot -no-alias-deps -c $l/gamma.mli
  >   odoc compile --output-dir h --parent-id pkg/$l -L $l:h/pkg/$l $l/gamma.cmti
  > done
  $ ocamlc -bin-annot -no-alias-deps -I p -c r/user.mli
  $ odoc compile --output-dir h --parent-id pkg/r -L r:h/pkg/r -L p:h/pkg/p r/user.cmti

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [M/0] is the item [M], with a number odoc adds
to tell apart items of the same name; [I/1] is [User]'s own [I]. [r(...)] is
a resolved module type path, and [root(pkg/p/Gamma)] is the unit [Gamma] of
the library [p] in the package [pkg]. [canonical(a,resolved(b))] is a path
[a] whose canonical path is [b].

Every library is given at link. The tag [Gamma.Impl] has two candidates in
Λl, [p]'s [Gamma] and [q]'s [Gamma]. odoc should warn that it is ambiguous.
[I.S] should then be shown as it is, with no canonical path, and link to
[User]'s own page for [S] ([I/module-type-S/index.html]).

A canonical path to either library's [Gamma.Impl] would be wrong, even one
that links to a page that exists. So would a link with no warning.

  $ odoc link -L p:h/pkg/p -L q:h/pkg/q -L r:h/pkg/r h/pkg/r/user.odoc
  File "Gamma":
  Ambiguous lookup. Possible units: pkg/p/Gamma, pkg/q/Gamma
  $ odoc_print --short --long-paths --show-root-parents h/pkg/r/user.odocl
  module I/1 : sig module type S/2 = sig end end
  module M/0 : r(canonical(I/1,resolved(root(pkg/q/Gamma).Impl)).S) 
  $ odoc html-generate -o html h/pkg/r/user.odocl
  $ grep -o 'href="[^"]*module-type-S[^"]*"' html/pkg/r/User/index.html
  href="../../q/Gamma/Impl/module-type-S/index.html"
