Rule R3 (doc/scope_model.mld, section 4.2), then projection through a module
type path (section 3.2, and O1 in section 7), where each path met follows
rule P2 with its own origin. The origin of a root is the unit whose source it
came from (section 3.1). P2 looks for a root with no digest among the
libraries its origin was compiled with, Λc(o). Λc(u) is the set of libraries
given with [-L] when the unit [u] was compiled (section 1).

Library [x] has a main module [X], compiled with [-no-alias-deps], whose
module type [S] aliases [Inner] of library [ix] as [H]. So [x] records no
digest for [Inner], as [entry FILE N], from [../digests.sh], shows. Library [y]'s [Y] declares [module type S = X.S]. [y] is
built against [x] and [ix]. Library [iy] has a different [Inner]. No library
is built against [iy], so no unit sees two [Inner]s (D3, section 2).

  $ cat ix/inner.mli
  type t
  $ cat iy/inner.mli
  type u
  $ cat x/x.mli
  module type S = sig
    module H = Inner
  end
  $ cat y/y.mli
  module type S = X.S
  $ (cd ix && ocamlc -bin-annot -c inner.mli)
  $ (cd iy && ocamlc -bin-annot -c inner.mli)
  $ (cd x && ocamlc -bin-annot -no-alias-deps -I ../ix -c x.mli)
  $ (cd y && ocamlc -bin-annot -no-alias-deps -I ../x -I ../ix -c y.mli)
  $ . ../digests.sh
  $ entry x/x.cmti Inner
  no digest
  $ odoc compile --output-dir h --parent-id pkg/ix -L ix:h/pkg/ix ix/inner.cmti
  $ odoc compile --output-dir h --parent-id pkg/iy -L iy:h/pkg/iy iy/inner.cmti
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x -L ix:h/pkg/ix x/x.cmti
  $ odoc compile --output-dir h --parent-id pkg/y -L y:h/pkg/y -L x:h/pkg/x -L ix:h/pkg/ix y/y.cmti

How to read [odoc_print --refs --long-paths --show-root-parents]. Each line
is one reference. [r(...)] is a resolved reference. [root(pkg/x/A)] is the unit [A] of the library
[x] in the package [pkg]. [alias(A,B)] is a reference written through the
alias [B], which resolves to [A]. [aliasmoduletype(A,B)] is the module type
[B], an alias to [A].

A page refers to [/y/Y.S.H]. Pages are out of the spec's scope,
but this reference names modules. Following [Y.S] leads to [X.S], and then
to the root [Inner] of [H], whose origin is [X].

Under D3, [y] can only see the [Inner] that [x] sees, so this case cannot
tell Λc(X) from Λc(Y). What it can show is that the root keeps a scope at
all. [iy] is given at link, like every library. A root that lost its origin
would be looked up among all the libraries given at link, and would find two
[Inner]s. So [H] should reach [ix]'s [Inner], with no warning.

  $ cat p.mld
  {0 Page}
  
  {!/y/Y.S.H}
  $ odoc compile --output-dir h --parent-id pkg p.mld
  $ odoc link -P pkg:h/pkg -L ix:h/pkg/ix -L iy:h/pkg/iy -L x:h/pkg/x -L y:h/pkg/y h/pkg/page-p.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/page-p.odocl
  r(alias(root(pkg/ix/Inner),aliasmoduletype(root(pkg/x/X).S,root(pkg/y/Y).S).H))
