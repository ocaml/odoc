Rule R3 (doc/scope_model.mld, section 4.2), then projection (section 3.2),
where each path met follows rule P2 with its own origin. The origin of a
root is the unit whose source it came from (section 3.1). P2 looks for a root
with no digest among the libraries its origin was compiled with, Λc(o), and
never among the libraries given only at link (section 8). Λc(u) is the set
of libraries given with [-L] when the unit [u] was compiled (section 1).

Library [x] has a module [Inner] and a main module [X], compiled with
[-no-alias-deps], whose module type [S] and module [H] both alias [Inner].
Library [y] has a different module [Inner]. No unit depends on both, so D3
(section 2) holds. The [H] in [X.S] was written in [X], like [X.H], and
Λc(X) holds only [x]'s [Inner].

  $ cat x/inner.mli
  type t
  $ cat x/x.mli
  module type S = sig
    module H = Inner
  end
  
  module H = Inner
  $ cat y/inner.mli
  type u
  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli x.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/inner.cmti
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/x.cmti
  $ odoc compile --output-dir h --parent-id pkg/y -L y:h/pkg/y y/inner.cmti

How to read [odoc_print --refs --long-paths --show-root-parents]. Each line
is one reference. [r(...)] is a resolved reference. [root(pkg/x/A)] is the unit [A] of the library
[x] in the package [pkg]. [alias(A,B)] is a reference written through the
alias [B], which resolves to [A]. [canonical(hidden(X),resolved(Y))] is the
hidden unit [X], shown by its canonical name [Y].

A page refers to [H] through the module type and through the module. Pages
are out of the spec's scope, but these references name modules.
Both libraries are given at link. Both references should reach
[pkg/x/Inner], and neither lookup should be ambiguous.

  $ cat p.mld
  {0 Page}
  
  Through the module type: {!/x/X.S.H}. Through the module: {!/x/X.H}.
  $ odoc compile --output-dir h --parent-id pkg p.mld
  $ odoc link -P pkg:h/pkg -L x:h/pkg/x -L y:h/pkg/y h/pkg/page-p.odoc
  File "Inner":
  Ambiguous lookup. Possible units: pkg/x/Inner, pkg/y/Inner
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/page-p.odocl
  r(alias(root(pkg/y/Inner),root(pkg/x/X).S.H))
  r(alias(root(pkg/y/Inner),root(pkg/x/X).H))
