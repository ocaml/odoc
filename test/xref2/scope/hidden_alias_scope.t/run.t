Rule R3 (doc/scope_model.mld, section 4.2), then projection through aliases
(section 3.2), where each path met follows rule P2 with its own origin. The
origin of a root is the unit whose source it came from (section 3.1). P2
looks for a root with no digest among the libraries its origin was compiled
with, Λc(o). Λc(u) is the set of libraries given with [-L] when the unit [u]
was compiled (section 1).

Library [l2] has a hidden module [Inner__Hidden], whose [N] aliases [A] of
library [a2]. Library [l1]'s [Outer] aliases [Inner__Hidden] as [M]. [l1] is
built against [l2] and [a2]. All are compiled with [-no-alias-deps], so
neither alias records a digest, as [entry FILE N], from [../digests.sh],
shows. Library [o] has a different [A]. No library
is built against [o], so no unit sees two [A]s (D3, section 2).

  $ cat a2/a.mli
  type t
  $ cat o/a.mli
  type u
  $ cat l2/inner__Hidden.mli
  module N = A
  $ cat l1/outer.mli
  module M = Inner__Hidden
  $ (cd a2 && ocamlc -bin-annot -c a.mli)
  $ (cd o && ocamlc -bin-annot -c a.mli)
  $ (cd l2 && ocamlc -bin-annot -no-alias-deps -I ../a2 -c inner__Hidden.mli)
  $ (cd l1 && ocamlc -bin-annot -no-alias-deps -I ../l2 -I ../a2 -c outer.mli)
  $ . ../digests.sh
  $ entry l2/inner__Hidden.cmti A
  no digest
  $ entry l1/outer.cmti Inner__Hidden
  no digest
  $ odoc compile --output-dir h --parent-id pkg/a2 -L a2:h/pkg/a2 a2/a.cmti
  $ odoc compile --output-dir h --parent-id pkg/o -L o:h/pkg/o o/a.cmti
  $ odoc compile --output-dir h --parent-id pkg/l2 -L l2:h/pkg/l2 -L a2:h/pkg/a2 l2/inner__Hidden.cmti
  $ odoc compile --output-dir h --parent-id pkg/l1 -L l1:h/pkg/l1 -L l2:h/pkg/l2 -L a2:h/pkg/a2 l1/outer.cmti

How to read [odoc_print --refs --long-paths --show-root-parents]. Each line
is one reference. [r(...)] is a resolved reference. [root(pkg/x/A)] is the unit [A] of the library
[x] in the package [pkg]. [alias(A,B)] is a reference written through the
alias [B], which resolves to [A]. [hidden(X)] is the hidden unit [X].

A page refers to [/l1/Outer.M.N]. Pages are out of the spec's scope,
but this reference names modules. Following [M] leads into
[l2]'s [Inner__Hidden], and then to the root [A] of [N], whose origin is
[Inner__Hidden]. [Inner__Hidden] is hidden, so it should not be linked to
(rule S3, section 5).

Under D3, [l1] can only see the [A] that [l2] sees, so this case cannot tell
Λc(Inner__Hidden) from Λc(Outer). What it can show is that the root keeps a
scope at all. [o] is given at link, like every library. A root that lost its
origin would be looked up among all the libraries given at link, and would
find two [A]s. So [N] should reach [a2]'s [A], with no warning.

  $ cat p.mld
  {0 Page}
  
  {!/l1/Outer.M.N}
  $ odoc compile --output-dir h --parent-id pkg p.mld
  $ odoc link -P pkg:h/pkg -L a2:h/pkg/a2 -L o:h/pkg/o -L l2:h/pkg/l2 -L l1:h/pkg/l1 h/pkg/page-p.odoc
  $ odoc_print --refs --long-paths --show-root-parents h/pkg/page-p.odocl
  r(alias(root(pkg/a2/A),alias(hidden(root(pkg/l2/Inner__Hidden)),root(pkg/l1/Outer).M).N))
