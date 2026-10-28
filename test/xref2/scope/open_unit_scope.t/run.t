Rule R2 (doc/scope_model.mld, section 4.2), then projection (section 3.2),
where each path met follows rule P2 with its own origin. R2 looks up the
first part of a reference in the modules given to [odoc link] with [--open],
and finds those modules as roots by rule R3. The origin of a root is the
unit whose source it came from (section 3.1). P2 looks for a root with no
digest among the libraries its origin was compiled with, Λc(o). Λc(u) is the
set of libraries given with [-L] when the unit [u] was compiled (section 1).

Library [x] has a module [Inner] and a module [Op] that aliases it as [H],
compiled with [-no-alias-deps], so [Op] does not import [Inner] with a digest.
Library [y] has a different module [Inner]. Library [z] does not depend on [x]
at all: its link opens [Op] with [--open], and it refers to [H.t] in a
comment. No unit's Λc holds two modules [Inner], so D3 (section 2) holds.
[y] is in Λc(z) although [z] does not use it, which D3 allows. The setup is
made to isolate R2: dune passes no [--open] to odoc, and odoc_driver passes
only [--open=""].

  $ cat x/inner.mli
  type t = int
  $ cat x/op.mli
  module H = Inner
  $ cat y/inner.mli
  type t = string
  $ cat z/z.mli
  (** See {!H.t}. *)
  type u
  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli op.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -c z.mli)

[Op] is compiled before [Inner], so the path in [H] is left for link to
resolve.

  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/inner.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/op.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/inner.cmti
  $ odoc compile --output-dir h --parent-id z -L z:h/z -L y:h/y z/z.cmti

R2 should find [H] in [Op]. Its root [Inner] has [Op] as its origin, so P2
should look for it in Λc(Op) only, not in Λc(z), and the lookup should not
be ambiguous. [H] should mean [x]'s [Inner].

The [jq] filter prints the target of each resolved alias in [z]'s linked
file, as [parent/Unit], with no repeats, where [parent] is the last part
of the unit's [--parent-id], here its library. The only alias there is [H], met
while resolving [{!H.t}].

  $ odoc link h/z/z.odoc -L z:h/z -L x:h/x -L y:h/y --open Op
  File "Inner":
  Ambiguous lookup. Possible units: x/Inner, y/Inner
  $ odoc_print h/z/z.odocl | jq -c '[.. | objects | .["`Alias"]? | select(. != null) | .[0]["`Identifier"]["`Root"] | .[0].Some["`Page"][1] + "/" + .[1]] | unique'
  ["y/Inner"]
