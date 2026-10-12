Libraries [x] and [y] each have a unit [Inner], with different interfaces.
[Z] is compiled against [x]'s, so its imports record the digest of [x]'s
[Inner].

  $ cat x/inner.mli
  type t
  $ cat y/inner.mli
  type t
  val extra : t
  $ cat z/z.mli
  type z = Inner.t
  $ (cd x && ocamlc -bin-annot -c inner.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -I ../x -c z.mli)
  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id x x/inner.cmti
  $ odoc compile --output-dir h --parent-id y y/inner.cmti

[odoc_print --short --show-root-parents] shows a resolved path with the library
of the unit it points to, such as [x/Inner.t]. An unresolved path has no
library: [Inner.t].

Given both directories, [odoc compile] should take the [Inner] with the digest
[Z] recorded, whichever directory comes first: [z] should be [x/Inner.t].
[y/Inner.t] would be wrong, because [Z] was not compiled against it.

  $ odoc compile --output-dir h --parent-id z -I h/x -I h/y z/z.cmti
  $ odoc_print --short --show-root-parents h/z/z.odoc
  type z = x/Inner.t
  $ odoc compile --output-dir h --parent-id z -I h/y -I h/x z/z.cmti
  $ odoc_print --short --show-root-parents h/z/z.odoc
  type z = x/Inner.t

Given only [y]'s directory, no [Inner] has that digest, so [z] should stay
unresolved: [Inner.t]. [y/Inner.t] would be wrong.

  $ odoc compile --output-dir h --parent-id z -I h/y z/z.cmti
  $ odoc_print --short --show-root-parents h/z/z.odoc
  type z = Inner.t

With [--long-paths], [odoc_print] also says whether the path was resolved. It
should be [unresolvedroot(Inner).t]; [resolved(root(y/Inner).t)] would be
wrong. ([z/0] is the type's identifier.)

  $ odoc_print --short --show-root-parents --long-paths h/z/z.odoc
  type z/0 = unresolvedroot(Inner).t
