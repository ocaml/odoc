`include module type of M`, where `M` is a module of a wrapped library,
re-exports `M`'s module types as paths rather than expanding them. Such a
module type has no expansion of its own, so odoc generates no page for it -
and anything referring to it must therefore link to the module type that does
have one, not to the local re-export.

The library, wrapped the way Dune does it:

  $ cat a__Comparator.mli
  module type S_fc = sig
    type comparable_t
  
    val f : comparable_t -> comparable_t
  end
  
  val make : compare:('a -> 'a -> int) -> (module S_fc with type comparable_t = 'a)
  $ cat a__.ml
  (** @canonical A.Comparator *)
  module Comparator = A__Comparator
  $ cat a.ml
  open A__
  
  module Comparator = Comparator

and a consumer, mirroring `core`'s `comparator.mli`:

  $ cat b.mli
  include module type of A.Comparator
  (** @inline *)

  $ ocamlc -c -bin-annot -no-alias-deps -w -49 a__.ml
  $ ocamlc -c -bin-annot -no-alias-deps -open A__ a__Comparator.mli
  $ ocamlc -c -bin-annot -no-alias-deps -open A__ a.ml
  $ ocamlc -c -bin-annot -I . b.mli

  $ odoc compile -I . a__Comparator.cmti
  $ odoc compile -I . a__.cmt
  $ odoc compile -I . a.cmt
  $ odoc compile -I . b.cmti
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate --indent -o html b.odocl

`B.S_fc` is a re-export with no expansion, so only `A.Comparator.S_fc` gets a
page:

  $ find html -name index.html | sort
  html/A/Comparator/index.html
  html/A/Comparator/module-type-S_fc/index.html
  html/A/index.html
  html/B/index.html

Every reference to it out of `B` - the `module type` spec, and both links in
the package type of `make`, which came in through the `@inline` include - must
point at that page rather than at a `B/module-type-S_fc/` that was never
generated:

  $ grep -o 'href="[^"]*module-type-S_fc[^"]*"' html/B/index.html | sed 's/href="//;s/"$//' | sort -u
  #module-type-S_fc
  ../A/Comparator/module-type-S_fc/index.html
  ../A/Comparator/module-type-S_fc/index.html#type-comparable_t

  $ test -e html/A/Comparator/module-type-S_fc/index.html && echo "target exists"
  target exists
