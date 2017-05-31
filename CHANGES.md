v1.1.1
-------

- add word-joiners between the question mark and the identifier of optional
  labels
  cf. ocaml-doc/odoc#19

- fix handling of stop comments

- put id on nav
  cf. ocaml-doc/odoc#80


v1.1.0
-------

- class types: fix rendering, they were previously rendered as classes.
  cf. ocaml-doc/odoc#72

- place docstring above include not below

- handle substitutions on first class modules
  cf. ocaml-doc/odoc#75

- switch to jbuilder

- use "display type" to render canonical modules as "module Foo : sig .. end"
  instead of "module Foo = Foo" (which was confusing)

v1.0.0
-------

Initial release.
