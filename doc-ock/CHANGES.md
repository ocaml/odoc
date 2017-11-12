v1.1.1
------

- minimize substs in paths.
  i.e. instead of having (Foo.Bar.Baz.t)/(Toto.Tata.Baz.t) we now have
  ((Foo.Bar)/(Toto.Tata)).Baz.t

- remember sections titles to be able to splice them when referencing sections.
  (ocaml-doc/odoc#37)

v1.1.0
-------

- switch build to jbuilder.

- add support for OCaml 4.04 and 4.05.

- nicer handling of canonical modules using "display types".

- ad hoc handling of undocumented module aliases: we retrieve the synopsis of
  the module which is aliased and use that to document the binding.


v1.0.0
-------

Initial release.
