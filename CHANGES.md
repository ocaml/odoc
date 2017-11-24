v1.2.0
------

- Support for standalone documentation pages (`.mld` files) (#61).
- Display `[@@deprecated]` attributes as the `@deprecated` tag (#57).
- Allow each component of OCaml paths to be disambiguated using the `kind-identifer` syntax (part of #61).
- Support OCaml 4.06.
- Fix spurious leading blank lines in verbatim text (ocaml-doc/octavius#6).

v1.1.1
-------

- make odoc more noisy when generating html for hidden units

- changed `html-deps` subcommand behavior: it now expects to be given a
  directory, not a single odoc file.

v1.1.0
-------

- switch to jbuilder

v1.0.0
-------

Initial release.
