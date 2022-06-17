# odoc &nbsp; [![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml%2Fodoc%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/ocaml/odoc) [![Coverage Status](https://coveralls.io/repos/github/ocaml/odoc/badge.svg)](https://coveralls.io/github/ocaml/odoc)

**odoc** is a documentation generator for OCaml. It reads *doc comments* ,
delimited with `(** ... *)`, and outputs HTML, LaTeX and man pages.

## Using `odoc` with OCaml

To install odoc with opam:

```
$ opam install odoc
```

For more information, see the [odoc website](https://ocaml.github.io/odoc) or build
the docs locally yourself from the `odoc` directory:

```
git clone https://github.com/ocaml/odoc.git
cd odoc
opam pin add . -n
opam install --deps-only -t odoc
dune build @docgen
```

and find the docs in `_build/default/doc/html/index.html`

## Contact

odoc is most discussed on [discuss.ocaml.org's Ecosystem category][discourse] with the `odoc` tag.
Please also don't hesitate to [open an issue][issues].

<br/>

## Contributing

Any [question asked](#contact), [issue opened][issues], feedback offered, is a
contribution to odoc, and the project and all its users are thankful :) If
you'd like to contribute code specifically, you may find the guide in
[`doc/contributing.mld`][contributing.mld] helpful.

[discourse]: https://discuss.ocaml.org/c/eco
[issues]: https://github.com/ocaml/odoc/issues/new
[contributing.mld]: https://ocaml.github.io/odoc/contributing.html
