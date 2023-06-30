<h1 align="center">
  <a href="https://ocaml.github.io/odoc/">
    odoc
  </a>
</h1>

<p align="center">
  <strong>OCaml Documentation Generator.</strong>
</p>

<p align="center">
  <a href="https://ocaml.ci.dev/github/ocaml/odoc">
    <img src="https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml%2Fodoc%2Fmaster&logo=ocaml" alt="OCaml-CI Build Status" />
  </a>
  <a href="https://coveralls.io/github/ocaml/odoc">
    <img src="https://coveralls.io/repos/github/ocaml/odoc/badge.svg" alt="Coverage Status" />
  </a>
</p>

**odoc** is a powerful and flexible documentation generator for OCaml. It reads *doc comments*, demarcated by `(** ... *)`, and transforms them into a variety of output formats, including HTML, LaTeX, and man pages.

- **Output Formats:** Odoc generates HTML for web browsing, LaTeX for PDF generation, and man pages for use on Unix-like systems.
- **Cross-References:** odoc uses the `ocamldoc` markup, which allows to create links for functions, types, modules, and documentation pages.
- **Link to Source Code:** Documentation generated includes links to the source code of functions, providing an easy way to navigate from the docs to the actual implementation.
- **Code Highlighting:** odoc automatically highlights syntax in code snippets for different languages.

odoc is part of the [OCaml Platform](https://ocaml.org/docs/platform), the recommended set of tools for OCaml.

## Getting Started

To begin using odoc, first install it using opam with the following command:

```
$ opam install odoc
```

Once installed, you can generate your project documentation with [Dune](https://github.com/ocaml/dune):

```
$ dune build @doc
```

Upon completion, you'll find your freshly minted docs in `_build/default/doc/html/index.html`:

```
$ open _build/default/doc/html/index.html
```

For more in-depth information and usage instructions, see the [odoc website](https://ocaml.github.io/odoc).

## Documentation

The full documentation for odoc, including comprehensive user and API guides, can be found on our [website](https://ocaml.github.io/odoc/).

## Contributing

### [Contributing Guide](CONTRIBUTING.md)

We wholeheartedly welcome contributors! To start, please read our [Contributing Guide](CONTRIBUTING.md) to familiarize yourself with our development process, including how to propose and how to start hacking on odoc.

### [Code of Conduct][coc]

In order to foster a welcoming and respectful community, odoc has adopted the [OCaml Code of Conduct](coc).

[coc]: https://ocaml.org/policies/code-of-conduct

### [Roadmap](ROADMAP.md)

Interested in the future of odoc? Take a look at our [Roadmap](ROADMAP.md) to understand our vision and planned advancements for odoc.

### [Discussions][discussions]

For conversations on ongoing development, be sure to visit the [odoc][discussions] section of the OCaml Discuss forum.

[discussions]: https://discuss.ocaml.org/tag/odoc

## License

odoc is distributed under the terms of the ISC License. See the [LICENSE](LICENSE) file for complete details.

## Acknowledgments

odoc owes its existence to the efforts of [Thomas Refis](https://github.com/trefis), [Leo White](https://github.com/lpw25), and [David Sheets](https://github.com/dsheets). The project was initiated in 2014 and 2015.

We'd also like to extend our appreciation to [Anton Bachin](https://github.com/aantron) and [Daniel Bünzli](https://github.com/dbuenzli) and [Jon Ludlam](https://github.com/jonludlam) for their pivotal contributions to odoc.

Furthermore, we express our gratitude to [Jane Street](https://www.janestreet.com/) and [Tarides](https://tarides.com/), whose funding has been critical to support the ongoing development of odoc.
