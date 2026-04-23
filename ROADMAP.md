# Roadmap

As of 2022, the lack of access to great documentation for OCaml packages is one of the biggest pain points [reported](https://ocaml-sf.org/docs/2022/ocaml-user-survey-2022.pdf) by the OCaml community.

To address this, our current focus is on empowering OCaml developers to create high-quality documentation for their packages.

Now that the documentation of OCaml packages is [readily available on OCaml.org](https://ocaml.org/packages), we want to make writing documentation rewarding and straightforward. We're working towards making Odoc suitable to create manuals by adding new features, improving the navigation, and expanding the odoc markup syntax to support rich content such as tables, images and graphs.

To prioritize our work, we look at OCaml projects that rely on other documentation generators:

- The OCaml Manual
- Dune's documentation
- The Ocsigen projects documentation

In collaboration with the maintainers of these projects, we identify the blockers they have to migrate to odoc and scope the necessary features and bug fixes.

The items below are listed roughly in the order we intend to tackle them.

## Odoc

- **OxCaml support.** OxCaml is a moving target, so this will necessarily be a different 'style' of support from OCaml support. We will make no effort to support multiple versions of the OxCaml compiler, and so each release of Odoc will likely be tied to a specific release of the OxCaml compiler. The aim is to support all relevant features of the OxCaml compiler, including modes, layouts and so on as part of the documentation.
- **New markup syntax.** Adding support for several new markup features, such as: admonitions, quotes, definition lists, and details.
- **Custom tags and plugins.** Adding support for custom tags to Odoc, and a plugin mechanism so that the behaviour of these custom tags can be specified.
- **Improved source rendering.** Enhanced rendering of source pages, as well as new syntax to link directly to implementations from doc comments.

## Sherlodoc

- Support for markdown output as well as HTML.
- Support for search in documentation pages.
- Improved support for incremental building.
