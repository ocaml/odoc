# Roadmap

As of 2022, the lack of access to great documentation for OCaml packages is one of the biggest pain points [reported](https://ocaml-sf.org/docs/2022/ocaml-user-survey-2022.pdf) by the OCaml community.

To address this, our current focus is on empowering OCaml developers to create high-quality documentation for their packages.

Now that the documentation of OCaml packages is [readily available on OCaml.org](https://ocaml.org/packages), we want to make writing documentation rewarding and straightforward. We’re working towards making Odoc suitable to create manuals by adding new features, improving the navigation, and expanding the odoc markup syntax to support rich content such as tables, images and graphs.
To prioritize our work, we look at OCaml projects that rely on other documentation generators:

- The OCaml Manual
- Dune's documentation
- The Ocsigen projects documentation

In collaboration with the maintainers of these projects, we identify the blockers they have to migrate to odoc and scope the necessary features and bug fixes.

Notable new features on our roadmap include:

- Adding a search bar to the HTML backend
- Adding a global navigation bar that contains the package API and manual pages
- Generation of an index page
- Add a special syntax for images and assets
