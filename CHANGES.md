v1.3.0
------

Additions

- Reason syntax output (#156, Patrick Stapfer).
- BuckleScript support (#179, Leandro Ostera).
- New CSS, appearance (#139, Rizo Isrof).
- Table of contents for the sections in each page (fe26392).
- Navigation breadcrumbs, and limit length of module paths in page
  titles (#150, Yotam Barnoy).
- Syntax highlighting of code blocks (99f2be9).
- Compiled `odoc` binary is now self-contained and requires no external
  files (bd3b53c).
- `--theme-uri` option (#154, Rizo Isrof).
- Option to convert `.mld` to HTML fragments rather than complete pages
  (#166, Rizo Isrof).

Bugs fixed

- Use regular dashes in arrows to support ligature fonts (#180, Leandro
  Ostera).
- Do not excessively indent code blocks (#133, Bobby Priambodo).
- Always prepend `page-` to output file name when compiling `.mld` files
  (#183, Rizo Isrof).
- Support `floatarray` type introduced in OCaml 4.06 (eb36158, Thomas
  Refis).
- Support destructive type substitution (57cbb4e, Thomas Refis).
- Render `<i>` tags in italics (#104, Thibault Suzanne).
- Flush HTML output correctly (#167, Rizo Isrof).
- Make HTML output more valid (#185, Leandro Ostera).
- Various improvements to parsing, output, documentation, and the
  development workflow (Yotam Barnoy, Luke Czyszczonik, Mohamed
  Elsharnouby, Rudi Grinberg, Rizo Isrof, Leandro Ostera, Bobby
  Priambodo, Thomas Refis, Patrick Stapfer).

Build and development

- `odoc` is now one repo.
- Dropped several dependencies.
- Considerable refactoring.
- New commnt parser (78a6699).
- Improved development workflow, including `CONTRIBUTING.md`, tests,
  coverage analysis, CI, and issue organization.
- Initial NPM packaging (Leandro Ostera, #214).
- Skeleton of odoc manual (Leandro Ostera, #203).

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
