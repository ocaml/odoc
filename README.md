Doc-ock — Extract documentation from OCaml files
------------------------------------------------
%%VERSION%%

Doc-ock is a library extract documentation from OCaml files

## Installation

Doc-ock can be installed with `opam`:

    opam install doc-ock

If you don't use `opam` consult the [`opam`](opam) file for build instructions.

## Testing

The package compiles and installs the interface
[`test/ocamlary.mli`](test/ocamlary.mli) in the package's `lib`
directory along the library. This allows documentation consumers and
renderers to exercise a common and tricky selection of documentation
features packages may use.
