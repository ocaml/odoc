When compiling with -no-alias-deps, module aliases produce imports without
digests. odoc should not try to resolve these imports, as looking them up in
the include path could find stale artifacts from a previous build, associating
a wrong digest and breaking incremental rebuilds.

Compile b.mli normally, and a.ml with -no-alias-deps. Module A contains
`module B = B`, so B appears as an import of A without a digest.

  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot -no-alias-deps -w -49 a.ml

Verify that B has no digest in the compiled .cmt:

  $ ocamlobjinfo a.cmt | grep "	--------------------------------	B" | head -1
  	--------------------------------	B

compile-deps correctly omits imports without digests:

  $ odoc compile-deps a.cmt | cut -d ' ' -f 1 | sort
  A
  CamlinternalFormatBasics
  Stdlib

Compile both with odoc. B's .odoc is present in the include path.

  $ odoc compile --pkg test -I . b.cmti
  $ odoc compile --pkg test -I . a.cmt

BUG: B gets spuriously resolved in A's imports despite having no digest.
This can cause stale artifact digests to be associated with the import,
breaking incremental rebuilds.

  $ odoc_print a.odoc | grep 'Resolved.*"B"'
      { "Resolved": [ "<root>", "B" ] },

link-deps reports B as a dependency with a potentially stale digest:

  $ odoc link-deps . | cut -d ' ' -f 1-2
  test B
