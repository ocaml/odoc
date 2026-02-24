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

B should remain unresolved in A's imports, since it had no digest:

  $ odoc_print a.odoc | grep 'Unresolved.*"B"'
      { "Unresolved": [ "B", "None" ] },

link-deps should not report B as a dependency:

  $ odoc link-deps . | grep B
  [1]
