A quick test to repro the issue found in #944

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  $ odoc link foo.odoc
  File "foo.odoc":
  Warning: Failed to lookup type unresolvedroot(Stdlib).Set.Make(unresolvedroot(Stdlib).String).t Parent_module: Unresolved apply

  $ odoc html-generate --indent -o html/ foo.odocl
