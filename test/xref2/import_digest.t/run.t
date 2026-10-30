Two directories each have a module [Inner], with different interfaces. [Z] is
compiled against the one in [x], and its imports record that [Inner]'s digest.

  $ (cd x && ocamlc -bin-annot -c inner.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -I ../x -c z.mli)

  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id x x/inner.cmti
  $ odoc compile --output-dir h --parent-id y y/inner.cmti

Both directories are on the search path when [Z] is compiled. The import is
resolved to the [Inner] with the digest [Z] records, which is [x]'s, though
[y]'s is found first.

  $ odoc compile --output-dir h --parent-id z -I h/x -I h/y z/z.cmti
  $ odoc_print h/z/z.odoc | jq -c '[.imports[] | select(.Resolved)]'
  [{"Resolved":["<root>","Inner"]}]

Link uses the unit the import was resolved to, and finds [Inner.t].

  $ odoc link h/z/z.odoc -I h/x -I h/y

With only [y] on the search path, no [Inner] has the digest [Z] records, and
the import is left unresolved.

  $ odoc compile --output-dir h --parent-id z -I h/y z/z.cmti
  $ odoc_print h/z/z.odoc | jq -c '[.imports[] | select(.Resolved)]'
  []
