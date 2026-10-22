Two libraries [a] and [b] each have a unit [X] with an alias [Y] to a unit
[X__y], canonically named [X.Y]. The units of [a] have the parent page
[pkg/a], and those of [b] have [pkg/b].

  $ cat a/x.mli a/x__y.mli
  module Y = X__y
  (** @canonical X.Y *)
  
  type t
  $ for l in a b; do
  >   for m in x__y x; do
  >     ocamlc -bin-annot -I $l -c $l/$m.mli
  >     odoc compile --output-dir h --parent-id pkg/$l -I h/pkg/$l $l/$m.cmti
  >   done
  > done

Both libraries are on the search path when [a]'s [X] is linked.

  $ odoc link -I h/pkg/a -I h/pkg/b h/pkg/a/x.odoc

By default, a root module is printed with its name only. The output does not
say which library's [X__y] the alias resolves to.

  $ odoc_print --short --long-paths -r Y h/pkg/a/x.odocl
  module root(X).Y =
    resolved(canonical(hidden(root(X__y)),resolved(root(X).Y)))
    

With [--show-root-parents], the parent pages of each root are printed too.
They show that the alias and its canonical path are both in [a].

  $ odoc_print --short --long-paths --show-root-parents -r Y h/pkg/a/x.odocl
  module root(pkg/a/X).Y =
    resolved(canonical(hidden(root(pkg/a/X__y)),resolved(root(pkg/a/X).Y)))
    

Without [--long-paths], the alias prints as its canonical path. So only the
parent pages of the canonical path are shown.

  $ odoc_print --short --show-root-parents -r Y h/pkg/a/x.odocl
  module pkg/a/X.Y =
    pkg/a/X.Y
    

A unit compiled without [--parent-id] has no parent page, and prints with its
name only.

  $ ocamlc -bin-annot -c c/z.mli
  $ odoc compile -o c/z.odoc c/z.cmti
  $ odoc link c/z.odoc
  $ odoc_print --short --long-paths --show-root-parents -r t c/z.odocl; echo
  type root(Z).t 
