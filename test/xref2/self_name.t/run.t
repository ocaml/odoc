Two libraries each have a module [Content] and a [Content__Html] canonically
named [Content.Html], as in [sibling_libraries.t]. The client's [Content]
refers to the server's [Content.Html] with a path reference, which says which
library it means.

  $ for lib in client server; do
  >   for m in content__Html content; do
  >     ocamlc -bin-annot -no-alias-deps -I $lib -c $lib/$m.mli
  >   done
  > done
  $ for lib in client server; do
  >   for m in content__Html content; do
  >     odoc compile --output-dir h --parent-id pkg/$lib -I h/pkg/$lib $lib/$m.cmti
  >   done
  > done

The client's own alias [Html] is looked up by name along the whole search
path, which has the [Content__Html] of both libraries. So [odoc link] should
warn once that the lookup is ambiguous. FIXME: the warning is printed twice.

  $ odoc link -I h/pkg/client -L client:h/pkg/client -L server:h/pkg/server h/pkg/client/content.odoc
  File "Content__Html":
  Ambiguous lookup. Possible units: pkg/client/Content__Html, pkg/server/Content__Html
  File "Content__Html":
  Ambiguous lookup. Possible units: pkg/client/Content__Html, pkg/server/Content__Html

How to read [odoc_print --refs --long-paths --show-root-parents]. Each line
is one reference. [r(...)] is a resolved reference, and [unresolvedroot(...)]
is one that did not resolve. [root(pkg/client/Content)] is the unit [Content]
of the library [client] in the package [pkg]. [alias(A,B)] is a reference
written through the alias [B], which resolves to [A].
[canonical(hidden(X),resolved(Y))] is the hidden unit [X], shown by its
canonical name [Y].

The reference should resolve to the server's [Content__Html], with the
canonical name [Html] in the server's [Content]. Every root in it should be in
[pkg/server].

FIXME: the canonical name is taken to be the client's. While the client's
[Content] is linked, a lookup of the name [Content] finds only the unit being
linked. The result also depends on the order of the directories. If the
server's directory sorts before [client], the reference lands on the client's
[Content__Html].

  $ odoc_print --refs --long-paths --show-root-parents h/pkg/client/content.odocl
  r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/client/Content).Html)),root(pkg/server/Content).Html))

A library's own canonical path means that library's module. Here the units
are compiled without [-no-alias-deps], as dune compiles them. The client's
alias [Html] names its own [Content__Html], and its canonical path
[Content.Html] means the client's [Content]. The server's [Content] is also on
the search path.

  $ mkdir own && cp -RL client server own && cd own
  $ for lib in client server; do
  >   for m in content__Html content; do
  >     ocamlc -bin-annot -I $lib -c $lib/$m.mli
  >   done
  > done
  $ for lib in client server; do
  >   for m in content__Html content; do
  >     odoc compile --output-dir h --parent-id pkg/$lib -I h/pkg/$lib $lib/$m.cmti
  >   done
  > done
  $ odoc link -I h/pkg/client -L client:h/pkg/client -L server:h/pkg/server h/pkg/client/content.odoc

The first line of the output below is the module, and the second line is the
alias it is defined as, in the notation above. The alias should resolve to the
client's [Content__Html], with the canonical name [Html] in the client's
[Content]. Both roots should be in [pkg/client].

  $ odoc_print --short --long-paths --show-root-parents -r Html h/pkg/client/content.odocl
  module root(pkg/client/Content).Html =
    resolved(canonical(hidden(root(pkg/client/Content__Html)),resolved(root(pkg/client/Content).Html)))
    
  $ cd ..

A page can refer to itself by name. Two packages each have a page [index],
and each page refers to [page-index]. The search path has both packages, and
the other package comes last.

  $ for p in pa pb; do
  >   odoc compile --output-dir h --parent-id $p $p/index.mld
  > done

The reference in [pa]'s page should go to that page, [pa/index]. A page that
uses its own name means itself, whatever other pages of that name the search
path has.

  $ odoc link -I h/pa -I h/pb h/pa/page-index.odoc
  $ odoc_print --refs --show-root-parents h/pa/page-index.odocl
  r(pa/index)
