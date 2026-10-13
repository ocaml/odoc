Two libraries of one package define modules with the same names, as
eliom.client and eliom.server do. Each has a main module [Content], a
[Content__Html] canonically named [Content.Html], and an alias [Wrap.C]
compiled with [-no-alias-deps], so [Wrap] does not import [Content].

This test records what odoc does today, for issue #1450. The text says what
each result should be. A FIXME says where odoc gets it wrong today.

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     ocamlc -bin-annot -no-alias-deps -I $lib -c $lib/$m.mli
  >   done
  > done

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     odoc compile --output-dir h --parent-id pkg/$lib -I h/pkg/$lib $lib/$m.cmti
  >   done
  > done

Both libraries are in the reference scope of every unit, with [-L]. The
[-I] path of a unit holds only its own library, as the compiler's did.

  $ L="-L client:h/pkg/client -L server:h/pkg/server"
  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     odoc link -I h/pkg/$lib $L h/pkg/$lib/$m.odoc
  >   done
  > done
  File "Content__Html":
  Ambiguous lookup. Possible units: pkg/client/Content__Html, pkg/server/Content__Html
  File "Content":
  Ambiguous lookup. Possible units: pkg/client/Content, pkg/server/Content
  File "Content__Html":
  Ambiguous lookup. Possible units: pkg/client/Content__Html, pkg/server/Content__Html
  File "Content":
  Ambiguous lookup. Possible units: pkg/client/Content, pkg/server/Content

Each line of the output below lists the root modules named in one unit, as
[library/Unit]. Each unit should name only units of its own library.

- The client's [Content] should name [client/Content] and
[client/Content__Html], through the alias [Html] and the canonical path
written on it.
- The client's [Wrap] should name [client/Content] and [client/Wrap], through
the alias [C].
- The server's [Content] should name [server/Content] and
[server/Content__Html].
- The server's [Wrap] should name [server/Content] and [server/Wrap].

FIXME: the client's two units name the server's units. Neither name is among
the imports of the unit that mentions it, so no digest tells the two
libraries apart, and the search covers the [-L] directories as well as the
[-I] ones. The last directory searched wins, so the server's units are right
only by chance.

  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/pkg/client/content.odocl
  ["client/Content","server/Content__Html"]
  $ roots h/pkg/client/wrap.odocl
  ["client/Wrap","server/Content"]
  $ roots h/pkg/server/content.odocl
  ["server/Content","server/Content__Html"]
  $ roots h/pkg/server/wrap.odocl
  ["server/Content","server/Wrap"]

A page of the package rather than of one library. It names a root module of
each library, one by plain name, and a submodule of each.

  $ odoc compile --output-dir h --parent-id pkg all.mld
  $ odoc link -P pkg:h/pkg $L h/pkg/page-all.odoc
  File "Content":
  Ambiguous lookup. Possible units: pkg/client/Content, pkg/server/Content
  File "Content__Html":
  Ambiguous lookup. Possible units: pkg/client/Content__Html, pkg/server/Content__Html
  File "Content":
  Ambiguous lookup. Possible units: pkg/client/Content, pkg/server/Content
  File "Content":
  Ambiguous lookup. Possible units: pkg/client/Content, pkg/server/Content
  File "Content":
  Ambiguous lookup. Possible units: pkg/client/Content, pkg/server/Content
  File "Content__Html":
  Ambiguous lookup. Possible units: pkg/client/Content__Html, pkg/server/Content__Html
  File "Content":
  Ambiguous lookup. Possible units: pkg/client/Content, pkg/server/Content

How to read [odoc_print --refs --long-paths --show-root-parents]. Each line
is one reference. [r(...)] is a resolved reference, and [unresolvedroot(...)]
is one that did not resolve. [root(pkg/client/Content)] is the unit [Content]
of the library [client] in the package [pkg]. [alias(A,B)] is a reference
written through the alias [B], which resolves to [A].
[canonical(hidden(X),resolved(Y))] is the hidden unit [X], shown by its
canonical name [Y].

The lines below are the references on the page, in the order they are
written.

The first two name a root module one library at a time, with a path
reference. They should resolve to [pkg/client/Content] and
[pkg/server/Content].

The third is the plain [{!Content}], which is ambiguous. odoc should warn, and
take one of the two.

The fourth is [{!/client/Content.Html}]. It should resolve to
[pkg/client/Content__Html], with the canonical name [pkg/client/Content.Html].
FIXME: it resolves to the server's [Content__Html]. Naming the library covers
the root and no more. [Content.Html] is an alias, so odoc resolves
[Content__Html] by name once the root is found, and a page about two
libraries has no [-I] to answer that.

The fifth is [{!/server/Content.Html}]. It should resolve to
[pkg/server/Content__Html], with the canonical name [pkg/server/Content.Html].

The last entry is the module list [{!modules: Content}]. Its [Content] is
ambiguous, like the third reference. Below it is the first paragraph of that
[Content], which refers to [Content.Html]. That reference should resolve to
the [Content__Html] and [Content.Html] of the same library as the entry. The
paragraph is resolved as the page is linked, not as the module was, so this
holds only while the page's lookups find the same library as the entry's.

  $ odoc_print --refs --long-paths --show-root-parents h/pkg/page-all.odocl
  r(root(pkg/client/Content))
  r(root(pkg/server/Content))
  r(root(pkg/server/Content))
  r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/server/Content).Html)),root(pkg/client/Content).Html))
  r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/server/Content).Html)),root(pkg/server/Content).Html))
  modules: r(root(pkg/server/Content))
    r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/server/Content).Html)),root(pkg/server/Content).Html))
