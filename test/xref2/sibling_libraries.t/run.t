Two libraries of one package define modules with the same names, as
eliom.client and eliom.server do. Each has a main module [Content], a
[Content__Html] canonically named [Content.Html], and an alias [Wrap.C]
compiled with [-no-alias-deps], so [Wrap] does not import [Content].

This test is for issue #1450. The text says what each result should be, and
what would be wrong.

It tests rules P2 (doc/scope_model.mld, section 3.2), R3 (section 4.2), and
S1 and S2 (section 5). Λc(u) is the set of libraries given with [-L] when the
unit [u] was compiled, and Λl the set given to [odoc link] (section 1). Each
library is compiled with [-L] for itself only, so no Λc holds both modules
[Content], and D3 (section 2) holds.

P2 looks for the target of an alias with no digest in Λc(o), where [o] is
the unit the alias was written in. So each alias should mean a unit of its
own library. R3 looks for the root of a reference in Λl, which holds both
libraries. A canonical tag is a reference too (S1). So a reference or a
canonical tag whose root is a plain [Content] is ambiguous. It should not
resolve, and odoc should warn (section 4.5). A canonical tag that does not
resolve leaves the path shown as it is (S2). The author removes the
ambiguity by naming the library, as in [{!/client/Content}]. A canonical tag
can name its library in the same way (S1), but dune does not write tags like
that yet.

The source files:

  $ cat client/content.mli
  (** The client content. Cf. {!Content.Html}. *)
  
  module Html = Content__Html
  $ cat client/content__Html.mli
  (** @canonical Content.Html *)
  
  type t
  
  val client_only : t
  $ cat client/wrap.mli
  module C = Content
  $ cat server/content.mli
  (** The server content. Cf. {!Content.Html}. *)
  
  module Html = Content__Html
  $ cat server/content__Html.mli
  (** @canonical Content.Html *)
  
  type t
  
  val server_only : t
  $ cat server/wrap.mli
  module C = Content

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     ocamlc -bin-annot -no-alias-deps -I $lib -c $lib/$m.mli
  >   done
  > done

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     odoc compile --output-dir h --parent-id pkg/$lib -L $lib:h/pkg/$lib $lib/$m.cmti
  >   done
  > done

Each unit is linked with both libraries in Λl, and with [-I] for its own
library only.

Linking each [Content] should warn: its alias [Html] is shown through the
canonical tag [Content.Html] of [Content__Html], whose root [Content] is
ambiguous. The aliases [Html] and [C] should not be ambiguous.

Linking with no warning would be wrong.

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

Each line of the output below lists the resolved roots named in one unit,
as [parent/Unit], with no repeats, where [parent] is the last part of the
unit's [--parent-id], here its library. Through its aliases, each unit should
name only units of its own library. The root of a canonical tag should not
resolve, so it should add nothing.

Each [Content] also has the comment [{!Content.Html}], written inside
[Content], where [Content] is ambiguous in Λl. By R3, a reference whose
origin is a unit named [Content] means that unit by [Content], so its root
should add the unit's own [Content]. The other library's [Content] would be
wrong.

- The client's [Content] should name [client/Content] and
[client/Content__Html].
- The client's [Wrap] should name [client/Content] and [client/Wrap].
- The server's [Content] should name [server/Content] and
[server/Content__Html].
- The server's [Wrap] should name [server/Content] and [server/Wrap].

These lists cannot show a canonical tag that resolves to the unit being
linked, because it adds no new name.

  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/pkg/client/content.odocl
  ["client/Content","server/Content__Html"]
  $ roots h/pkg/client/wrap.odocl
  ["client/Wrap","server/Content"]
  $ roots h/pkg/server/content.odocl
  ["server/Content","server/Content__Html"]
  $ roots h/pkg/server/wrap.odocl
  ["server/Content","server/Wrap"]

A page of the package rather than of one library. Pages are out of the
spec's scope, but the references on this page name modules, so
R3 applies. The page names a root module of each library, one by plain name,
and a submodule of each. Its module list names the [Content] of each
library.

Linking the page should warn for each lookup of [Content] that does not name
a library and whose origin is not a unit named [Content]: the plain
[{!Content}], and the roots of the canonical paths. A warning for the
paragraphs in the module list would be wrong (see below).

  $ cat all.mld
  {0 Both libraries}
  
  Named one at a time: {!/client/Content} and {!/server/Content}.
  
  By plain name: {!Content}.
  
  A submodule of each: {!/client/Content.Html} and {!/server/Content.Html}.
  
  {!modules: /client/Content /server/Content}
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
  File "Content__Html":
  Ambiguous lookup. Possible units: pkg/client/Content__Html, pkg/server/Content__Html
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

The third is the plain [{!Content}], which is ambiguous. It should not
resolve.

Resolving it to either library's [Content] would be wrong. odoc warns and
takes one (section 11, "Ambiguous names").

The fourth is [{!/client/Content.Html}]. It should resolve to
[pkg/client/Content__Html]: projection has reached the client's [Content],
and the alias [Html] in it follows P2 over the client's Λc. Its canonical
tag should not resolve, so it should be shown as it is.

A resolved canonical tag, to either library's [Content.Html], would be
wrong.

The fifth is [{!/server/Content.Html}]. It should resolve to
[pkg/server/Content__Html]. Its canonical tag should not resolve either.

A resolved canonical tag would be wrong here too.

The last entries are the module list. Each entry should be the [Content] it
names, the client's and then the server's. Below each entry is the first
paragraph of that [Content], which refers to [{!Content.Html}]. The
paragraph keeps the origin of the comment it comes from, that [Content]
(section 4.1). By R3, a reference whose origin is a unit named [Content]
means that unit. So the client's paragraph should mean the client's
[Content.Html], and the server's paragraph the server's, with no warning.
odoc resolves the paragraph again on the page, with the page's Λl, which is
the R0 gap of section 11.

Resolving the client's paragraph to the server's [Content.Html] would be
wrong.

  $ odoc_print --refs --long-paths --show-root-parents h/pkg/page-all.odocl
  r(root(pkg/client/Content))
  r(root(pkg/server/Content))
  r(root(pkg/server/Content))
  r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/server/Content).Html)),root(pkg/client/Content).Html))
  r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/server/Content).Html)),root(pkg/server/Content).Html))
  modules: r(root(pkg/client/Content))
    r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/server/Content).Html)),root(pkg/server/Content).Html))
  modules: r(root(pkg/server/Content))
    r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/server/Content).Html)),root(pkg/server/Content).Html))
