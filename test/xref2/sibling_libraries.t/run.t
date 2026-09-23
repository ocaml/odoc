Two libraries of one package define modules with the same names, as
eliom.client and eliom.server do. Each has a main module [Content], a
[Content__Html] canonically named [Content.Html], and an alias [Wrap.C]
compiled with [-no-alias-deps], so [Wrap] does not import [Content].

This test records what odoc does today, for issue #1450. Several of the
results below are wrong, and the text says which.

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
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content

Which library the root modules named in each unit come from. The first two
are wrong. The client's [Content] should name its own [Content__Html], through
the alias [Html] and the canonical path written on it, and the client's [Wrap]
should name its own [Content], through the alias [C]. Both name the server's.
Neither name is among the imports of the unit that mentions it, so no digest
is there to tell the two libraries apart, and the search covers the [-L] directories as
well as the [-I] ones. The server's units are right by chance: it is the last
directory searched that wins.

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
  Ambiguous lookup. Possible files: Content
  Content
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }
Where each reference on the page points, in the order they are written. The
first two name a root module one library at a time, and a path reference says
which library it means, so both land where they were aimed. The third is the
plain [{!Content}], which is ambiguous, and odoc says so and takes one.

The fourth is [{!/client/Content.Html}], and it is wrong. Naming the library
covers the root and no more: [Content.Html] is an alias, so odoc resolves
[Content__Html] by name once the root is found, and a page about two libraries
has no [-I] to answer that. Both submodule references land on the server, so
there is nothing an author can write on this page to reach the client's.

  $ refs h/pkg/page-all.odocl
  ["client/Content","server/Content","server/Content","client/Content server/Content server/Content__Html","server/Content server/Content__Html","server/Content server/Content__Html"]
  $ modules() { odoc_print $1 | jq -c 'def libs: [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique; [.. | objects | select(has("`Modules")) | .["`Modules"][] | {shown: (.[0] | libs), says: (.[1] | libs)}]'; }
A module list shows, beside each module it names, that module's own first
paragraph. The paragraph is resolved as the page is linked, not as the module
was, so it points where the page can reach rather than where its author meant.

  $ modules h/pkg/page-all.odocl
  [{"shown":["server/Content"],"says":["server/Content","server/Content__Html"]}]
