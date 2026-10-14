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

  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }

The reference reaches the server's [Content__Html], whose canonical path
[Content.Html] means the server's [Content]. While the client's [Content] is
being linked, though, looking up the name [Content] finds only the unit being
linked, so the canonical path is taken to be the client's: [client/Content]
appears among the reference's roots, which is wrong. The warning is about the
client's own alias [Html], which is looked up by name along the whole search
path.

  $ odoc link -I h/pkg/client -L client:h/pkg/client -L server:h/pkg/server h/pkg/client/content.odoc
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  $ refs h/pkg/client/content.odocl
  ["client/Content server/Content server/Content__Html"]
