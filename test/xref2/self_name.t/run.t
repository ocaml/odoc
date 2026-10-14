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
[Content.Html] means the server's [Content], and that is what it is taken to
be. The unit being linked stands for its own file but does not hide the other
[Content], so a name looked up along the whole search path has two answers
and odoc warns: once for the client's own alias [Html], and once for the
canonical path's root.

  $ odoc link -I h/pkg/client -L client:h/pkg/client -L server:h/pkg/server h/pkg/client/content.odoc
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  $ refs h/pkg/client/content.odocl
  ["server/Content server/Content__Html"]
