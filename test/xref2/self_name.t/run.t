Two libraries each have a module [Content], a [Content__Html] canonically
named [Content.Html], and an alias [Wrap.C] to [Content], as in
[sibling_libraries.t]. The client's [Content] refers to the server's
[Content.Html] and [Wrap.C] with path references, which say which library
they mean.

It tests rules P1 and P2 (doc/scope_model.mld, section 3.2), R3 (section
4.2), and S1 and S2 (section 5). Λc(u) is the set of libraries given with
[-L] when the unit [u] was compiled, and Λl the set given to [odoc link]
(section 1). Each library is compiled with [-L] for itself only, so no Λc
holds both modules [Content], and D3 (section 2) holds.

A path is looked up in Λc(o), where [o] is the unit it was written in. For
paths, the unit being linked is not special. A reference is looked up in Λl
(R3). A canonical tag is a reference too (S1), so where its root is
ambiguous in Λl, it should not resolve, and odoc should warn (section 4.5).
The path is then shown as it is (S2).

The source files:

  $ cat client/content.mli
  (** The client content. The server's Html is {!/server/Content.Html}, and
      the server's alias to its [Content] is {!/server/Wrap.C}. *)
  
  module Html = Content__Html
  $ cat client/content__Html.mli
  (** @canonical Content.Html *)
  
  type t
  
  val client_only : t
  $ cat client/wrap.mli
  module C = Content
  $ cat server/content.mli
  (** The server content. *)
  
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

Linking should warn about each lookup of the root [Content] of a canonical
tag, which is ambiguous. The client's own alias [Html] should not be
ambiguous.

Linking with no warning would be wrong.

  $ odoc link -L client:h/pkg/client -L server:h/pkg/server h/pkg/client/content.odoc

How to read [odoc_print --refs --long-paths --show-root-parents]. Each line
is one reference. [r(...)] is a resolved reference, and [unresolvedroot(...)]
is one that did not resolve. [root(pkg/client/Content)] is the unit [Content]
of the library [client] in the package [pkg]. [alias(A,B)] is a reference
written through the alias [B], which resolves to [A].
[canonical(hidden(X),resolved(Y))] is the hidden unit [X], shown by its
canonical name [Y].

The first reference should resolve to [pkg/server/Content__Html]: R3 finds
the server's [Content], and its alias [Html] follows P2 over the server's
Λc. Its canonical tag should not resolve, so it should be shown as it is.
The second should resolve to [pkg/server/Content], the module that the
server's alias [C] names.

A resolved canonical tag, to either library's [Content.Html], would be
wrong.

  $ odoc_print --refs --long-paths --show-root-parents h/pkg/client/content.odocl
  r(alias(canonical(hidden(root(pkg/server/Content__Html)),resolved(root(pkg/client/Content).Html)),root(pkg/server/Content).Html))
  r(alias(root(pkg/server/Content),root(pkg/server/Wrap).C))

A library's own canonical tag, as dune writes it. Unlike dune, the units
here are compiled without [-no-alias-deps], so [Content] records a digest for
[Content__Html], and its alias [Html] follows P1. The alias names the
client's own [Content__Html], whose canonical tag is [Content.Html]. The
server's [Content] is also in Λl.

Linking should warn that the root [Content] of the canonical tag is
ambiguous.

Linking with no warning would be wrong.

  $ mkdir own && cp -RL client server own && cd own
  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     ocamlc -bin-annot -I $lib -c $lib/$m.mli
  >   done
  > done
  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     odoc compile --output-dir h --parent-id pkg/$lib -L $lib:h/pkg/$lib $lib/$m.cmti
  >   done
  > done
  $ odoc link -L client:h/pkg/client -L server:h/pkg/server h/pkg/client/content.odoc

The first line of the output below is the module, and the second line is the
alias it is defined as, in the notation above. The alias should resolve to
[pkg/client/Content__Html]. Its canonical tag should not resolve, so it
should be shown as it is. A canonical tag that names its library (S1), which
would let dune pick [pkg/client/Content], is not written by dune yet.

A resolved canonical tag would be wrong, even to the client's own
[Content.Html].

  $ odoc_print --short --long-paths --show-root-parents -r Html h/pkg/client/content.odocl
  module root(pkg/client/Content).Html =
    resolved(canonical(hidden(root(pkg/client/Content__Html)),resolved(root(pkg/client/Content).Html)))
    
  $ cd ..

Pages are out of the spec's scope, so no rule of it applies to
the rest of this test. A page can refer to itself by name. Two packages each have a page [index],
and each page refers to [page-index]. The search path has both packages, and
the other package comes last.

  $ cat pa/index.mld
  {0 Index of pa}
  
  This page: {!page-index}.
  $ cat pb/index.mld
  {0 Index of pb}
  
  This page: {!page-index}.
  $ for p in pa pb; do
  >   odoc compile --output-dir h --parent-id $p $p/index.mld
  > done

The reference in [pa]'s page should go to that page, [pa/index], and the one
in [pb]'s page to [pb/index]. A page that uses its own name means itself,
whatever other pages of that name the search path has, and in whatever
order.

  $ odoc link -I h/pa -I h/pb h/pa/page-index.odoc
  $ odoc_print --refs --show-root-parents h/pa/page-index.odocl
  r(pa/index)
  $ odoc link -I h/pa -I h/pb h/pb/page-index.odoc
  $ odoc_print --refs --show-root-parents h/pb/page-index.odocl
  r(pb/index)
