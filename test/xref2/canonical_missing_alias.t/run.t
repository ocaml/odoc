This test reproduces a problem seen in real libraries (Jane Street's `core`
being the motivating example) that use Dune's wrapping together with a
hand-written top-level module.

Dune generates the alias module `main__.ml` with a canonical tag `Main.M` for
every module `M` in the library. When `main.ml` is hand-written, it is up to
the author to re-export those modules under exactly those names. If a module
is re-exported under a *different* name - or not at all - the canonical path
Dune wrote down doesn't exist, and references to that module can't be
resolved. See `doc/dune.mld` for a description of the wrapping scheme.

Here `Zone` is a module of the library:

  $ cat main__Zone.mli
  type t
  
  val name : t -> string


`main__Foo.mli` refers to it, so `Zone.t` will end up in `Main.Foo`'s
signature:

  $ cat main__Foo.mli
  val f : Zone.t -> int

This is the alias module Dune generates. Note the canonical tag on `Zone`
pointing at `Main.Zone`:

  $ cat main__.ml
  (** @canonical Main.Zone *)
  module Zone = Main__Zone
  
  (** @canonical Main.Foo *)
  module Foo = Main__Foo


But the hand-written `main.ml` doesn't have a `Zone` alias - it exposes the
module under a different name, so `Main.Zone` never exists:

  $ cat main.ml
  open Main__
  
  module Foo = Foo
  
  (* [Zone] is exposed here, but under a different name - so [Main.Zone], the
     canonical path Dune put on the alias in [main__.ml], does not exist. *)
  module Private = struct
    module Zone_alias = Zone
  end
  
  let zone_name = Zone.name




Build it the way Dune does: the alias module first, with `-no-alias-deps`, and
everything else with `-open Main__`.

  $ ocamlc -c -bin-annot -no-alias-deps -w -49 main__.ml
  $ ocamlc -c -bin-annot -no-alias-deps main__Zone.mli
  $ ocamlc -c -bin-annot -no-alias-deps -open Main__ main__Foo.mli
  $ ocamlc -c -bin-annot -no-alias-deps -open Main__ main.ml

  $ odoc compile -I . main__Zone.cmti
  $ odoc compile -I . main__.cmt
  $ odoc compile -I . main__Foo.cmti
  $ odoc compile -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc html-generate --indent -o html main.odocl

`Main.Private.Zone_alias` is documented, since it's an alias of a hidden
module and so gets expanded:

  $ find html/Main -name index.html | sort
  html/Main/Foo/index.html
  html/Main/Private/Zone_alias/index.html
  html/Main/Private/index.html
  html/Main/index.html

But the reference in `Main.Foo` is unresolved: the canonical path `Main.Zone`
can't be resolved, and odoc has no way of knowing that
`Main.Private.Zone_alias` names the same module.

  $ grep -A3 'val</span> f' html/Main/Foo/index.html
        <span><span class="keyword">val</span> f : 
         <span><span class="xref-unresolved">Main__.Zone.t</span> 
          <span class="arrow">&#45;&gt;</span>
         </span> int

The fix is to give `Zone` a canonical path that actually exists. The tag Dune
generates can't be changed, but a canonical tag in the module's own preamble
takes precedence over it, so the library author can correct it from
`main__Zone.mli`:

  $ sed -i '1i (** @canonical Main.Private.Zone_alias *)\n' main__Zone.mli
  $ cat main__Zone.mli
  (** @canonical Main.Private.Zone_alias *)
  
  type t
  
  val name : t -> string



  $ rm -rf html *.cm* *.odoc *.odocl
  $ ocamlc -c -bin-annot -no-alias-deps -w -49 main__.ml
  $ ocamlc -c -bin-annot -no-alias-deps main__Zone.mli
  $ ocamlc -c -bin-annot -no-alias-deps -open Main__ main__Foo.mli
  $ ocamlc -c -bin-annot -no-alias-deps -open Main__ main.ml

  $ odoc compile -I . main__Zone.cmti
  $ odoc compile -I . main__.cmt
  $ odoc compile -I . main__Foo.cmti
  $ odoc compile -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc html-generate --indent -o html main.odocl

`Main.Private.Zone_alias` should still be documented - it must not lose its
expansion just because it is now the canonical destination of the module it is
an alias of. But its page has gone:

  $ find html/Main -name index.html | sort
  html/Main/Foo/index.html
  html/Main/Private/index.html
  html/Main/index.html

and while the reference now names the right module, it is rendered as plain
text rather than a link to it:

  $ grep -A4 'val</span> f' html/Main/Foo/index.html
        <span><span class="keyword">val</span> f : 
         <span>Private.Zone_alias.t <span class="arrow">&#45;&gt;</span></span>
          int
        </span>
       </code>
