Kind abbreviations are referred to by paths, which go through the same
expansion machinery as type paths.

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot c.mli
  $ odoc compile a.cmti
  $ odoc compile -I . b.cmti
  $ odoc compile c.cmti
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc link -I . c.odoc
  $ odoc html-generate -o html a.odocl
  $ odoc html-generate -o html b.odocl
  $ odoc html-generate -o html c.odocl
  $ odoc markdown-generate -o md a.odocl
  $ odoc markdown-generate -o md b.odocl
  $ odoc markdown-generate -o md c.odocl

Uses of a kind abbreviation link to its declaration, like uses of a type:

  $ cat md/A.md
  
  # Module `A`
  
  ```ocaml
  kind_ k = value mod portable
  ```
  ```ocaml
  type u
  ```
  ```ocaml
  type t : k
  ```
  ```ocaml
  type t2 = u
  ```
  ```ocaml
  type h : Hidden__.hk
  ```
  ```ocaml
  module type S = sig ... end
  ```
  ```ocaml
  module F (X : S) : sig ... end
  ```
  ```ocaml
  module Arg : S
  ```
  ```ocaml
  module App : sig ... end
  ```
  ```ocaml
  module Local : sig ... end
  ```
  ```ocaml
  type l : Local.lk
  ```
  ```ocaml
  type 'a w : immutable_data with u
  ```
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/A/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../index.html">Index</a>
  <a href="#type-u">u</a>
  <a href="module-type-S/index.html">S</a>
  <a href="F/index.html">F</a>
  <a href="F/argument-1-X/index.html">X</a>
  <a href="module-type-S/index.html">S</a>
  <a href="Arg/index.html">Arg</a>
  <a href="module-type-S/index.html">S</a>
  <a href="App/index.html">App</a>
  <a href="Local/index.html">Local</a>

The functor application [F(Arg)] substitutes the argument's kind:

  $ cat md/A-App.md
  
  # Module `A.App`
  
  ```ocaml
  type t : X.sk = F(Arg).t
  ```
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/A/App/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../../index.html">Index</a>
  <a href="../index.html">A</a>
  <a href="../F/index.html#type-t">F(Arg).t</a>

After [include module type of A], [t : k] refers to [A.k], like [t2 = A.u]:

  $ cat md/B.md
  
  # Module `B`
  
  ```ocaml
  kind_ k = value mod portable
  ```
  ```ocaml
  type u
  ```
  ```ocaml
  type t : k
  ```
  ```ocaml
  type t2 = u
  ```
  ```ocaml
  type h : Hidden__.hk
  ```
  ```ocaml
  module type S = sig ... end
  ```
  ```ocaml
  module F (X : S) : sig ... end
  ```
  ```ocaml
  module Arg : S
  ```
  ```ocaml
  module App : sig ... end
  ```
  ```ocaml
  module Local : sig ... end
  ```
  ```ocaml
  type l : Local.lk
  ```
  ```ocaml
  type 'a w : immutable_data with u
  ```
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/B/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../index.html">Index</a>
  <a href="../A/index.html">A</a>
  <a href="#type-u">u</a>
  <a href="module-type-S/index.html">S</a>
  <a href="F/index.html">F</a>
  <a href="F/argument-1-X/index.html">X</a>
  <a href="module-type-S/index.html">S</a>
  <a href="Arg/index.html">Arg</a>
  <a href="module-type-S/index.html">S</a>
  <a href="App/index.html">App</a>
  <a href="Local/index.html">Local</a>

A kind in a module with a canonical alias links to the canonical module:

  $ cat md/C.md
  
  # Module `C`
  
  ```ocaml
  module Canon : sig ... end
  ```
  ```ocaml
  type c : Canon__.ck
  ```
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/C/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../index.html">Index</a>
  <a href="Canon/index.html">Canon</a>
