A kind abbreviation can be named [value], shadowing the built-in kind.

  $ cat > m.mli << EOF
  > kind_ value = value mod portable
  > 
  > type t_unannotated
  > 
  > val poly_unannotated : 'a. 'a -> 'a
  > 
  > type t : value
  > 
  > kind_ k
  > 
  > type u : k
  > 
  > module K : sig
  >   kind_ k
  > end
  > 
  > type v : K.k
  > 
  > type 'a w : immutable_data with 'a
  > 
  > type p : float64 & immediate
  > 
  > type m : immediate = int
  > 
  > type n : value mod portable = int
  > EOF

  $ ocamlc -c -bin-annot m.mli
  $ odoc compile m.cmti
  $ odoc link m.odoc
  $ odoc markdown-generate -o md m.odocl

The later [value] on [t] is the abbreviation above, so it is rendered. The
[value] the compiler inserts on unannotated declarations is the
built-in default, so it is hidden:

  $ cat md/M.md
  
  # Module `M`
  
  ```ocaml
  kind_ value = value mod portable
  ```
  ```ocaml
  type t_unannotated
  ```
  ```ocaml
  val poly_unannotated : 'a. 'a -> 'a
  ```
  ```ocaml
  type t : value
  ```
  ```ocaml
  kind_ k
  ```
  ```ocaml
  type u : k
  ```
  ```ocaml
  module K : sig ... end
  ```
  ```ocaml
  type v : K.k
  ```
  ```ocaml
  type 'a w : immutable_data with 'a
  ```
  ```ocaml
  type p : float64 & immediate
  ```
  ```ocaml
  type m : immediate = int
  ```
  ```ocaml
  type n : value mod portable = int
  ```

Kind names link to their abbreviations:

  $ odoc html-generate -o html m.odocl
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/M/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../index.html">Index</a>
  <a href="#kind-value">value</a>
  <a href="#kind-k">k</a>
  <a href="K/index.html">K</a>
  <a href="K/index.html#kind-k">K.k</a>
  <a href="#kind-value">value</a>

From a [.cmi] alone, the compiler drops the kind annotation on [t] and stores only
its expansion, so [t] is rendered as [value mod portable], where [value] is the
built-in kind.

  $ mkdir cmi && cp m.mli cmi/ && cd cmi
  $ ocamlc -c m.mli
  $ odoc compile m.cmi
  $ odoc link m.odoc
  $ odoc markdown-generate -o md m.odocl
  $ cat md/M.md
  
  # Module `M`
  
  ```ocaml
  kind_ value
  ```
  ```ocaml
  type t_unannotated
  ```
  ```ocaml
  val poly_unannotated : 'a -> 'a
  ```
  ```ocaml
  type t : value mod portable
  ```
  ```ocaml
  kind_ k
  ```
  ```ocaml
  type u : k
  ```
  ```ocaml
  module K : sig ... end
  ```
  ```ocaml
  type v : K.k
  ```
  ```ocaml
  type 'a w : immutable_data with 'a
  ```
  ```ocaml
  type p : float64 & value non_pointer mod external_
  ```
  ```ocaml
  type m : immediate = int
  ```
  ```ocaml
  type n : immediate = int
  ```

Kind names still link to their abbreviations (but [t : value mod portable] is
now the default `value`, so it does not):

  $ odoc html-generate -o html m.odocl
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/M/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../index.html">Index</a>
  <a href="#kind-k">k</a>
  <a href="K/index.html">K</a>
  <a href="K/index.html#kind-k">K.k</a>
