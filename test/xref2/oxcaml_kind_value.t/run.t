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
  > 
  > type unboxed_record = #{ p : float#; q : int }
  > 
  > module F (X : sig end) : sig
  >   kind_ k
  > end
  > 
  > type applied : F(K).k
  > 
  > type ('a, 'b) two_with : immutable_data with 'a @@ many with 'b @@ contended
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
  ```ocaml
  type unboxed_record = #{
    p : float#;
    q : int;
  }
  ```
  ```ocaml
  module F (X : sig ... end) : sig ... end
  ```
  ```ocaml
  type applied : F(K).k
  ```
  ```ocaml
  type ('a, 'b) two_with : (immutable_data with 'a @@ many) with 'b @@ contended
  ```

Kind names link to their abbreviations, and [F(K).k] links into the result
signature of [F]:

  $ odoc html-generate -o html m.odocl
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/M/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../index.html">Index</a>
  <a href="#kind-value">value</a>
  <a href="#kind-k">k</a>
  <a href="K/index.html">K</a>
  <a href="K/index.html#kind-k">K.k</a>
  <a href="#kind-value">value</a>
  <a href="F/index.html">F</a>
  <a href="F/argument-1-X/index.html">X</a>
  <a href="F/index.html#kind-k">F(K).k</a>

From a [.cmi] alone, the compiler drops the kind annotation on [t] and stores only
its expansion, so [t] is rendered as [value mod portable], where [value] is the
built-in kind. [unboxed_record] has no annotation either, but its inferred kind
is not rendered as it was not written by the user. The with-bounds of
[two_with] are reordered by the compiler, but each type keeps its modality.

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
  ```ocaml
  type unboxed_record = #{
    p : float#;
    q : int;
  }
  ```
  ```ocaml
  module F (X : sig ... end) : sig ... end
  ```
  ```ocaml
  type applied : F(K).k
  ```
  ```ocaml
  type ('a, 'b) two_with : (immutable_data with 'b @@ contended) with 'a @@ many
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
  <a href="F/index.html">F</a>
  <a href="F/argument-1-X/index.html">X</a>
  <a href="F/index.html#kind-k">F(K).k</a>
