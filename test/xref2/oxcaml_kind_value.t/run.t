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
  $ odoc compile m.cmti 2>&1 | head -1
  odoc: internal error, uncaught exception:
  $ odoc link m.odoc
  Usage: odoc link [--help] [--custom-layout] [--open=MODULE] [OPTION]…
         FILE.odoc
  odoc: FILE.odoc argument: no m.odoc file or directory
  [2]
  $ odoc markdown-generate -o md m.odocl
  Usage: odoc markdown-generate [--help] [--extra-suffix=SUFFIX]
         [--sidebar=FILE.odoc-sidebar] [--syntax=SYNTAX] [OPTION]…
         FILE.odocl…
  odoc: FILE.odocl… arguments: no m.odocl file or directory
  [2]

The [value] the compiler inserts on unannotated declarations is the built-in
default, so it is hidden. Kind names are rendered as written, so the later
[value] on [t], which is the abbreviation above, cannot be told apart from the
default and is hidden too (BROKEN):

  $ cat md/M.md
  cat: md/M.md: No such file or directory
  [1]

Kind names are rendered as plain text, without links:

  $ odoc html-generate -o html m.odocl
  Usage: odoc html-generate [--help] [OPTION]… FILE.odocl…
  odoc: FILE.odocl… arguments: no m.odocl file or directory
  [2]
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/M/index.html | grep -v 'class="anchor"'
  grep: html/M/index.html: No such file or directory
  [1]

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
  type t_unannotated
  ```
  ```ocaml
  val poly_unannotated : 'a -> 'a
  ```
  ```ocaml
  type t
  ```
  ```ocaml
  type u
  ```
  ```ocaml
  module K : sig ... end
  ```
  ```ocaml
  type v
  ```
  ```ocaml
  type 'a w
  ```
  ```ocaml
  type p
  ```
  ```ocaml
  type m : immediate = int
  ```
  ```ocaml
  type n : immediate = int
  ```
  ```ocaml
  type unboxed_record = {
    p : float#;
    q : int;
  }
  ```
  ```ocaml
  module F (X : sig ... end) : sig ... end
  ```
  ```ocaml
  type applied
  ```
  ```ocaml
  type ('a, 'b) two_with
  ```

Kind names are still rendered as plain text:

  $ odoc html-generate -o html m.odocl
  $ grep -o '<a href="[^"]*">[^<]*</a>' html/M/index.html | grep -v 'class="anchor"'
  <a href="../index.html">Up</a>
  <a href="../index.html">Index</a>
  <a href="K/index.html">K</a>
  <a href="F/index.html">F</a>
  <a href="F/argument-1-X/index.html">X</a>
