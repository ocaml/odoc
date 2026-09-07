OxCaml parameterized libraries: check that odoc renders documentation for
library parameters, their implementations, parameterized libraries and the
instantiations of parameterized libraries types.

The library graph built below is:

- `A_param` and `B_param`: library parameters.
- `A1` and `A2`: implementations of `A_param`.
- `B1`: implementation of `B_param`.
- `A_of_b`: parameterised by `B_param`, implements `A_param`.
- `Only_a`: parameterised by `A_param`.
- `Both_ab`: parameterised by `A_param` and `B_param`, uses `Only_a(A1)` and `Only_a(A_of_b)`.
- `Final`: uses `Both_ab(A1)(B1)` and `Both_ab(A2)(B1)`.

The support for OxCaml parameterized library in Dune requires:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using oxcaml 0.1)
  > EOF

The library parameters:

  $ mkdir a_param
  $ cat > a_param/dune <<EOF
  > (library_parameter
  >  (name a_param))
  > EOF
  $ cat > a_param/a_param.mli <<EOF
  > (** The [A_param] library parameter. *)
  > 
  > type t
  > (** Abstract elements. *)
  > 
  > val name : string
  > 
  > val make : int -> t
  > 
  > val to_string : t -> string
  > 
  > module Sub : sig
  >   type s
  >   val zero : s
  > end
  > 
  > module type ORDER = sig
  >   type o
  >   val compare : o -> o -> int
  > end
  > EOF

  $ mkdir b_param
  $ cat > b_param/dune <<EOF
  > (library_parameter
  >  (name b_param))
  > EOF
  $ cat > b_param/b_param.mli <<EOF
  > (** The [B_param] library parameter. *)
  > 
  > type u
  > 
  > val tag : u
  > 
  > val of_int : int -> u
  > 
  > val combine : u -> u -> u
  > 
  > val show : u -> string
  > EOF

Two implementations of [A_param]:

  $ mkdir a1
  $ cat > a1/dune <<EOF
  > (library
  >  (name a1)
  >  (implements a_param))
  > EOF
  $ cat > a1/a1.ml <<EOF
  > type t = int
  > let name = "a1"
  > let make n = n
  > let to_string = string_of_int
  > module Sub = struct type s = unit let zero = () end
  > module type ORDER = sig type o val compare : o -> o -> int end
  > EOF

  $ mkdir a2
  $ cat > a2/dune <<EOF
  > (library
  >  (name a2)
  >  (implements a_param))
  > EOF
  $ cat > a2/a2.ml <<EOF
  > type t = string
  > let name = "a2"
  > let make = string_of_int
  > let to_string s = s
  > module Sub = struct type s = bool let zero = false end
  > module type ORDER = sig type o val compare : o -> o -> int end
  > EOF

One implementation of [B_param]:

  $ mkdir b1
  $ cat > b1/dune <<EOF
  > (library
  >  (name b1)
  >  (implements b_param))
  > EOF
  $ cat > b1/b1.ml <<EOF
  > type u = int
  > let tag = 0
  > let of_int n = n
  > let combine = (+)
  > let show = string_of_int
  > EOF

[A_of_b] is parameterised by [B_param] and implements [A_param]:

  $ mkdir a_of_b
  $ cat > a_of_b/dune <<EOF
  > (library
  >  (name a_of_b)
  >  (parameters b_param)
  >  (implements a_param))
  > EOF
  $ cat > a_of_b/a_of_b.ml <<EOF
  > type t = B_param.u
  > let name = "a_of_b"
  > let make n = B_param.of_int n
  > let to_string t = B_param.show t
  > module Sub = struct type s = B_param.u let zero = B_param.tag end
  > module type ORDER = sig type o val compare : o -> o -> int end
  > EOF

[Only_a] is parameterised by [A_param]. It exercises types, submodules, module
aliases, functors and first-class module arguments referring to the parameter:

  $ mkdir only_a
  $ cat > only_a/dune <<EOF
  > (library
  >  (name only_a)
  >  (parameters a_param))
  > EOF
  $ cat > only_a/only_a.ml <<EOF
  > (** Helpers built on top of the {!A_param} parameter. *)
  > 
  > type wrapped = { value : A_param.t; label : string }
  > 
  > let wrap value = { value; label = A_param.name }
  > 
  > let default n = wrap (A_param.make n)
  > 
  > let show w = A_param.to_string w.value
  > 
  > module Alias = A_param.Sub
  > 
  > module Make (O : A_param.ORDER) = struct
  >   let min a b = if O.compare a b <= 0 then a else b
  > end
  > 
  > module type S = sig type q end
  > 
  > module Inner = struct type i = int let v = 0 end
  > 
  > let pick (type a) (module O : A_param.ORDER with type o = a) (x : a) (y : a) =
  >   if O.compare x y <= 0 then x else y
  > EOF

[Both_ab] is parameterised by [A_param] and [B_param], depends on [Only_a] and
instantiates it as [Only_a(A1)] and [Only_a(A_of_b)] (the latter implicitly
using [Both_ab]'s [B_param]):

  $ mkdir both_ab
  $ cat > both_ab/dune <<EOF
  > (library
  >  (name both_ab)
  >  (parameters a_param b_param)
  >  (libraries
  >   (instantiate only_a a1 :as only_a1)
  >   (instantiate only_a a_of_b :as only_a_of_b)))
  > EOF
  $ cat > both_ab/both_ab.ml <<EOF
  > type combined = { a : A_param.t; b : B_param.u }
  > 
  > let make i = { a = A_param.make i; b = B_param.of_int i }
  > 
  > let demo_a1 : Only_a1.wrapped = Only_a1.default 1
  > 
  > let demo_a_of_b : Only_a_of_b.wrapped = Only_a_of_b.default 2
  > 
  > module Nested = Only_a1.Inner
  > 
  > let nested : Only_a1.Inner.i = 0
  > 
  > module type Sig = Only_a1.S
  > 
  > let packed : (module Only_a1.S) option = None
  > EOF

[Final] depends on the full instantiations [Both_ab(A1)(B1)] and
[Both_ab(A2)(B1)]:

  $ mkdir final
  $ cat > final/dune <<EOF
  > (library
  >  (name final)
  >  (libraries
  >   (instantiate both_ab a1 b1 :as both_a1_b1)
  >   (instantiate both_ab a2 b1 :as both_a2_b1)))
  > EOF
  $ cat > final/final.ml <<EOF
  > let x = Both_a1_b1.make 1
  > let y = Both_a2_b1.make 2
  > let combos : Both_a1_b1.combined * Both_a2_b1.combined = (x, y)
  > EOF

Everything builds and odoc generates documentation for all the libraries:

  $ dune build @doc-private 2>&1

Render to markdown for inspection:

  $ for f in $(find _build/default/_doc/_odocls -name '*.odocl' | sort); do
  >   odoc markdown-generate "$f" -o markdown 2>&1
  > done

The library names below are suffixed with an opaque hash by dune; we normalise
it away so the output is stable.

  $ md() { cat markdown/$1@*/$2.md | sed 's/@[0-9a-f]*/@HASH/g'; }

The library parameters are reported as such:

  $ md a_param A_param
  
  # Library parameter `A_param`
  
  The `A_param` library parameter.
  
  ```ocaml
  type t
  ```
  Abstract elements.
  
  ```ocaml
  val name : string
  ```
  ```ocaml
  val make : int -> t
  ```
  ```ocaml
  val to_string : t -> string
  ```
  ```ocaml
  module Sub : sig ... end
  ```
  ```ocaml
  module type ORDER = sig ... end
  ```
  $ md b_param B_param
  
  # Library parameter `B_param`
  
  The `B_param` library parameter.
  
  ```ocaml
  type u
  ```
  ```ocaml
  val tag : u
  ```
  ```ocaml
  val of_int : int -> u
  ```
  ```ocaml
  val combine : u -> u -> u
  ```
  ```ocaml
  val show : u -> string
  ```

The implementations show which parameter they implement, with a link to the
parameter documentation:

  $ md a1 A1
  
  # Module `A1`
  
  Implements the library parameter [`A_param`](./../a_param@HASH/A_param.md).
  
  ```ocaml
  type t = int
  ```
  ```ocaml
  val name : string
  ```
  ```ocaml
  val make : 'a -> 'a
  ```
  ```ocaml
  val to_string : int -> string
  ```
  ```ocaml
  module Sub : sig ... end
  ```
  ```ocaml
  module type ORDER = sig ... end
  ```
  $ md a2 A2
  
  # Module `A2`
  
  Implements the library parameter [`A_param`](./../a_param@HASH/A_param.md).
  
  ```ocaml
  type t = string
  ```
  ```ocaml
  val name : string
  ```
  ```ocaml
  val make : int -> string
  ```
  ```ocaml
  val to_string : 'a -> 'a
  ```
  ```ocaml
  module Sub : sig ... end
  ```
  ```ocaml
  module type ORDER = sig ... end
  ```
  $ md b1 B1
  
  # Module `B1`
  
  Implements the library parameter [`B_param`](./../b_param@HASH/B_param.md).
  
  ```ocaml
  type u = int
  ```
  ```ocaml
  val tag : int
  ```
  ```ocaml
  val of_int : 'a -> 'a
  ```
  ```ocaml
  val combine : int -> int -> int
  ```
  ```ocaml
  val show : int -> string
  ```
  $ md a_of_b A_of_b
  
  # Module `A_of_b`
  
  Implements the library parameter [`A_param`](./../a_param@HASH/A_param.md).
  
  
  ## Library parameters
  
  ```ocaml
  parameter B_param
  ```
  
  ## Signature
  
  ```ocaml
  type t = B_param.u
  ```
  ```ocaml
  val name : string
  ```
  ```ocaml
  val make : int -> B_param.u
  ```
  ```ocaml
  val to_string : B_param.u -> string
  ```
  ```ocaml
  module Sub : sig ... end
  ```
  ```ocaml
  module type ORDER = sig ... end
  ```

The parameterised libraries list the parameters they are parameterised by, with
links to the parameter documentation. Instantiations are rendered in OxCaml
instance syntax as `Only_a[A_param:A1]` and `Both_ab[A_param:A1][B_param:B1]`
(the base library, each parameter and each argument keeping their own link)
rather than through the internal wrapper modules:

  $ md only_a Only_a
  
  # Module `Only_a`
  
  Helpers built on top of the [`A_param`](./../a_param@HASH/A_param.md) parameter.
  
  
  ## Library parameters
  
  ```ocaml
  parameter A_param
  ```
  
  ## Signature
  
  ```ocaml
  type wrapped = {
    value : A_param.t;
    label : string;
  }
  ```
  ```ocaml
  val wrap : A_param.t -> wrapped
  ```
  ```ocaml
  val default : int -> wrapped
  ```
  ```ocaml
  val show : wrapped -> string
  ```
  ```ocaml
  module Alias = A_param.Sub
  ```
  ```ocaml
  module Make (O : A_param.ORDER) : sig ... end
  ```
  ```ocaml
  module type S = sig ... end
  ```
  ```ocaml
  module Inner : sig ... end
  ```
  ```ocaml
  val pick : (module A_param.ORDER with type o = 'a) -> 'a -> 'a -> 'a
  ```
  $ md both_ab Both_ab
  
  # Module `Both_ab`
  
  
  ## Library parameters
  
  ```ocaml
  parameter A_param
  ```
  ```ocaml
  parameter B_param
  ```
  
  ## Signature
  
  ```ocaml
  type combined = {
    a : A_param.t;
    b : B_param.u;
  }
  ```
  ```ocaml
  val make : int -> combined
  ```
  ```ocaml
  val demo_a1 : Only_a[A_param:A1].wrapped
  ```
  ```ocaml
  val demo_a_of_b : Only_a[A_param:A_of_b].wrapped
  ```
  ```ocaml
  module Nested = Only_a[A_param:A1].Inner
  ```
  ```ocaml
  val nested : Only_a[A_param:A1].Inner.i
  ```
  ```ocaml
  module type Sig = Only_a[A_param:A1].S
  ```
  ```ocaml
  val packed : (module Only_a[A_param:A1].S) option
  ```
  $ md final Final
  
  # Module `Final`
  
  ```ocaml
  val x : Both_ab[A_param:A1][B_param:B1].combined
  ```
  ```ocaml
  val y : Both_ab[A_param:A2][B_param:B1].combined
  ```
  ```ocaml
  val combos : 
    Both_ab[A_param:A1][B_param:B1].combined
    * Both_ab[A_param:A2][B_param:B1].combined
  ```

The page of a library parameter keeps the url derived from its identifier, so
the sidebar marks it as the current unit and keeps its children, exactly like a
plain module:

  $ odoc compile-index --root _build/default/_doc/_odocls/
  $ odoc sidebar-generate index.odoc-index
  $ for lib in a_param a1; do
  >   odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html \
  >     $(find _build/default/_doc/_odocls -iname "$lib.odocl")
  > done
  $ grep -rl 'current_unit' html/ | sed 's/@[0-9a-f]*/@HASH/g' | sort
  html/a1@HASH/A1/Sub/index.html
  html/a1@HASH/A1/index.html
  html/a1@HASH/A1/module-type-ORDER/index.html
  html/a_param@HASH/A_param/Sub/index.html
  html/a_param@HASH/A_param/index.html
  html/a_param@HASH/A_param/module-type-ORDER/index.html
