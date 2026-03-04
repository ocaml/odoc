
# Module `Type`

```ocaml
type abstract
```
Some *documentation*.

```ocaml
type alias = int
```
```ocaml
type private_ = private int
```
```ocaml
type 'a constructor = 'a
```
```ocaml
type arrow = int -> int
```
```ocaml
type higher_order = (int -> int) -> int
```
```ocaml
type labeled = l:int -> int
```
```ocaml
type optional = ?l:int -> int
```
```ocaml
type labeled_higher_order = (l:int -> int) -> (?l:int -> int) -> int
```
```ocaml
type pair = int * int
```
```ocaml
type parens_dropped = int * int
```
```ocaml
type triple = int * int * int
```
```ocaml
type nested_pair = (int * int) * int
```
```ocaml
type instance = int constructor
```
```ocaml
type long =
  labeled_higher_order ->
  [ `Bar | `Baz of triple ] ->
  pair ->
  labeled ->
  higher_order ->
  (string -> int) ->
  (int * float * char * string * char * unit) option ->
  nested_pair ->
  arrow ->
  string ->
  nested_pair array
```
```ocaml
type variant_e = {
  a : int;
}
```
```ocaml
type variant = 
  | A
  | B of int
  | C (* foo *)
  | D (* bar *)
  | E of variant_e
```
```ocaml
type variant_c = {
  a : int;
}
```
```ocaml
type _ gadt = 
  | A : int gadt
  | B : int -> string gadt
  | C : variant_c -> unit gadt
```
```ocaml
type degenerate_gadt = 
  | A : degenerate_gadt
```
```ocaml
type private_variant = private 
  | A
```
```ocaml
type record = {
  a : int;
  mutable b : int;
  c : int; (* foo *)
  d : int; (* bar *)
  e : 'a. 'a;
}
```
```ocaml
type polymorphic_variant = [ 
  | `A
  | `B of int
  | `C of int * unit
  | `D
 ]
```
```ocaml
type polymorphic_variant_extension = [ 
  | polymorphic_variant
  | `E
 ]
```
```ocaml
type nested_polymorphic_variant = [ 
  | `A of [ `B | `C ]
 ]
```
```ocaml
type private_extenion = private [> 
  | polymorphic_variant
 ]
```
```ocaml
type object_ = < a : int ; b : int ; c : int >
```
```ocaml
module type X = sig ... end
```
```ocaml
type module_ = (module X)
```
```ocaml
type module_substitution = (module X with type t = int and type u = unit)
```
```ocaml
type +'a covariant
```
```ocaml
type -'a contravariant
```
```ocaml
type _ bivariant = int
```
```ocaml
type ('a, 'b) binary
```
```ocaml
type using_binary = (int, int) binary
```
```ocaml
type 'custom name
```
```ocaml
type 'a constrained = 'a constraint 'a = int
```
```ocaml
type 'a exact_variant = 'a constraint 'a = [ `A | `B of int ]
```
```ocaml
type 'a lower_variant = 'a constraint 'a = [> `A | `B of int ]
```
```ocaml
type 'a any_variant = 'a constraint 'a = [>  ]
```
```ocaml
type 'a upper_variant = 'a constraint 'a = [< `A | `B of int ]
```
```ocaml
type 'a named_variant = 'a constraint 'a = [< polymorphic_variant ]
```
```ocaml
type 'a exact_object = 'a constraint 'a = < a : int ; b : int >
```
```ocaml
type 'a lower_object = 'a constraint 'a = < a : int ; b : int.. >
```
```ocaml
type 'a poly_object = 'a constraint 'a = < a : 'a. 'a >
```
```ocaml
type ('a, 'b) double_constrained = 'a * 'b constraint 'a = int constraint 'b = unit
```
```ocaml
type as_ = (int as 'a) * 'a
```
```ocaml
type extensible = ..
```
```ocaml
type extensible += 
  | Extension (* Documentation for Extension. *)
  | Another_extension (* Documentation for Another_extension. *)
```
```ocaml
type mutually = 
  | A of recursive
```
```ocaml
and recursive = 
  | B of mutually
```
```ocaml
exception Foo of int * int
```
```ocaml
type 'a t = ([ `A ] as 'a) option
```