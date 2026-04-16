
# Module `Recent`

```ocaml
module type S = sig ... end
```
```ocaml
module type S1 = functor (_ : S) -> S
```
```ocaml
type variant = 
  | A
  | B of int
  | C (* foo *)
  | D (* bar *)
  | E of {
    a : int;
  }
```
```ocaml
type _ gadt = 
  | A : int gadt
  | B : int -> string gadt (* foo *)
  | C : {
    a : int;
  } -> unit gadt
```
```ocaml
type polymorphic_variant = [ 
  | `A
  | `B of int
  | `C (* foo *)
  | `D (* bar *)
 ]
```
```ocaml
type empty_variant = |
```
```ocaml
type nonrec nonrec_ = int
```
```ocaml
type empty_conj = 
  | X : [< `X of & 'a & int * float ] -> empty_conj
```
```ocaml
type conj = 
  | X : [< `X of int & [< `B of int & float ] ] -> conj
```
```ocaml
val empty_conj : [< `X of & 'a & int * float ]
```
```ocaml
val conj : [< `X of int & [< `B of int & float ] ]
```
```ocaml
module Z : sig ... end
```
```ocaml
module X : sig ... end
```
```ocaml
module type PolyS = sig ... end
```
```ocaml
type +-'a phantom
```
```ocaml
val f : (x:int * y:int) phantom -> unit
```