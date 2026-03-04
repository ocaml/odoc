
# Module `CanonicalTest.Base_Tests`

```ocaml
module C : module type of Base.List
```
```ocaml
module L = Base.List
```
```ocaml
val foo : int L.t -> float L.t
```
```ocaml
val bar : 'a Base.List.t -> 'a Base.List.t
```
```ocaml
val baz : 'a Base.List.t -> unit
```