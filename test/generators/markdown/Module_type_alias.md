
# Module `Module_type_alias`

Module Type Aliases

```ocaml
module type A = sig ... end
```
```ocaml
module type B = functor (C : sig ... end) -> sig ... end
```
```ocaml
module type D = A
```
```ocaml
module type E = functor (F : sig ... end) -> B
```
```ocaml
module type G = functor (H : sig ... end) -> D
```
```ocaml
module type I = B
```