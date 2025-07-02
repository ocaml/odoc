
# Module `Module_type_alias`

Module Type Aliases

```
module type A = sig ... end
```
```
module type B = functor (C : sig ... end) -> sig ... end
```
```
module type D = A
```
```
module type E = functor (F : sig ... end) -> B
```
```
module type G = functor (H : sig ... end) -> D
```
```
module type I = B
```