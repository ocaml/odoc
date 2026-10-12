
# Module `Oxcaml.Include_functor_named_type`

```ocaml
module type MakeType = functor (X : sig ... end) -> sig ... end
```
This is a module where the functor is named and then included.

```ocaml
type t
```
```ocaml
type included = t
```