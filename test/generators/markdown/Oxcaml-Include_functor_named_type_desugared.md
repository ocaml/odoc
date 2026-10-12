
# Module `Oxcaml.Include_functor_named_type_desugared`

```ocaml
module type MakeType = functor (X : sig ... end) -> sig ... end
```
This is the desugared version of [`Include_functor_named_type`](./Oxcaml-Include_functor_named_type.md).

```ocaml
type t
```
```ocaml
type included = t
```