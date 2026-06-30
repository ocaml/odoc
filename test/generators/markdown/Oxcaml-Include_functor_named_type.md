
# Module `Oxcaml.Include_functor_named_type`

```ocaml
module type Make = functor (_ : sig ... end) -> sig ... end
```
This is a Module where the type is named and then included.

```ocaml
type t
```
```ocaml
type included
```