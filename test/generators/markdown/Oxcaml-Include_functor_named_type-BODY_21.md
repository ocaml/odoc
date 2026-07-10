
# Module `Include_functor_named_type.BODY_21`

```ocaml
module type MakeType = functor (_ : sig ... end) -> sig ... end
```
This is a module where the functor is named and then included.

```ocaml
type t
```
```ocaml
module INCLUDE_24 : MakeType
```