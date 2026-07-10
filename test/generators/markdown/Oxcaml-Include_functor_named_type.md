
# Module `Oxcaml.Include_functor_named_type`

```ocaml
module BODY_21 : sig ... end
```
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
```ocaml
module APPLIED_25 : module type of struct include INCLUDE_24(BODY_21) end
```
```ocaml
type included = INCLUDE_24(BODY_21).included
```