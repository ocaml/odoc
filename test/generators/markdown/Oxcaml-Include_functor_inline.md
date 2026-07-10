
# Module `Oxcaml.Include_functor_inline`

```ocaml
module BODY_26 : sig ... end
```
```ocaml
module type Make = functor (_ : sig ... end) -> sig ... end
```
This is a test case where the functor is named and included inline

```ocaml
type t
```
```ocaml
module INCLUDE_29 : Make
```
```ocaml
module APPLIED_30 : module type of struct include INCLUDE_29(BODY_26) end
```
```ocaml
type included = INCLUDE_29(BODY_26).included
```