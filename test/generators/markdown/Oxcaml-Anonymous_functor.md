
# Module `Oxcaml.Anonymous_functor`

```ocaml
module BODY_31 : sig ... end
```
```ocaml
type t
```
In this test case the functor is defined inline

```ocaml
module INCLUDE_34 (T : sig ... end) : sig ... end
```
```ocaml
module APPLIED_35 : module type of struct include INCLUDE_34(BODY_31) end
```
```ocaml
type included = INCLUDE_34(BODY_31).included
```