
# Module `Oxcaml.Include_functor`

```ocaml
module BODY_8 : sig ... end
```
```ocaml
module Make (T : sig ... end) : sig ... end
```
Module which defines a functor and includes it via `module type of`

```ocaml
type t
```
```ocaml
module INCLUDE_11 : module type of Make
```
```ocaml
module APPLIED_12 : module type of struct include INCLUDE_11(BODY_8) end
```
```ocaml
type included = BODY_8.t
```