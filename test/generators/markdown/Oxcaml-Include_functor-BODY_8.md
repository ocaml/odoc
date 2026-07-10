
# Module `Include_functor.BODY_8`

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