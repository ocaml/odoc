
# Module `Oxcaml.Include_functor`

```ocaml
module Make (T : sig ... end) : sig ... end
```
Module which defines a functor and includes it via `module type of`

```ocaml
type t
```
```ocaml
type included = t
```