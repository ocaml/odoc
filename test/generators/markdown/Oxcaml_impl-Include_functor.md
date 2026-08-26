
# Module `Oxcaml_impl.Include_functor`

```ocaml
module Make (T : sig ... end) : sig ... end
```
This module demonstrates the `include functor` functionality. `Make` uses its argument, so `included` has to come out equal to `t` rather than abstract.

```ocaml
type t
```
```ocaml
type included
```