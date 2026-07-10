
# Module `Oxcaml_impl.Include_functor`

```ocaml
module BODY_4 : sig ... end
```
```ocaml
module Make (T : sig ... end) : sig ... end
```
This module demonstrates the `include functor` functionality

```ocaml
type t
```
```ocaml
type included = Make(BODY_4).included
```