
# Module `Oxcaml.Include_functor_desugared`

```ocaml
module Make (T : sig ... end) : sig ... end
```
This module is the desugared version of [`Include_functor`](./Oxcaml-Include_functor.md): the synthetic module aliases the preceding items rather than copying them, so that the types the functor inherits from its argument resolve back to them.

```ocaml
type t
```
```ocaml
type included = t
```