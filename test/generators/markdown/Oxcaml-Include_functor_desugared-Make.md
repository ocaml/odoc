
# Module `Include_functor_desugared.Make`

This module is the desugared version of [`Include_functor`](./Oxcaml-Include_functor.md): the synthetic module aliases the preceding items rather than copying them, so that the types the functor inherits from its argument resolve back to them.


## Parameters

```ocaml
module T : sig ... end
```

## Signature

```ocaml
type included = T.t
```