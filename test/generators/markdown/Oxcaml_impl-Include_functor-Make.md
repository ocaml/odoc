
# Module `Include_functor.Make`

This module demonstrates the `include functor` functionality. `Make` uses its argument, so `included` has to come out equal to `t` rather than abstract.


## Parameters

```ocaml
module T : sig ... end
```

## Signature

```ocaml
type included = T.t
```