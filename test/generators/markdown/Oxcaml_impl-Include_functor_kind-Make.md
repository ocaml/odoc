
# Module `Include_functor_kind.Make`

A kind abbreviation reaching the functor through its argument. `T.k` is rendered as plain text: it is a reference through the parameter, which the wrapper the functor is applied to does not bind.


## Parameters

```ocaml
module T : sig ... end
```

## Signature

```ocaml
type inherited : T.k
```