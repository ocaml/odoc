
# Module `Include_functor_kind.Make`

A kind abbreviation reaching the functor through its argument. `T.k` is rendered as plain text: kind annotations hold references, which are not substituted when the functor is applied.


## Parameters

```ocaml
module T : sig ... end
```

## Signature

```ocaml
type inherited : T.k
```