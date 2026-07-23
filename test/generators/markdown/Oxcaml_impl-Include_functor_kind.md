
# Module `Oxcaml_impl.Include_functor_kind`

```ocaml
module Make (T : sig ... end) : sig ... end
```
A kind abbreviation reaching the functor through its argument. `T.k` is rendered as plain text: kind annotations hold references, which are not substituted when the functor is applied.

```ocaml
kind_ k = value mod portable
```
```ocaml
type inherited : T.k
```