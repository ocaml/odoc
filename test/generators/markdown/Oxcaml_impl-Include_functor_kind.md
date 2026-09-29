
# Module `Oxcaml_impl.Include_functor_kind`

```ocaml
module Make (T : sig ... end) : sig ... end
```
A kind abbreviation reaching the functor through its argument. `T.k` is rendered as plain text: it is a reference through the parameter, which the wrapper the functor is applied to does not bind.

```ocaml
kind_ k = value mod portable
```
```ocaml
type inherited : T.k
```