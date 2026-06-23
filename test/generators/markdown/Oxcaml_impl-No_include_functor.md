
# Module `Oxcaml_impl.No_include_functor`

```ocaml
module Make (T : sig ... end) : sig ... end
```
```ocaml
module T : sig ... end
```
```ocaml
type t = T.t
```
```ocaml
type included = Make(T).included
```