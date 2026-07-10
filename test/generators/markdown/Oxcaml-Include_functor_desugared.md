
# Module `Oxcaml.Include_functor_desugared`

```ocaml
module Make (T : sig ... end) : sig ... end
```
```ocaml
type t
```
```ocaml
type included = Make(DUMMY__).included
```