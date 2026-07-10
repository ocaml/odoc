
# Module `Oxcaml_impl.Include_functor_desugared`

```ocaml
module Make (T : sig ... end) : sig ... end
```
This module is the desugared version from above

```ocaml
type t
```
```ocaml
type included = Make(DUMMY__).included
```