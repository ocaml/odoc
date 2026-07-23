
# Module `Oxcaml_impl.No_include_functor`

```ocaml
module Make (T : sig ... end) : sig ... end
```
This module shows how to achieve the effect without `include functor`, with an intermediate module `T`.

```ocaml
module T : sig ... end
```
```ocaml
type t = T.t
```
```ocaml
type included = Make(T).included
```