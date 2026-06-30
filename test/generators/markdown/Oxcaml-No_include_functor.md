
# Module `Oxcaml.No_include_functor`

```ocaml
module Make (T : sig ... end) : sig ... end
```
Module without any `include functor` features, this is how things are done in plain OCaml at the moment.

```ocaml
module T : sig ... end
```
```ocaml
type t
```
```ocaml
type included = Make(T).included
```