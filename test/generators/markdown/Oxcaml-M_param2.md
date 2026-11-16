
# Module `Oxcaml.M_param2`

Each instantiation renders its own copy of `param_kind`.

```ocaml
kind_ param_kind = value mod portable
```
```ocaml
type ('a : param_kind) t
```
A kind-constrained type parameter inside a signature expansion.

```ocaml
type t_direct : param_kind
```
A kind annotation inside a signature expansion.
