
# Module `Oxcaml.M_param`

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
