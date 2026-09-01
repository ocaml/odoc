
# Module type `Oxcaml.S_param`

```ocaml
kind_ param_kind = value mod portable
```
```ocaml
type ('a : param_kind) t
```
A kind-constrained type parameter inside a signature expansion; the use should link to `param_kind`.
