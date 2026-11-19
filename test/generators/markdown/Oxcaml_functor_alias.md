
# Module `Oxcaml_functor_alias`

A functor whose result aliases its argument, applied by `include functor`.

OxCaml 5.4 rejects such an alias, as OCaml does, so this is kept apart from the `oxcaml.mli` case and runs on earlier versions of OxCaml only.

```ocaml
module Make (T : sig ... end) : sig ... end
```
```ocaml
type t
```
```ocaml
type 'a p
```
```ocaml
module X : sig ... end
```
```ocaml
module type S = sig ... end
```
```ocaml
val via_module_type_include : unit
```
```ocaml
module Aliased : sig ... end
```
The input module itself. It should contain everything from the top level module
