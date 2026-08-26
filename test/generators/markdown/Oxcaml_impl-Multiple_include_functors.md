
# Module `Oxcaml_impl.Multiple_include_functors`

```ocaml
module First (T : sig ... end) : sig ... end
```
Two `include functor`s in the same structure, with an item defined between them.

```ocaml
module Second (T : sig ... end) : sig ... end
```
```ocaml
type t
```
```ocaml
type between
```
```ocaml
type second = BODY__15.first
```
```ocaml
type third
```