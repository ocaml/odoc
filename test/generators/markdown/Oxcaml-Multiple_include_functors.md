
# Module `Oxcaml.Multiple_include_functors`

```ocaml
module First (T : sig ... end) : sig ... end
```
Two `include functor`s in the same signature, with an item defined between them.

```ocaml
module Second (T : sig ... end) : sig ... end
```
```ocaml
type t
```
```ocaml
type first
```
```ocaml
type between
```
```ocaml
type second = t
```
```ocaml
type third
```