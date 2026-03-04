
# Module `Functor2`

```ocaml
module type S = sig ... end
```
```ocaml
module X (Y : S) (Z : S) : sig ... end
```
```ocaml
module type XF = functor (Y : S) -> functor (Z : S) -> sig ... end
```