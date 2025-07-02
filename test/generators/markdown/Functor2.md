
# Module `Functor2`

```
module type S = sig ... end
```
```
module X (Y : S) (Z : S) : sig ... end
```
```
module type XF = functor (Y : S) -> functor (Z : S) -> sig ... end
```