
# Module `Functor`

```
module type S = sig ... end
```
```
module type S1 = functor (_ : S) -> S
```
```
module F1 (Arg : S) : S
```
```
module F2 (Arg : S) : S with type t = Arg.t
```
```
module F3 (Arg : S) : sig ... end
```
```
module F4 (Arg : S) : S
```
```
module F5 () : S
```