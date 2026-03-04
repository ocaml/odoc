
# Module `Functor`

```ocaml
module type S = sig ... end
```
```ocaml
module type S1 = functor (_ : S) -> S
```
```ocaml
module F1 (Arg : S) : S
```
```ocaml
module F2 (Arg : S) : S with type t = Arg.t
```
```ocaml
module F3 (Arg : S) : sig ... end
```
```ocaml
module F4 (Arg : S) : S
```
```ocaml
module F5 () : S
```