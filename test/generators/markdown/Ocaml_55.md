
# Module `Ocaml_55`

```ocaml
module type X = sig ... end
```
```ocaml
type m = (module X)
```
```ocaml
val f0 : m -> unit
```
```ocaml
val f55 : (module M : X) -> M.t
```
```ocaml
val f' : (module M : X with type t = int) -> int
```
```ocaml
val f'' : (module X with type t = int) -> int
```
```ocaml
module type Y = sig ... end
```
```ocaml
val g : (module M : Y) -> int M.t
```
```ocaml
val g' : (module M : Y) -> int
```
```ocaml
val g'' : (module Y) -> int
```