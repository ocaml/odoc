
# Module `Bugs_pre_410`

```
type 'a opt' = int option
```
```
val foo' : ?bar:'a -> unit -> unit
```
Similar to `Bugs`, but the printed type of `~bar` should be `int`, not `'a`. This probably requires fixing in the compiler. See [https://github.com/ocaml/odoc/pull/230\#issuecomment-433226807](https://github.com/ocaml/odoc/pull/230#issuecomment-433226807).
