
# Module type `Module_type_of.S`

```ocaml
module type T = sig ... end
```
```ocaml
module M : T
```
```ocaml
module N : module type of struct include M end
```