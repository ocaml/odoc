
# Module `With8.M`

```ocaml
module type S = With5.S
```
```ocaml
module N : module type of struct include With5.N end with type t = With5.N.t
```