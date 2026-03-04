
# Module `Module`

Foo.

```ocaml
val foo : unit
```
The module needs at least one signature item, otherwise a bug causes the compiler to drop the module comment (above). See [https://caml.inria.fr/mantis/view.php?id=7701](https://caml.inria.fr/mantis/view.php?id=7701).

```ocaml
module type S = sig ... end
```
```ocaml
module type S1
```
```ocaml
module type S2 = S
```
```ocaml
module type S3 = S with type t = int and type u = string
```
```ocaml
module type S4 = S with type t := int
```
```ocaml
module type S5 = S with type 'a v := 'a list
```
```ocaml
type ('a, 'b) result
```
```ocaml
module type S6 = S with type ('a, 'b) w := ('a, 'b) result
```
```ocaml
module M' : sig ... end
```
```ocaml
module type S7 = S with module M = M'
```
```ocaml
module type S8 = S with module M := M'
```
```ocaml
module type S9 = module type of M'
```
```ocaml
module Mutually : sig ... end
```
```ocaml
module Recursive : sig ... end
```