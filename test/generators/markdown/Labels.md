
# Module `Labels`


## Attached to unit


## Attached to nothing

```ocaml
module A : sig ... end
```
```ocaml
type t
```
Attached to type

```ocaml
val f : t
```
Attached to value

```ocaml
val e : unit -> t
```
Attached to external

```ocaml
module type S = sig ... end
```
```ocaml
class c : object ... end
```
```ocaml
class type  cs = object ... end
```
```ocaml
exception E
```
Attached to exception

```ocaml
type x = ..
```
```ocaml
type x += 
  | X
```
Attached to extension

```ocaml
module S := A
```
Attached to module subst

```ocaml
type s := t
```
Attached to type subst

```ocaml
type u = 
  | A' (* Attached to constructor *)
```
```ocaml
type v = {
  f : t; (* Attached to field *)
}
```
Testing that labels can be referenced

- [Attached to unit](./#L1)
- [Attached to nothing](./#L2)
- [Attached to module](./#L3)
- [Attached to type](./#L4)
- [Attached to value](./#L5)
- [Attached to module type](./#L6)
- [Attached to class](./#L7)
- [Attached to class type](./#L8)
- [Attached to exception](./#L9)
- [Attached to extension](./#L10)
- [Attached to module subst](./#L11)
- [Attached to type subst](./#L12)
- [Attached to constructor](./#L13)
- [Attached to field](./#L14)