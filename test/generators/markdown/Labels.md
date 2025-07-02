
# Module `Labels`


## Attached to unit


## Attached to nothing

```
module A : sig ... end
```
```
type t
```
Attached to type

```
val f : t
```
Attached to value

```
val e : unit -> t
```
Attached to external

```
module type S = sig ... end
```
```
class c : object ... end
```
```
class type  cs = object ... end
```
```
exception E
```
Attached to exception

```
type x = ..
```
```
type x += 
```
```
| X
```
```

```
Attached to extension

```
module S := A
```
Attached to module subst

```
type s := t
```
Attached to type subst

```
type u = 
```
```
| A'
```
Attached to constructor

```

```
```
type v = {
```
`f : t;`
Attached to field

```
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