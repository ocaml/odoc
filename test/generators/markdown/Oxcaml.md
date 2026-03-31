
# Module `Oxcaml`

```ocaml
val f : int -> ('a. 'a -> 'a) -> unit
```
Polymorphic arguments require parentheses

Unboxed types have a trailing hash '\#'

```ocaml
type pt = {
  x : int;
  y : float32#;
}
```
```ocaml
type segment = {
  start : pt#;
  stop : pt#;
}
```