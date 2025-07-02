
# Module `Bugs`

```
type 'a opt = 'a option
```
```
val foo : ?bar:'a -> unit -> unit
```
Triggers an assertion failure when [https://github.com/ocaml/odoc/issues/101](https://github.com/ocaml/odoc/issues/101) is not fixed.

```
val repeat : 'a -> 'b -> 'a * 'b * 'a * 'b
```
Renders as `val repeat : 'a -> 'b -> 'c * 'd * 'e * 'f` before https://github.com/ocaml/odoc/pull/1173
