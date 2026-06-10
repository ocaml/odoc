
# Module `Oxcaml.M1`

Module without modality.

```ocaml
type s = {
  a : int;
}
```
```ocaml
val x : int
```
```ocaml
val f : string -> bool
```
```ocaml
val portable : string -> string array @@ portable
```
```ocaml
val contended : string -> bytes @@ contended
```
`uncontended` and `nonportable` are the defaults (not rendered).

```ocaml
val uncontended : string -> bytes
```
```ocaml
val nonportable : string -> string array
```