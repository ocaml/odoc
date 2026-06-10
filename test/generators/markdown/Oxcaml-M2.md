
# Module `Oxcaml.M2`

Module with `portable` modality. The modality is applied to all value members of `M2`.

```ocaml
type s : immutable_data = {
  a : int;
}
```
```ocaml
val x : int @@ portable
```
```ocaml
val f : string -> bool @@ portable
```
```ocaml
val portable : string -> string array @@ portable
```
```ocaml
val contended : string -> bytes @@ portable contended
```
```ocaml
val uncontended : string -> bytes @@ portable
```
```ocaml
val nonportable : string -> string array @@ portable
```