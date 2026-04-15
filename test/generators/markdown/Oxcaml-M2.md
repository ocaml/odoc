
# Module `Oxcaml.M2`

Module with `portable` modality. The modality is applied to all value members of `M2`.

```ocaml
val x : int @@ portable
```
```ocaml
val f : string -> bool @@ portable
```
```ocaml
type s : immutable_data = {
  a : int;
}
```