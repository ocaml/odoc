
# Module `Oxcaml.M3`

`contended` modality applied to all definitions in the module, except the ones which have already specified this axis.

```ocaml
type s
```
```ocaml
val f : string -> bool @@ contended
```
```ocaml
val portable : string -> string array @@ portable contended
```
```ocaml
val nonportable : string -> string array @@ contended
```
```ocaml
val uncontended : string -> bytes
```