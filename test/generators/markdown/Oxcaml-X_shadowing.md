
# Module `Oxcaml.X_shadowing`

```ocaml
type t : to_be_shadowed
```
Uses the outer `to_be_shadowed`, declared before the one below, so the link should point outside this module.

```ocaml
kind_ to_be_shadowed = value_or_null mod non_null global
```
Shadows the outer `to_be_shadowed`.
