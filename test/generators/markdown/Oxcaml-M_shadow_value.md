
# Module `Oxcaml.M_shadow_value`

```ocaml
kind_ value = value mod portable
```
```ocaml
type t_unannotated
```
```ocaml
val poly_unannotated : 'a. 'a -> 'a
```
The compiler implicitly adds the default kind `value`, but it should not be confused with the kind abbreviation above.

```ocaml
type t : value
```
`value` here is the abbreviation above, not the built-in default, so it is rendered and linked.
