
# Module `Oxcaml.M_shadow_value`

```ocaml
type t_unannotated
```
```ocaml
val poly_unannotated : 'a. 'a -> 'a
```
The compiler implicitly adds the default kind `value`, but it should not be confused with the kind abbreviation above.

```ocaml
type t
```
`value` here is the abbreviation above, not the built-in default. Kinds are rendered as written, so it cannot be told apart from the default and is not rendered.
