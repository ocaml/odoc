
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

## Layouts

```ocaml
type t_any
```
Layout `any`.

```ocaml
type t_value_or_null
```
Layout `value_or_null`.

```ocaml
type t_float64
```
Layout `float64`.

```ocaml
type t_float32
```
Layout `float32`.

```ocaml
type t_word
```
Layout `word`.

```ocaml
type t_bits64
```
Layout `bits64`.

```ocaml
type t_bits32
```
Layout `bits32`.

```ocaml
type t_vec128
```
Layout `vec128`.

```ocaml
type t_void
```
Layout `void`.


## Kind abbreviations

```ocaml
type t_value
```
`value` is the default kind, so the annotation is not rendered.

```ocaml
type t_immediate
```
Kind abbreviation `immediate`.

```ocaml
type t_immediate64
```
Kind abbreviation `immediate64`.

```ocaml
type t_immutable_data
```
Kind abbreviation `immutable_data`.

```ocaml
type t_sync_data
```
Kind abbreviation `sync_data`.

```ocaml
type t_mutable_data
```
Kind abbreviation `mutable_data`.


## Kind annotations with modalities

```ocaml
type t_portable
```
Kind annotation with a modality.

```ocaml
type t_contended
```
Kind annotation with a different modality.

```ocaml
type t_multi_mod
```
Kind annotation with multiple modalities.

```ocaml
type t_everything
```
The `everything` bounds abbreviation.


## Kind annotations on parameterized types

```ocaml
type 'a imm_param
```
A type parameter with a kind constraint.

```ocaml
type 'a float_param
```
A type parameter with a different kind constraint.

```ocaml
type ('a, 'b) multi_kind
```
Multiple kind-constrained parameters.


## Kind annotations with `with` constraints

```ocaml
type 'a t_with
```
Kind annotation with a `with` constraint.

```ocaml
type 'a t_with_modalities
```
Kind annotation with a `with` constraint and modalities.


## Kind annotations on type aliases

```ocaml
type t_alias = int
```
Has both a kind annotation and a manifest.


## Kind-constrained polymorphism in values

```ocaml
val poly_immediate : 'a. 'a -> 'a
```
Kind constraint on a polymorphic type variable.

```ocaml
val poly_float64 : 'a. 'a -> 'a
```
Kind constraint on a polymorphic type variable with a different kind.


## Parenthesization of product kinds

```ocaml
type t_many_modalities
```
A `mod` kind annotation with many modalities.

```ocaml
type t_outer_mod
```
Should render as `(float64 & immediate) mod portable`.

```ocaml
type t_inner_mod
```
Should render as `float64 & (immediate mod portable)`.
