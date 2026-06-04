
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
type t_any : any
```
Layout `any`.

```ocaml
type t_value_or_null : value_or_null
```
Layout `value_or_null`.

```ocaml
type t_float64 : float64
```
Layout `float64`.

```ocaml
type t_float32 : float32
```
Layout `float32`.

```ocaml
type t_word : word
```
Layout `word`.

```ocaml
type t_bits64 : bits64
```
Layout `bits64`.

```ocaml
type t_bits32 : bits32
```
Layout `bits32`.

```ocaml
type t_vec128 : vec128
```
Layout `vec128`.

```ocaml
type t_void : void
```
Layout `void`.


## Kind abbreviations

```ocaml
type t_value
```
`value` is the default kind, so the annotation is not rendered.

```ocaml
type t_immediate : immediate
```
Kind abbreviation `immediate`.

```ocaml
type t_immediate64 : immediate64
```
Kind abbreviation `immediate64`.

```ocaml
type t_immutable_data : immutable_data
```
Kind abbreviation `immutable_data`.

```ocaml
type t_sync_data : sync_data
```
Kind abbreviation `sync_data`.

```ocaml
type t_mutable_data : mutable_data
```
Kind abbreviation `mutable_data`.


## Kind annotations with modalities

```ocaml
type t_portable : value mod portable
```
Kind annotation with a modality.

```ocaml
type t_contended : value mod contended
```
Kind annotation with a different modality.

```ocaml
type t_multi_mod : value mod portable contended
```
Kind annotation with multiple modalities.

```ocaml
type t_everything : float64 mod everything
```
The `everything` bounds abbreviation.


## Kind annotations on parameterized types

```ocaml
type ('a : immediate) imm_param
```
A type parameter with a kind constraint.

```ocaml
type ('a : float64) float_param
```
A type parameter with a different kind constraint.

```ocaml
type (('a : immediate), ('b : float64)) multi_kind
```
Multiple kind-constrained parameters.


## Kind annotations with `with` constraints

```ocaml
type 'a t_with : immediate with 'a
```
Kind annotation with a `with` constraint.

```ocaml
type 'a t_with_modalities : immutable_data with 'a @@ portable contended
```
Kind annotation with a `with` constraint and modalities.


## Kind annotations on type aliases

```ocaml
type t_alias : immediate = int
```
Has both a kind annotation and a manifest.


## Kind-constrained polymorphism in values

```ocaml
val poly_immediate : ('a : immediate). 'a -> 'a
```
Kind constraint on a polymorphic type variable.

```ocaml
val poly_float64 : ('a : float64). 'a -> 'a
```
Kind constraint on a polymorphic type variable with a different kind.


## Parenthesization of product kinds

```ocaml
type t_many_modalities : value mod global aliased many contended portable forkable unyielding immutable stateless external_
```
A `mod` kind annotation with many modalities.

```ocaml
type t_outer_mod : (float64 & immediate) mod portable
```
Should render as `(float64 & immediate) mod portable`.

```ocaml
type t_inner_mod : float64 & (immediate mod portable)
```
Should render as `float64 & (immediate mod portable)`.


## Kind abbreviations

```ocaml
type t_abbrev : my_abbrev mod immutable
```
A type with an abbreviated kind.

```ocaml
val add : bool -> int -> int -> int [@@zero_alloc]
```
Zero allocation bindings have an extension attribute attached. See https://oxcaml.org/documentation/miscellaneous-extensions/zero\_alloc\_check/

```ocaml
val add_opt : bool -> int -> int -> int [@@zero_alloc opt]
```
Like `add` but with an `opt` attribute.

```ocaml
val add_strict : bool -> int -> int -> int [@@zero_alloc strict]
```
Like `add` but with a `strict` attribute.

```ocaml
val add_strict_opt : bool -> int -> int -> int [@@zero_alloc strict opt]
```
Like `add` but with a `strict` and `opt` attributes.

```ocaml
val add_opt_strict : bool -> int -> int -> int [@@zero_alloc strict opt]
```
Like `add` but with a `strict` and `opt` attributes in reverse order.

```ocaml
val alt_syntax : int -> int [@@zero_alloc]
```
Alternative syntax for zero alloc annotation

```ocaml
val curried_zero_alloc : int -> int -> int [@@zero_alloc arity 1]
```
Function that returns a function that is `zero_alloc`.
