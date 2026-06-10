
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


## Zero alloc

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


## Modalities

```ocaml
type opaque
```
```ocaml
type modalities_all = {
  f_global : opaque @@ global; (* Locality modality. *)
  f_local : opaque; (* Locality modality (local is not rendered). *)
  f_unique : opaque; (* Uniqueness modality (unique is not rendered). *)
  f_aliased : opaque @@ aliased; (* Uniqueness modality. *)
  f_many : opaque @@ many; (* Linearity modality. *)
  f_once : opaque; (* Linearity modality (once is not rendered). *)
  f_portable : opaque @@ portable; (* Portability modality. *)
  f_nonportable : opaque; (* Portability modality (nonportable is not rendered). *)
  f_uncontended : opaque; (* Contention modality (uncontended is not rendered). *)
  f_contended : opaque @@ contended; (* Contention modality. *)
  f_unyielding : opaque @@ unyielding; (* Yield modality. *)
  f_yielding : opaque; (* Yield modality (yielding is not rendered). *)
  f_forkable : opaque @@ forkable; (* Fork modality. *)
  f_unforkable : opaque; (* Fork modality (unforkable is not rendered). *)
  f_stateless : opaque @@ stateless; (* Statefulness modality. *)
  f_stateful : opaque; (* Statefulness modality (stateful is not rendered). *)
  f_immutable : opaque @@ immutable; (* Visibility modality. *)
  f_read_write : opaque; (* Visibility modality (read_write is not rendered). *)
  f_no_modality : opaque; (* No modality, for reference. *)
}
```

## Multiple modalities on a field

```ocaml
type modalities_multi = {
  a : opaque @@ global portable; (* Field with global portable modalities. *)
}
```

## Modalities on tuple and function fields

```ocaml
type modalities_tuple = {
  f : int * string @@ portable; (* Tuple field with modality. *)
}
```
```ocaml
type modalities_fn = {
  g : int -> int @@ portable; (* Function field with modality. *)
}
```

## Modalities on constructor arguments

```ocaml
type modalities_cstr = 
  | A of string @@ global (* Constructor argument with global modality. *)
  | B of int -> int @@ portable (* Function constructor argument with modality. *)
  | C of int * string @@ portable (* Tuple constructor argument with modality. *)
  | D of int @@ portable * string @@ global (* Per-element modalities in a constructor tuple. *)
  | E of {
    x : int @@ portable;
    y : string @@ global;
  } (* Per-element modalities in a constructor record. *)
  | F (* Constant constructor. *)
```
```ocaml
type 'a modalities_gadt = 
  | A : string @@ global -> [ `a ] modalities_gadt (* Constructor argument with global modality. *)
  | B : (int -> int) @@ portable -> [ `b ] modalities_gadt (* Function constructor argument with modality. *)
  | C : int * string @@ portable -> [ `c ] modalities_gadt (* Tuple constructor argument with modality. *)
  | D : int @@ portable * string @@ global -> [ `d ] modalities_gadt (* Per-element modalities in a constructor tuple. *)
  | E : {
    x : int @@ portable;
    y : string @@ global;
  } -> [ `e ] modalities_gadt (* Per-element modalities in a constructor record. *)
  | F : [ `f ] modalities_gadt (* Constant constructor. *)
```

### Modalities on values

```ocaml
val portable_fn : int -> int @@ portable
```
Value with `portable` modality.


### Modalities on module declarations

```ocaml
module type S = sig ... end
```
```ocaml
module M1 : S
```
Module without modality.

```ocaml
module M2 : sig ... end
```
Module with `portable` modality. The modality is applied to all value members of `M2`.

```ocaml
module M3 : sig ... end
```
`contended` modality applied to all definitions in the module, except the ones which have already specified this axis.
