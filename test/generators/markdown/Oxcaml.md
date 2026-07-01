
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


## Modes

```ocaml
val mode_arg : int -> int
```
Mode on a function argument.

```ocaml
val mode_ret : int -> int
```
Mode on a function return.

```ocaml
val mode_both : int -> int
```
Modes on both argument and return.

```ocaml
val mode_multi : string -> string
```
Multiple modes on argument and return.

```ocaml
val mode_labeled : x:int -> int
```
Mode on a labeled argument.

```ocaml
val mode_optional : ?x:int -> unit -> int
```
Mode on an optional argument.

```ocaml
val mode_higher_order : ('a -> 'b) -> 'a -> 'b
```
Mode on a higher-order function argument.

```ocaml
val mode_arrow_result : int -> int -> int
```
Mode on a result that is itself an arrow. The arrow must be parenthesized so the mode does not appear to bind to the inner return type.


### Curry-implied result modes

Closing over an argument constrains the partial-application closure across several axes, not just locality. When the result mode is the one currying implies from the argument, it is suppressed (as the compiler does).

```ocaml
val curry_once : (int -> int) -> int -> int
```
`once` argument: the implied `once` result mode is suppressed.

```ocaml
val curry_portable : (int -> int) -> int -> int
```
`portable` argument: the implied result mode is suppressed.

```ocaml
val curry_contended : (int -> int) -> int -> int
```
`contended` argument: the implied result mode is suppressed.


### Result modes that are kept

A result mode is only suppressed when it is exactly the one currying implies. An explicit mode on a different axis is kept (and the arrow result is parenthesized).

```ocaml
val keep_portable : int -> int -> int
```
`portable` on the result is not implied by a `local` argument, so it is kept.

```ocaml
val keep_once : int -> int -> int
```
`once` on the result is not implied by a `local` argument, so it is kept.

```ocaml
val keep_over_once : (int -> int) -> int -> int
```
The curry-implied `once` is suppressed, but the explicit `portable` is kept.

```ocaml
val keep_over_local : (int -> int) -> int -> int
```
The curry-implied `local` is suppressed, but the explicit `portable` is kept.

```ocaml
val keep_portable_over_nonportable : (int -> int) -> int -> int
```
The `nonportable` argument mode is the default and dropped, while the explicit `portable` result, not implied by currying, is kept.


### All mode axes

```ocaml
val mode_global : int -> unit
```
Locality mode (legacy, not rendered).

```ocaml
val mode_local : int -> unit
```
Locality mode.

```ocaml
val mode_aliased : int -> unit
```
Uniqueness mode (legacy, not rendered).

```ocaml
val mode_unique : int -> unit
```
Uniqueness mode.

```ocaml
val mode_many : int -> unit
```
Linearity mode (legacy, not rendered).

```ocaml
val mode_once : int -> unit
```
Linearity mode.

```ocaml
val mode_portable : int -> unit
```
Portability mode.

```ocaml
val mode_shareable : int -> unit
```
Portability mode (intermediate value).

```ocaml
val mode_nonportable : int -> unit
```
Portability mode (legacy, not rendered).

```ocaml
val mode_uncontended : int -> unit
```
Contention mode (legacy, not rendered).

```ocaml
val mode_shared : int -> unit
```
Contention mode.

```ocaml
val mode_contended : int -> unit
```
Contention mode.

```ocaml
val mode_yielding : int -> unit
```
Yield mode.

```ocaml
val mode_unyielding : int -> unit
```
Yield mode (legacy, not rendered).

```ocaml
val mode_forkable : int -> unit
```
Fork mode (identity on a non-`local` argument, not rendered).

```ocaml
val mode_local_forkable : int -> unit
```
Fork mode, rendered because the argument is also `local`.

```ocaml
val mode_unforkable : int -> unit
```
Fork mode.

```ocaml
val mode_local_unforkable : int -> unit
```
Fork mode (identity for a `local` argument, not rendered).

```ocaml
val mode_stateless : int -> unit
```
Statefulness mode.

```ocaml
val mode_observing : int -> unit
```
Statefulness mode.

```ocaml
val mode_stateful : int -> unit
```
Statefulness mode (identity when `portability` is at its default, not rendered).

```ocaml
val mode_immutable : int -> unit
```
Visibility mode.

```ocaml
val mode_read : int -> unit
```
Visibility mode.

```ocaml
val mode_read_write : int -> unit
```
Visibility mode (legacy, not rendered).

```ocaml
val mode_static : int -> unit
```
Staticity mode.

```ocaml
val mode_dynamic : int -> unit
```
Staticity mode (legacy, not rendered).


### Cross-axis suppression

Some axes have a default value that is implied by another axis; the implied value is suppressed when rendering.

```ocaml
val mode_local_yielding : int -> unit
```
`yielding` is the default for `local`, so it is not rendered.

```ocaml
val mode_local_unyielding : int -> unit
```
`unyielding` is non-default for `local`, so it is rendered.

```ocaml
val mode_immutable_contended : int -> unit
```
`contended` is the default for `immutable`, so it is not rendered.

```ocaml
val mode_immutable_uncontended : int -> unit
```
`uncontended` is non-default for `immutable`, so it is rendered.

```ocaml
val mode_stateless_portable : int -> unit
```
`portable` is the default for `stateless`, so it is not rendered.

```ocaml
val mode_stateful_portable : int -> unit
```
`portable` is non-default for `stateful`, so it is rendered.


### Modes in type definitions

```ocaml
type mode_alias = int -> int
```
Type alias for an arrow with a mode on its argument.

```ocaml
type mode_record = {
  fn : int -> int; (* Record field whose type is an arrow with a mode. *)
  fn_both : int -> int; (* Arrow field with modes on both sides. *)
  mutable mfn : int -> int; (* Mutable arrow field with a mode. *)
}
```
```ocaml
type mode_cstr = 
  | Mc_arrow of int -> int (* Constructor argument is a parenthesized arrow with a mode. *)
  | Mc_nested of (int -> int) -> unit (* Nested arrow: higher-order with a mode on the inner argument. *)
  | Mc_gadt : ('a -> 'a) -> mode_cstr (* GADT constructor *)
```