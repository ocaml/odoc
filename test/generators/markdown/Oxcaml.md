
# Module `Oxcaml`

```ocaml
val f : int -> ('a. 'a -> 'a) -> unit
```
Polymorphic arguments require parentheses


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


## Modalities

```ocaml
type opaque
```
```ocaml
type modalities_all = {
  f_global : opaque; (* Locality modality. *)
  f_local : opaque; (* Locality modality (identity, not rendered). *)
  f_unique : opaque; (* Uniqueness modality. *)
  f_aliased : opaque; (* Uniqueness modality (identity, not rendered). *)
  f_many : opaque; (* Linearity modality. *)
  f_once : opaque; (* Linearity modality (identity, not rendered). *)
  f_portable : opaque; (* Portability modality. *)
  f_nonportable : opaque; (* Portability modality (identity, not rendered). *)
  f_uncontended : opaque; (* Contention modality (identity, not rendered). *)
  f_contended : opaque; (* Contention modality. *)
  f_unyielding : opaque; (* Yield modality. *)
  f_yielding : opaque; (* Yield modality (identity, not rendered). *)
  f_forkable : opaque; (* Fork modality. *)
  f_unforkable : opaque; (* Fork modality (identity, not rendered). *)
  f_stateless : opaque; (* Statefulness modality. *)
  f_stateful : opaque; (* Statefulness modality (identity, not rendered). *)
  f_immutable : opaque; (* Visibility modality. *)
  f_read_write : opaque; (* Visibility modality (identity, not rendered). *)
  f_no_modality : opaque; (* No modality, for reference. *)
}
```

## Multiple modalities on a field

```ocaml
type modalities_multi = {
  a : opaque; (* Field with global portable modalities. *)
}
```

## Modalities on tuple and function fields

```ocaml
type modalities_tuple = {
  f : int * string; (* Tuple field with modality. *)
}
```
```ocaml
type modalities_fn = {
  g : int -> int; (* Function field with modality. *)
}
```

## Modalities on constructor arguments

```ocaml
type modalities_cstr = 
  | A of string (* Constructor argument with global modality. *)
  | B of int -> int (* Function constructor argument with modality. *)
  | C of int * string (* Tuple constructor argument with modality. *)
  | D of int * string (* Per-element modalities in a constructor tuple. *)
  | E (* Constant constructor. *)
```

## Modalities on values

```ocaml
val portable_fn : int -> int
```
Value with `portable` modality.


## Modalities on module declarations

```ocaml
module type S = sig ... end
```
```ocaml
module M1 : S
```
Module without modality.

```ocaml
module M2 : S
```
Module with `portable` modality. The modality is applied to all value members of `M2`.

```ocaml
module M3 : sig ... end
```
`contended` modality applied to all definitions in the module.
