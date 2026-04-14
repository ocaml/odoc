
# Module `Oxcaml`

```ocaml
val f : int -> ('a. 'a -> 'a) -> unit
```
Polymorphic arguments require parentheses


## Kind annotations on abstract types

```ocaml
type t_immediate : immediate
```
Kind annotation on abstract type.

```ocaml
type t_value
```
`value` is the default kind, so the annotation is not rendered.

```ocaml
type t_any : any
```
A type with the `any` kind.

```ocaml
type t_float64 : float64
```
A type with the `float64` kind.

```ocaml
type t_bits32 : bits32
```
A type with the `bits32` kind.

```ocaml
type t_bits64 : bits64
```
A type with the `bits64` kind.

```ocaml
type t_word : word
```
A type with the `word` kind.

```ocaml
type t_void : void
```
A type with the `void` kind.


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
