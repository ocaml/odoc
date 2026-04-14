
# Module `Oxcaml`

```ocaml
val f : int -> ('a. 'a -> 'a) -> unit
```
Polymorphic arguments require parentheses


## Kind annotations on abstract types

These type declarations have kind annotations that constrain their layout. Currently, odoc does not render the kind annotations.

```ocaml
type t_immediate
```
Should be `type t_immediate : immediate`

```ocaml
type t_value
```
Should be `type t_value : value`

```ocaml
type t_any
```
Should be `type t_any : any`

```ocaml
type t_float64
```
Should be `type t_float64 : float64`

```ocaml
type t_bits32
```
Should be `type t_bits32 : bits32`

```ocaml
type t_bits64
```
Should be `type t_bits64 : bits64`

```ocaml
type t_word
```
Should be `type t_word : word`

```ocaml
type t_void
```
Should be `type t_void : void`


## Kind annotations with modalities

```ocaml
type t_portable
```
Should be `type t_portable : value mod portable`

```ocaml
type t_contended
```
Should be `type t_contended : value mod contended`

```ocaml
type t_multi_mod
```
Should be `type t_multi_mod : value mod portable contended`


## Kind annotations on parameterized types

```ocaml
type 'a imm_param
```
The kind constraint on `'a` is not rendered.

```ocaml
type 'a float_param
```
The kind constraint on `'a` is not rendered.

```ocaml
type ('a, 'b) multi_kind
```
Multiple kind-constrained parameters.


## Kind annotations with `with` constraints

```ocaml
type 'a t_with
```
Should be `type 'a t_with : immediate with 'a`


## Kind annotations on type aliases

```ocaml
type t_alias = int
```
Has both a kind annotation and a manifest.


## Kind-constrained polymorphism in values

```ocaml
val poly_immediate : 'a. 'a -> 'a
```
The kind constraint on `'a` is not rendered.

```ocaml
val poly_float64 : 'a. 'a -> 'a
```
The kind constraint on `'a` is not rendered.
