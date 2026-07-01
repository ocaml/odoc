
# Module `Oxcaml_impl`

```ocaml
val add : bool -> int -> int -> int [@@zero_alloc]
```
```ocaml
module To_be_included : sig ... end
```
```ocaml
module Including : sig ... end
```
```ocaml
type opaque
```

## Modalities


### Modalities on record fields

```ocaml
type modalities_record = {
  f_global : opaque @@ global; (* Locality modality. *)
  f_portable : opaque @@ portable; (* Portability modality. *)
  f_multi : opaque @@ global portable; (* Multiple modalities. *)
  f_plain : opaque; (* No modality, for reference. *)
}
```

### Modalities on constructor arguments

```ocaml
type modalities_variant = 
  | A of string @@ global (* Constructor argument with global modality. *)
  | B of int -> int @@ portable (* Function constructor argument with modality. *)
  | C of int @@ portable * string @@ global (* Per-element modalities in a constructor tuple. *)
```

## Modes


### Modes in type definitions

```ocaml
type mode_alias = int @ local -> int
```
Type alias for an arrow with a mode on its argument.

```ocaml
type mode_record = {
  fn : int @ local -> int; (* Field whose type is an arrow with a mode. *)
  fn_both : int @ local -> int @ local; (* Modes on both sides. *)
}
```
```ocaml
type mode_cstr = 
  | Mc_arrow of int @ local -> int (* Constructor argument is a parenthesized arrow with a mode. *)
  | Mc_nested of (int @ local -> int) -> unit (* Nested arrow: higher-order with a mode on the inner argument. *)
```

### Modes on values

```ocaml
val mode_arg : int @ local -> int
```
Mode on a function argument, via a type annotation.

```ocaml
val mode_multi : string @ local once -> string @ local once
```
Multiple modes on argument and return.
