
# Module `Oxcaml`

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
val f : int -> int [@@zero_alloc]
```
Alternative syntax for zero alloc annotation
