
# Module `Oxcaml`

```ocaml
val f : int -> ('a. 'a -> 'a) -> unit
```
Polymorphic arguments require parentheses

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
