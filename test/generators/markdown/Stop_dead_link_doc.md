
# Module `Stop_dead_link_doc`

```ocaml
module Foo : sig ... end
```
```ocaml
type foo = 
  | Bar of Foo.t
```
```ocaml
type bar = 
  | Bar of {
    field : Foo.t;
  }
```
```ocaml
type foo_ = 
  | Bar_ of int * Foo.t * int
```
```ocaml
type bar_ = 
  | Bar__ of Foo.t option
```
```ocaml
type another_foo = 
  | Bar of Another_Foo.t
```
```ocaml
type another_bar = 
  | Bar of {
    field : Another_Foo.t;
  }
```
```ocaml
type another_foo_ = 
  | Bar_ of int * Another_Foo.t * int
```
```ocaml
type another_bar_ = 
  | Bar__ of Another_Foo.t option
```