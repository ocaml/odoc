
# Module `Nested`

This comment needs to be here before \#235 is fixed.


## Module

```ocaml
module X : sig ... end
```
This is module X.


## Module type

```ocaml
module type Y = sig ... end
```
This is module type Y.


## Functor

```ocaml
module F (Arg1 : Y) (Arg2 : sig ... end) : sig ... end
```
This is a functor F.


## Class

```ocaml
class virtual z : object ... end
```
This is class z.

```ocaml
class virtual inherits : object ... end
```