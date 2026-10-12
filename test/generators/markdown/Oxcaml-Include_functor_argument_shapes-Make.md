
# Module `Include_functor_argument_shapes.Make`


## Parameters

```ocaml
module T : Arg
```

## Signature

```ocaml
type included = T.t
```
No parameters, so the alias odoc puts in the synthetic module is a bare `type t = t`.

```ocaml
type applied = int T.p
```
A named parameter, which the alias threads through as `type 'a p = 'a p`.

```ocaml
type anonymous = bool T.anon
```
An anonymous parameter: `_` gives the alias no name to mention on the right, so one is invented, as `type 'a0 anon = 'a0 anon`.

```ocaml
type from_module = T.X.v
```
Reached through a submodule of the argument, aliased as `module X = X`.

```ocaml
module type Reexported = T.S
```
A module type of the argument, aliased as a path to it.

```ocaml
class output_class_via_type : T.class_type
```
A class whose type comes from an explicitely named class type

```ocaml
class output_class_via_name : T.class_
```
A class whose type comes from the name of a class

```ocaml
module Aliased = T
```
The input module itself. It should contain everything from the top level module
