
# Module `Oxcaml.Include_functor_argument_shapes`

```ocaml
module type Arg = sig ... end
```
Everything the expansion of the functor can inherit from its argument: types, including parameterised and anonymously parameterised ones, submodules, and module types.

```ocaml
module Make (T : Arg) : sig ... end
```
```ocaml
class class_type : object ... end
```
```ocaml
class class_ : class_type
```
```ocaml
type t
```
```ocaml
type 'a p
```
```ocaml
type _ anon
```
```ocaml
module X : sig ... end
```
```ocaml
module type S = sig ... end
```
```ocaml
val via_module_type_include : unit
```
```ocaml
val via_module_include : unit
```
```ocaml
type included = t
```
No parameters, so the alias odoc puts in the synthetic module is a bare `type t = t`.

```ocaml
type applied = int p
```
A named parameter, which the alias threads through as `type 'a p = 'a p`.

```ocaml
type anonymous = bool anon
```
An anonymous parameter: `_` gives the alias no name to mention on the right, so one is invented, as `type 'a0 anon = 'a0 anon`.

```ocaml
type from_module = X.v
```
Reached through a submodule of the argument, aliased as `module X = X`.

```ocaml
module type Reexported = S
```
A module type of the argument, aliased as a path to it.

```ocaml
class output_class_via_type : BODY__3.class_type
```
A class whose type comes from an explicitely named class type

```ocaml
class output_class_via_name : BODY__3.class_
```
A class whose type comes from the name of a class

```ocaml
module Aliased : sig ... end
```
The input module itself. It should contain everything from the top level module
