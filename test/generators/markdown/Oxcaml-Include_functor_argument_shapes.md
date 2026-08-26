
# Module `Oxcaml.Include_functor_argument_shapes`

```ocaml
module type Arg = sig ... end
```
Everything the expansion of the functor can inherit from its argument: types, including parameterised and anonymously parameterised ones, submodules, and module types.

```ocaml
module Make (T : Arg) : sig ... end
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
