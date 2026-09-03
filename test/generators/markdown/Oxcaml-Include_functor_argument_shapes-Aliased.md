
# Module `Include_functor_argument_shapes.Aliased`

The input module itself. It should contain everything from the top level module

```ocaml
module type Arg = Arg
```
Everything the expansion of the functor can inherit from its argument: types, including parameterised and anonymously parameterised ones, submodules, and module types.

```ocaml
module Make = Make
```
```ocaml
module type To_include_module_type = sig ... end
```
```ocaml
module To_include_module : sig ... end
```
```ocaml
type t = t
```
```ocaml
type 'a p = 'a p
```
```ocaml
type 'a0 anon = 'a0 anon
```
```ocaml
module X = X
```
```ocaml
module type S = S
```