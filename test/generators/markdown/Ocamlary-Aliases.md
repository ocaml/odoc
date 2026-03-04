
# Module `Ocamlary.Aliases`

Let's imitate jst's layout.

```ocaml
module Foo : sig ... end
```
```ocaml
module A' = Foo.A
```
```ocaml
type tata = Foo.A.t
```
```ocaml
type tbtb = Foo.B.t
```
```ocaml
type tete
```
```ocaml
type tata' = A'.t
```
```ocaml
type tete2 = Foo.E.t
```
```ocaml
module Std : sig ... end
```
```ocaml
type stde = Std.E.t
```

#### include of Foo

Just for giggle, let's see what happens when we include [`Foo`](./Ocamlary-Aliases-Foo.md).

```ocaml
module A = Foo.A
```
```ocaml
module B = Foo.B
```
```ocaml
module C = Foo.C
```
```ocaml
module D = Foo.D
```
```ocaml
module E : sig ... end
```
```ocaml
type testa = A.t
```
And also, let's refer to [`A.t`](./Ocamlary-Aliases-Foo-A.md#type-t) and [`Foo.B.id`](./Ocamlary-Aliases-Foo-B.md#val-id)

```ocaml
module P1 : sig ... end
```
```ocaml
module P2 : sig ... end
```
```ocaml
module X1 = P2.Z
```
```ocaml
module X2 = P2.Z
```
```ocaml
type p1 = X1.t
```
```ocaml
type p2 = X2.t
```