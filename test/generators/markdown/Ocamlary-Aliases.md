
# Module `Ocamlary.Aliases`

Let's imitate jst's layout.

```
module Foo : sig ... end
```
```
module A' = Foo.A
```
```
type tata = Foo.A.t
```
```
type tbtb = Foo.B.t
```
```
type tete
```
```
type tata' = A'.t
```
```
type tete2 = Foo.E.t
```
```
module Std : sig ... end
```
```
type stde = Std.E.t
```

#### include of Foo

Just for giggle, let's see what happens when we include [`Foo`](./Ocamlary-Aliases-Foo.md).

```
module A = Foo.A
```
```
module B = Foo.B
```
```
module C = Foo.C
```
```
module D = Foo.D
```
```
module E : sig ... end
```
```
type testa = A.t
```
And also, let's refer to [`A.t`](./Ocamlary-Aliases-Foo-A.md#type-t) and [`Foo.B.id`](./Ocamlary-Aliases-Foo-B.md#val-id)

```
module P1 : sig ... end
```
```
module P2 : sig ... end
```
```
module X1 = P2.Z
```
```
module X2 = P2.Z
```
```
type p1 = X1.t
```
```
type p2 = X2.t
```