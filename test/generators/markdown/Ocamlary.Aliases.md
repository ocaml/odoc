Ocamlary

Aliases

Module `Ocamlary.Aliases`

Let's imitate jst's layout.

<a id="module-Foo"></a>

###### module [Foo](Ocamlary.Aliases.Foo.md)

<a id="module-A'"></a>

###### module A' =

> [Foo.A](Ocamlary.Aliases.Foo.A.md)

<a id="type-tata"></a>

###### type tata =

> [Foo.A.t](Ocamlary.Aliases.Foo.A.md#type-t)

<a id="type-tbtb"></a>

###### type tbtb =

> [Foo.B.t](Ocamlary.Aliases.Foo.B.md#type-t)

<a id="type-tete"></a>

###### type tete

<a id="type-tata'"></a>

###### type tata' =

> [A'.t](Ocamlary.Aliases.Foo.A.md#type-t)

<a id="type-tete2"></a>

###### type tete2 =

> [Foo.E.t](Ocamlary.Aliases.Foo.E.md#type-t)

<a id="module-Std"></a>

###### module [Std](Ocamlary.Aliases.Std.md)

<a id="type-stde"></a>

###### type stde =

> [Std.E.t](Ocamlary.Aliases.Foo.E.md#type-t)

### include of Foo

---

Just for giggle, let's see what happens when we include
[`Foo`](Ocamlary.Aliases.Foo.md).

<a id="module-A"></a>

###### module A =

> [Foo.A](Ocamlary.Aliases.Foo.A.md)

<a id="module-B"></a>

###### module B =

> [Foo.B](Ocamlary.Aliases.Foo.B.md)

<a id="module-C"></a>

###### module C =

> [Foo.C](Ocamlary.Aliases.Foo.C.md)

<a id="module-D"></a>

###### module D =

> [Foo.D](Ocamlary.Aliases.Foo.D.md)

<a id="module-E"></a>

###### module [E](Ocamlary.Aliases.E.md)

<a id="type-testa"></a>

###### type testa =

> [A.t](Ocamlary.Aliases.Foo.A.md#type-t)

And also, let's refer to [`A.t`](Ocamlary.Aliases.Foo.A.md#type-t) and
[`Foo.B.id`](Ocamlary.Aliases.Foo.B.md#val-id)

<a id="module-P1"></a>

###### module [P1](Ocamlary.Aliases.P1.md)

<a id="module-P2"></a>

###### module [P2](Ocamlary.Aliases.P2.md)

<a id="module-X1"></a>

###### module X1 =

> [P2.Z](Ocamlary.Aliases.P1.Y.md)

<a id="module-X2"></a>

###### module X2 =

> [P2.Z](Ocamlary.Aliases.P1.Y.md)

<a id="type-p1"></a>

###### type p1 =

> [X1.t](Ocamlary.Aliases.P1.Y.md#type-t)

<a id="type-p2"></a>

###### type p2 =

> [X2.t](Ocamlary.Aliases.P1.Y.md#type-t)
