
# Module `Module_inline`

Tests for the `\@inline` attribute on module declarations.

When a module declaration carries `\@inline` in its doc comment, its contents are rendered directly on the parent page rather than on a separate sub-page. This mirrors how `include … (**\@inline*)` works.

```ocaml
module Normal : sig ... end
```
A normal module — contents appear on a separate page (default behaviour).

An inlined module — contents appear directly on this page.

```ocaml
type t
```
```ocaml
val create : unit -> t
```
```ocaml
module Alias = Normal
```
A module without an inline signature is unaffected by `\@inline`.

Nested: the outer module is inlined; inner sub-modules still get their own pages unless they are also marked `\@inline`.

```ocaml
module Inner : sig ... end
```
Nested module without `\@inline` — separate page.

```ocaml
val x : int
```