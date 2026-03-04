
# Module `Toplevel_comments`

A doc comment at the beginning of a module is considered to be that module's doc.

```ocaml
module type T = sig ... end
```
Doc of `T`, part 1\.

```ocaml
module Include_inline : sig ... end
```
Doc of `T`, part 2\.

```ocaml
module Include_inline' : sig ... end
```
Doc of `Include_inline`, part 1\.

```ocaml
module type Include_inline_T = sig ... end
```
Doc of `T`, part 2\.

```ocaml
module type Include_inline_T' = sig ... end
```
Doc of `Include_inline_T'`, part 1\.

```ocaml
module M : sig ... end
```
Doc of `M`

```ocaml
module M' : sig ... end
```
Doc of `M'` from outside

```ocaml
module M'' : sig ... end
```
Doc of `M''`, part 1\.

```ocaml
module Alias : T
```
Doc of `Alias`.

```ocaml
class c1 : int -> object ... end
```
Doc of `c1`, part 1\.

```ocaml
class type  ct = object ... end
```
Doc of `ct`, part 1\.

```ocaml
class c2 : ct
```
Doc of `c2`.

```ocaml
module Ref_in_synopsis : sig ... end
```
[`t`](./Toplevel_comments-Ref_in_synopsis.md#type-t).

```ocaml
module Comments_on_open : sig ... end
```