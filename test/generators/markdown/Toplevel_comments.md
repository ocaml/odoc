
# Module `Toplevel_comments`

A doc comment at the beginning of a module is considered to be that module's doc.

```
module type T = sig ... end
```
Doc of `T`, part 1\.

```
module Include_inline : sig ... end
```
Doc of `T`, part 2\.

```
module Include_inline' : sig ... end
```
Doc of `Include_inline`, part 1\.

```
module type Include_inline_T = sig ... end
```
Doc of `T`, part 2\.

```
module type Include_inline_T' = sig ... end
```
Doc of `Include_inline_T'`, part 1\.

```
module M : sig ... end
```
Doc of `M`

```
module M' : sig ... end
```
Doc of `M'` from outside

```
module M'' : sig ... end
```
Doc of `M''`, part 1\.

```
module Alias : T
```
Doc of `Alias`.

```
class c1 : int -> object ... end
```
Doc of `c1`, part 1\.

```
class type  ct = object ... end
```
Doc of `ct`, part 1\.

```
class c2 : ct
```
Doc of `c2`.

```
module Ref_in_synopsis : sig ... end
```
[`t`](./Toplevel_comments-Ref_in_synopsis.md#type-t).

```
module Comments_on_open : sig ... end
```