Kind abbreviations can be referred to with the [kind-] prefix.

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ odoc compile a.cmti
  File "a.mli", line 22, characters 8-14:
  Warning: Expected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', a path, or an unqualified reference.
  $ odoc compile -I . b.cmti
  $ odoc link -I . a.odoc
  File "a.mli", line 21, characters 6-19:
  Warning: Failed to resolve reference unresolvedroot(value) Couldn't find "value"
  File "a.mli", line 20, characters 6-16:
  Warning: Failed to resolve reference unresolvedroot(mk) Couldn't find "mk"
  $ odoc link -I . b.odoc
  $ odoc markdown-generate -o md a.odocl
  $ odoc markdown-generate -o md b.odocl

The references resolve to the declarations' anchors. The kind [k] and the type
[k] have distinct anchors:

  $ cat md/A.md
  
  # Module `A`
  
  ```ocaml
  kind_ k = value mod portable
  ```
  A kind abbreviation.
  
  ```ocaml
  type k
  ```
  A type with the same name as the kind.
  
  ```ocaml
  module M : sig ... end
  ```
  ```ocaml
  module type S = sig ... end
  ```
  ```ocaml
  type t
  ```
  References:
  
  - [`k`](./#kind-k) at the toplevel, distinct from [`k`](./#type-k);
  - [`M.mk`](./A-M.md#kind-mk) and [`M.mk`](./A-M.md#kind-mk) in a submodule;
  - [`S.sk`](./A-module-type-S.md#kind-sk) in a module type;
  - [with text](./#kind-k);
  - `mk` is not in scope;
  - `value` is built in, it has no declaration;
  - `kind-k.foo` is not a valid reference.

References from another unit:

  $ cat md/B.md
  
  # Module `B`
  
  ```ocaml
  type t
  ```
  [`A.k`](./A.md#kind-k) and [`A.M.mk`](./A-M.md#kind-mk) in another unit.
