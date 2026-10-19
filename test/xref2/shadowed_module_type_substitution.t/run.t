The module type cases of [shadowed_substitution.t], which need OCaml 4.13
for [module type S := ...].

A module type substitution hidden by stop comments has an identifier of its
own, so it doesn't replace the module type it substitutes, and a later use
of [S] still resolves:

  $ cat hidden.mli
  (**/**)
  module type S = sig val x : int end
  module type S := S
  (**/**)
  
  module X : S
  $ ocamlc -c -bin-annot hidden.mli
  $ odoc compile hidden.cmti
  $ odoc link hidden.odoc
  $ odoc markdown-generate -o md hidden.odocl
  $ cat md/Hidden.md md/Hidden-X.md
  
  # Module `Hidden`
  
  ```ocaml
  module X : sig ... end
  ```
  # Module `Hidden.X`
  
  ```ocaml
  val x : int
  ```

A reference goes to a real module type rather than to a later substitution
of the same name:

  $ cat refs.mli
  module type S = sig type b end
  module type S := sig end
  
  val s : unit
  (** See {!S} and {!S.b}. *)
  $ ocamlc -c -bin-annot refs.mli
  $ odoc compile refs.cmti
  $ odoc link refs.odoc
  File "refs.mli", line 5, characters 17-23:
  Warning: Failed to resolve reference unresolvedroot(S).b Couldn't find "b"
  $ odoc markdown-generate -o md refs.odocl
  $ cat md/Refs.md
  
  # Module `Refs`
  
  ```ocaml
  module type S := sig ... end
  ```
  ```ocaml
  val s : unit
  ```
  See [`S`](./Refs-module-type-S.md) and `S.b`.
