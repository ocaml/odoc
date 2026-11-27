A local substitution, [type t := ...], isn't part of the resulting
signature: it names a type for the items after it, and nothing outside the
signature can refer to it. So it must not shadow a real [type t] declared
before it, and it shouldn't be shown with odoc's internal disambiguated
name either.

Most cases below show odoc's markdown output: it renders each item as
written, and each reference in a doc comment as a link to its target.

This is the shape ppx_template produces (e.g. in Base.Avltree): a declared
[t], then one block per kind, each starting with [type t := ...]. For the
default kind the substitution is [t := t] itself. The declared type's form
doesn't matter: a variant ([t]) and an abstract type ([u]) used to be shown,
with all their uses, under an internal name, and an alias ([v]) vanished
into the substitution line.

  $ cat test.mli
  type 'a t = Empty | Leaf of 'a
  type 'a t_float = Empty_f | Leaf_f of 'a
  type 'a u
  type 'a v = 'a list
  
  type 'a t := 'a t
  type 'a u := 'a u
  type 'a v := 'a v
  val first : 'a t -> 'a u -> 'a v -> unit
  
  type 'a t := 'a t_float
  type 'a u := 'a t_float
  type 'a v := 'a t_float
  val first_float : 'a t -> 'a u -> 'a v -> unit
  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc

The declared types keep their names and definitions, each substitution is
shown with the name it was written with, and a use of a substitution names
it too:

  $ odoc markdown-generate -o md test.odocl
  $ cat md/Test.md
  
  # Module `Test`
  
  ```ocaml
  type 'a t = 
    | Empty
    | Leaf of 'a
  ```
  ```ocaml
  type 'a t_float = 
    | Empty_f
    | Leaf_f of 'a
  ```
  ```ocaml
  type 'a u
  ```
  ```ocaml
  type 'a v = 'a list
  ```
  ```ocaml
  type 'a t := 'a t
  ```
  ```ocaml
  type 'a u := 'a u
  ```
  ```ocaml
  type 'a v := 'a v
  ```
  ```ocaml
  val first : 'a t -> 'a u -> 'a v -> unit
  ```
  ```ocaml
  type 'a t := 'a t_float
  ```
  ```ocaml
  type 'a u := 'a t_float
  ```
  ```ocaml
  type 'a v := 'a t_float
  ```
  ```ocaml
  val first_float : 'a t -> 'a u -> 'a v -> unit
  ```

Each substitution has its own anchor in the HTML, distinct from the real
type's, and each use links to the substitution in scope where it's written:
[first] to the first substitutions, [first_float] to the second. (Markdown
output doesn't link code, so these look at the HTML: the [sed] prints a
value's declaration, from its [<div>] to the closing [</div>].)

  $ odoc html-generate --indent -o html test.odocl
  $ grep -o 'id="type-[^"]*"' html/Test/index.html
  id="type-t"
  id="type-t.Empty"
  id="type-t.Leaf"
  id="type-t_float"
  id="type-t_float.Empty_f"
  id="type-t_float.Leaf_f"
  id="type-u"
  id="type-v"
  id="type-subst-t"
  id="type-subst-u"
  id="type-subst-v"
  id="type-subst-2-t"
  id="type-subst-2-u"
  id="type-subst-2-v"
  $ sed -n '/id="val-first"/,/<\/div>/p' html/Test/index.html | grep -o 'href="#type-[^"]*"'
  href="#type-subst-t"
  href="#type-subst-u"
  href="#type-subst-v"
  $ sed -n '/id="val-first_float"/,/<\/div>/p' html/Test/index.html | grep -o 'href="#type-[^"]*"'
  href="#type-subst-2-t"
  href="#type-subst-2-u"
  href="#type-subst-2-v"

Nothing in the output, anchors included, uses the internal form:

  $ grep -c 'shadowed/(' html/Test/index.html || true
  0

When a signature is included, its substitutions are applied, and only to
the items they were written for, not to a real item of the same name
(here [type t = string]), including through a nested include:

  $ cat include.mli
  module type S = sig
    type t := int
    val x : t
    type t = string
    val y : t
  end
  
  module type I = sig
    include S
  end
  
  module type J = sig
    include sig
      include S
      type u := float
      val z : u
    end
  end
  $ ocamlc -c -bin-annot include.mli
  $ odoc compile include.cmti
  $ odoc link include.odoc
  $ odoc markdown-generate -o md include.odocl
  $ cat md/Include-module-type-I.md md/Include-module-type-J.md
  
  # Module type `Include.I`
  
  ```ocaml
  val x : int
  ```
  ```ocaml
  type t = string
  ```
  ```ocaml
  val y : t
  ```
  # Module type `Include.J`
  
  ```ocaml
  val x : int
  ```
  ```ocaml
  type t = string
  ```
  ```ocaml
  val y : t
  ```
  ```ocaml
  val z : float
  ```

Substitutions hidden by stop comments keep distinct identifiers, from the
items they substitute and from each other, so later public uses still
resolve:

  $ cat hidden.mli
  (**/**)
  type t = int
  type t := t
  type t := t list
  module Base : sig module Nested : sig type n = bool end end
  module M := Base
  module M := M.Nested
  (**/**)
  
  val u : t
  val w : M.n
  $ ocamlc -c -bin-annot hidden.mli
  $ odoc compile hidden.cmti
  $ odoc link hidden.odoc
  $ odoc markdown-generate -o md hidden.odocl
  $ cat md/Hidden.md
  
  # Module `Hidden`
  
  ```ocaml
  val u : int list
  ```
  ```ocaml
  val w : bool
  ```

A reference goes to a real item rather than to a substitution of the same
name, whichever comes first, including a real item from an [include]:

  $ cat refs.mli
  module Other : sig end
  
  type t := int
  type t = string
  
  val y : t
  (** See {!t}. *)
  
  module M : sig type a end
  module M := Other
  
  val m : unit
  (** See {!M} and {!M.a}. *)
  
  include sig type i = string end
  type i := int
  
  val inc : unit
  (** See {!i}. *)
  $ ocamlc -c -bin-annot refs.mli
  $ odoc compile refs.cmti
  $ odoc link refs.odoc
  $ odoc markdown-generate -o md refs.odocl
  $ cat md/Refs.md
  
  # Module `Refs`
  
  ```ocaml
  module Other : sig ... end
  ```
  ```ocaml
  type t := int
  ```
  ```ocaml
  type t = string
  ```
  ```ocaml
  val y : t
  ```
  See [`t`](./#type-t).
  
  ```ocaml
  module M : sig ... end
  ```
  ```ocaml
  module M := Other
  ```
  ```ocaml
  val m : unit
  ```
  See [`M`](./Refs-M.md) and [`M.a`](./Refs-M.md#type-a).
  
  ```ocaml
  type i = string
  ```
  ```ocaml
  type i := int
  ```
  ```ocaml
  val inc : unit
  ```
  See [`i`](./#type-i).

A reference can come before the substitution it refers to. A real item
takes the name over a substitution whichever comes first, and repeated
substitutions of a name leave the name to the last one:

  $ cat refs_order.mli
  val fwd : unit
  (** See {!offset}. *)
  
  type offset := int
  
  type t = string
  type t := int
  
  val after_real : unit
  (** See {!t}. *)
  
  type u := int
  type u := bool
  
  val repeated : unit
  (** See {!u}. *)
  $ ocamlc -c -bin-annot refs_order.mli
  $ odoc compile refs_order.cmti
  $ odoc link refs_order.odoc
  $ odoc markdown-generate -o md refs_order.odocl
  $ cat md/Refs_order.md
  
  # Module `Refs_order`
  
  ```ocaml
  val fwd : unit
  ```
  See [`offset`](./#type-subst-offset).
  
  ```ocaml
  type offset := int
  ```
  ```ocaml
  type t = string
  ```
  ```ocaml
  type t := int
  ```
  ```ocaml
  val after_real : unit
  ```
  See [`t`](./#type-t).
  
  ```ocaml
  type u := int
  ```
  ```ocaml
  type u := bool
  ```
  ```ocaml
  val repeated : unit
  ```
  See [`u`](./#type-subst-2-u).

References to substitutions in a signature that is included are resolved
where it is included, after its substitutions have been applied. So in [I],
[{!t}] goes to [I]'s own [type t], and [{!M}] and [{!M.b}] don't resolve, as
expected: the warnings below are part of the expected output.

  $ cat refs_include.mli
  module Base : sig type b end
  
  module type S = sig
    type t := int
  
    val x : t
    (** See {!t}. *)
  
    module M := Base
  
    val m : unit
    (** See {!M} and {!M.b}. *)
  end
  
  module I : sig
    type t = string
  
    include S
  end
  $ ocamlc -c -bin-annot refs_include.mli
  $ odoc compile refs_include.cmti
  $ odoc link refs_include.odoc
  File "refs_include.mli", line 12, characters 19-25:
  Warning: While resolving the expansion of include at File "refs_include.mli", line 18, character 2
  Failed to resolve reference unresolvedroot(M).b Couldn't find "M"
  File "refs_include.mli", line 12, characters 10-14:
  Warning: While resolving the expansion of include at File "refs_include.mli", line 18, character 2
  Failed to resolve reference unresolvedroot(M) Couldn't find "M"
  $ odoc markdown-generate -o md refs_include.odocl
  $ cat md/Refs_include-module-type-S.md md/Refs_include-I.md
  
  # Module type `Refs_include.S`
  
  ```ocaml
  type t := int
  ```
  ```ocaml
  val x : t
  ```
  See [`t`](./#type-subst-t).
  
  ```ocaml
  module M := Base
  ```
  ```ocaml
  val m : unit
  ```
  See [`M`](./Refs_include-Base.md) and [`M.b`](./Refs_include-Base.md#type-b).
  
  # Module `Refs_include.I`
  
  ```ocaml
  type t = string
  ```
  ```ocaml
  val x : int
  ```
  See [`t`](./#type-t).
  
  ```ocaml
  val m : unit
  ```
  See `M` and `M.b`.

A class declares a type of the same name, so a reference goes to the class
rather than to a type substitution:

  $ cat refs_class.mli
  class k : object end
  
  type k := int
  
  val after : unit
  (** See {!k}. *)
  $ ocamlc -c -bin-annot refs_class.mli
  $ odoc compile refs_class.cmti
  $ odoc link refs_class.odoc
  $ odoc markdown-generate -o md refs_class.odocl
  $ cat md/Refs_class.md
  
  # Module `Refs_class`
  
  ```ocaml
  class k : object ... end
  ```
  ```ocaml
  type k := int
  ```
  ```ocaml
  val after : unit
  ```
  See [`k`](./Refs_class-class-k.md).

A hidden item isn't rendered, so it doesn't take the name over a
substitution: the reference goes to the substitution, which is.

  $ cat refs_hidden.mli
  (**/**)
  type t = int
  (**/**)
  
  type t := t list
  
  val x : t
  (** See {!t}. *)
  $ ocamlc -c -bin-annot refs_hidden.mli
  $ odoc compile refs_hidden.cmti
  $ odoc link refs_hidden.odoc
  $ odoc markdown-generate -o md refs_hidden.odocl
  $ cat md/Refs_hidden.md
  
  # Module `Refs_hidden`
  
  ```ocaml
  type t := int list
  ```
  ```ocaml
  val x : t
  ```
  See [`t`](./#type-subst-t).

Substitutions aren't part of a unit's interface, so an [--open]ed unit's
substitutions are not in scope, and the references don't resolve:

  $ cat opened.mli opener.mli
  type t := int
  
  val x : t
  
  module M := Stdlib.List
  val y : int
  (** See {!t} and {!M}. *)
  $ ocamlc -c -bin-annot opened.mli
  $ ocamlc -c -bin-annot opener.mli
  $ odoc compile opened.cmti
  $ odoc compile -I . opener.cmti
  $ odoc link -I . --open=Opened opener.odoc
  File "opener.mli", line 2, characters 17-21:
  Warning: Failed to resolve reference unresolvedroot(M) Couldn't find "M"
  File "opener.mli", line 2, characters 8-12:
  Warning: Failed to resolve reference unresolvedroot(t) Couldn't find "t"

The same goes for an [open]ed module's substitutions:

  $ cat opens.mli
  module O : sig
    type t := int
  
    val x : t
  
    module M := Stdlib.List
  end
  
  open O
  
  val y : int
  (** See {!t} and {!M}. *)
  $ ocamlc -c -bin-annot opens.mli
  $ odoc compile opens.cmti
  $ odoc link opens.odoc
  File "opens.mli", line 12, characters 17-21:
  Warning: Failed to resolve reference unresolvedroot(M) Couldn't find "M"
  File "opens.mli", line 12, characters 8-12:
  Warning: Failed to resolve reference unresolvedroot(t) Couldn't find "t"
