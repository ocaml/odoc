A local substitution, [type t := ...], isn't part of the resulting
signature: it names a type for the items after it, and nothing outside the
signature can refer to it. So it must not shadow a real [type t] declared
before it, and it shouldn't be shown with odoc's internal disambiguated
name either.

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
  $ odoc html-generate --indent -o html test.odocl

The declared types keep their names and definitions, each substitution is
shown with the name it was written with, and a use of a substitution names
it too:

  $ tr '\n' ' ' < html/Test/index.html | sed -e 's/<[^>]*>//g' -e 's/&#45;&gt;/->/g' | tr -s ' ' | awk '{ gsub(/ (type|val) /, "\n&"); print }' | grep -E '^ (type|val) ' | sed -e 's/^ //' -e 's/ *$//'
  type 'a t = | Empty | Leaf of 'a
  type 'a t_float = | Empty_f | Leaf_f of 'a
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

Each substitution has its own anchor, distinct from the real type's, and
each use links to the substitution in scope where it's written: [first] to
the first substitutions, [first_float] to the second.

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
  $ for v in first first_float; do echo "$v:"; sed -n "/id=\"val-$v\"/,/<\/div>/p" html/Test/index.html | grep -o 'href="#type-[^"]*"' | grep -v "val-"; done
  first:
  href="#type-subst-t"
  href="#type-subst-u"
  href="#type-subst-v"
  first_float:
  href="#type-subst-2-t"
  href="#type-subst-2-u"
  href="#type-subst-2-v"

Nothing in the output, anchors included, uses the internal form:

  $ grep -c 'shadowed/(' html/Test/index.html || true
  0

When a signature is included, its substitutions are applied, and only to
the items they were written for, not to a real item of the same name
(here [type t = string]), including through a nested include:

  $ ocamlc -c -bin-annot include.mli
  $ odoc compile include.cmti
  $ odoc link include.odoc
  $ odoc html-generate --indent -o html include.odocl
  $ for m in I J; do echo "$m:"; tr '\n' ' ' < html/Include/module-type-$m/index.html | sed -e 's/<[^>]*>//g' | tr -s ' ' | awk '{ gsub(/ (type|val) /, "\n&"); print }' | grep -E '^ (type|val) ' | sed -e 's/^ //' -e 's/ *$//'; done
  I:
  type Include.I include S
  val x : int
  type t = string
  val y : t
  J:
  type Include.J include sig ... end include S
  val x : int
  type t = string
  val y : t
  val z : float

Substitutions hidden by stop comments keep distinct identifiers, from the
items they substitute and from each other, so later public uses still
resolve:

  $ ocamlc -c -bin-annot hidden.mli
  $ odoc compile hidden.cmti
  $ odoc link hidden.odoc
  $ odoc html-generate --indent -o html hidden.odocl
  $ tr '\n' ' ' < html/Hidden/index.html | sed -e 's/<[^>]*>//g' -e 's/&#45;&gt;/->/g' | tr -s ' ' | awk '{ gsub(/ (type|val|module) /, "\n&"); print }' | grep -E '^ (type|val|module) ' | sed -e 's/^ //' -e 's/ *$//'
  val u : int list
  module X : sig ... end
  val w : bool

A reference after a real declaration goes to it, not to an earlier
substitution of the same name:

  $ ocamlc -c -bin-annot refs.mli
  $ odoc compile refs.cmti
  $ odoc link refs.odoc 2>&1 | grep -v Stdlib
  [1]
  $ odoc html-generate --indent -o html refs.odocl
  $ for v in y m s; do echo "$v:" $(awk "/id=\"val-$v\"/ { f = 1 } f && /spec-doc/ { g = 1 } g { print } g && /<\/p>/ { exit }" html/Refs/index.html | grep -o 'href="[^"]*"'); done
  y: href="#type-t"
  m: href="M/index.html" href="M/index.html#type-a"
  s: href="module-type-S/index.html" href="module-type-S/index.html#type-b"

A reference can come before the substitution it refers to. A real item
takes the name over a substitution whichever comes first, and repeated
substitutions of a name leave the name to the last one:

  $ ocamlc -c -bin-annot refs_order.mli
  $ odoc compile refs_order.cmti
  $ odoc link refs_order.odoc
  $ odoc html-generate --indent -o html refs_order.odocl
  $ for v in fwd after_real repeated; do echo "$v:" $(awk "/id=\"val-$v\"/ { f = 1 } f && /spec-doc/ { g = 1 } g { print } g && /<\/p>/ { exit }" html/Refs_order/index.html | grep -o 'href="[^"]*"'); done
  fwd: href="#type-subst-offset"
  after_real: href="#type-t"
  repeated: href="#type-subst-2-u"

References to substitutions in a signature that is included are resolved
where it is included, after its substitutions have been applied:

  $ ocamlc -c -bin-annot refs_include.mli
  $ odoc compile refs_include.cmti
  $ odoc link refs_include.odoc
  File "refs_include.mli", line 12, characters 19-25:
  Warning: While resolving the expansion of include at File "refs_include.mli", line 18, character 2
  Failed to resolve reference unresolvedroot(M).b Couldn't find "M"
  File "refs_include.mli", line 12, characters 10-14:
  Warning: While resolving the expansion of include at File "refs_include.mli", line 18, character 2
  Failed to resolve reference unresolvedroot(M) Couldn't find "M"
  $ odoc html-generate --indent -o html refs_include.odocl
  $ for p in module-type-S I; do for v in x m; do echo "$p $v:" $(awk "/id=\"val-$v\"/ { f = 1 } f && /spec-doc/ { g = 1 } g { print } g && /<\/p>/ { exit }" html/Refs_include/$p/index.html | grep -o 'href="[^"]*"'); done; done
  module-type-S x: href="#type-subst-t"
  module-type-S m: href="../Base/index.html" href="../Base/index.html#type-b"
  I x: href="#type-t"
  I m:

A class declares a type of the same name, so a reference goes to the class
rather than to a type substitution:

  $ ocamlc -c -bin-annot refs_class.mli
  $ odoc compile refs_class.cmti
  $ odoc link refs_class.odoc
  $ odoc html-generate --indent -o html refs_class.odocl
  $ awk '/id="val-after"/ { f = 1 } f && /spec-doc/ { g = 1 } g { print } g && /<\/p>/ { exit }' html/Refs_class/index.html | grep -o 'href="[^"]*"'
  href="class-k/index.html"

Substitutions aren't part of a unit's interface, so an [--open]ed unit's
substitutions are not in scope:

  $ ocamlc -c -bin-annot opened.mli
  $ ocamlc -c -bin-annot opener.mli
  $ odoc compile opened.cmti
  $ odoc compile -I . opener.cmti
  $ odoc link -I . --open=Opened opener.odoc
  File "opener.mli", line 2, characters 17-21:
  Warning: Failed to resolve reference unresolvedroot(M) Couldn't find "M"
  File "opener.mli", line 2, characters 8-12:
  Warning: Failed to resolve reference unresolvedroot(t) Couldn't find "t"

In source pages, each substitution has an anchor of its own:

  $ ocamlc -c -bin-annot source.ml
  $ odoc compile-impl --source-id src/source.ml -I . source.cmt --output-dir .
  $ odoc compile -I . source.cmt
  $ odoc link -I . impl-source.odoc
  $ odoc html-generate-source --impl impl-source.odocl -o html source.ml
  $ grep -o 'id="module-type-S[^"]*"' html/src/source.ml.html
  id="module-type-S"
  id="module-type-S.type-t"
  id="module-type-S.type-subst-t"
  id="module-type-S.module-type-T"
  id="module-type-S.module-type-subst-T"
  id="module-type-S.module-type-subst-T.type-q"
  id="module-type-S.module-M"
  id="module-type-S.module-subst-M"
