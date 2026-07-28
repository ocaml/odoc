The name of a type is not part of the OCaml path of its constructors and of
its fields, so it must not be spliced into the text a reference is rendered
as: [M.t.Foo] is not valid syntax whereas [M.Foo] is.
See https://github.com/ocaml/odoc/issues/372

  $ ocamlc -c -bin-annot a.mli
  $ odoc compile --warn-error -I . a.cmti

Only the reference whose parent is a class fails to resolve:

  $ odoc link a.odoc
  File "a.mli", line 54, characters 64-76:
  Warning: Failed to resolve reference unresolvedroot(cls).Alpha Couldn't find "Alpha"

  $ odoc html-generate --output-dir html --indent a.odocl

Constructors and fields are qualified by their module path only, never by the
name of their type. Extensions, exceptions and types keep their parent, and
the anchors are unaffected:

  $ cat html/A/index.html | grep '<a href' | grep '<code>'
       <a href="#type-t.Alpha"><code>Alpha</code></a>, 
      <a href="#type-t.Alpha"><code>Alpha</code></a>, 
      <a href="#type-t.Alpha"><code>Alpha</code></a>, 
      <a href="#type-t.Alpha"><code>Alpha</code></a>, 
      <a href="#type-r.fld"><code>fld</code></a>, 
      <a href="#type-r.fld"><code>fld</code></a> and 
      <a href="#type-r.fld"><code>fld</code></a>.
      <a href="Bla/index.html#type-ha.Alpha"><code>Bla.Alpha</code></a>
       and <a href="Bla/index.html#type-ha.Alpha"><code>Bla.Alpha</code></a>
      <a href="Bla/index.html#type-ra.fla"><code>Bla.fla</code></a> and
       <a href="Bla/index.html#type-ra.fla"><code>Bla.fla</code></a> both
      <a href="Bla/index.html#type-sw.On"><code>Bla.`On</code></a> and 
      <a href="Bla/index.html#type-sw.Off"><code>Bla.`Off</code></a>.
      <a href="Bla/Inner/index.html#type-i.Gamma"><code>Bla.Inner.Gamma</code>
      <a href="Bla/Inner/index.html#type-i.Gamma"><code>Bla.Inner.Gamma</code>
      <a href="Bla/index.html#extension-Ext_a"><code>Bla.Ext_a</code></a>
      , <a href="Bla/index.html#exception-Exn_a"><code>Bla.Exn_a</code></a>
      , <a href="Bla/index.html#type-ha"><code>Bla.ha</code></a> and 
      <a href="Bla/Inner/index.html#type-i"><code>Bla.Inner.i</code></a>
      <a href="Ambiguous/index.html#type-a.Same"><code>Ambiguous.Same</code>
      <a href="Ambiguous/index.html#type-b.Same"><code>Ambiguous.Same</code>

With its own text, the reference is rendered as the tooltip:

  $ cat html/A/index.html | grep -o 'title="[^"]*"'
  title="Bla.Alpha"

An unresolved reference is still printed exactly as it was written, and is
not turned into a link:

  $ cat html/A/index.html | grep 'cls.Alpha'
      <code>cls.Alpha</code>
