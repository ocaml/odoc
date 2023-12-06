This test tests the ability to reference constructors, omitting the type they
are coming from.

  $ ocamlc -c -bin-annot a.mli
  $ odoc compile --warn-error -I . a.cmti

It is possible to omit type parent in constructor reference, and use directly
the parent module. All references in [a.mli] resolve without warning, except the
faulty reference.

  $ odoc link a.odoc
  File "a.mli", line 15, characters 4-22:
  Warning: Failed to resolve reference unresolvedroot(t).A Couldn't find "t"

Let's now check that the reference point to the right page/anchor:

  $ odoc html-generate --output-dir html --indent a.odocl

  $ cat html/A/index.html | grep \# | grep Foo | grep -v anchor
     <p><a href="#type-u.Foo"><code>Foo</code></a> 
      <a href="#type-u.Foo"><code>u.Foo</code></a> 
      <a href="#type-u.Foo"><code>Foo</code></a>
     <p><a href="M/index.html#type-t.Foo"><code>M.t.Foo</code></a> and 
      <a href="M/index.html#type-t.Foo"><code>M.t.Foo</code></a>
     <p><a href="M/index.html#type-t.Foo"><code>M.t.Foo</code></a> and 
      <a href="M/index.html#type-t.Foo"><code>M.t.Foo</code></a>
