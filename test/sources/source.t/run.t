Files containing some values:

  $ cat a.ml
  type t = string
  
  let x = 2
  let y = x + 1
  let z a = if x = 1 || true then x + y else 0
  
  module A = struct end
  module B = A
  
  module type T = sig end
  module type U = T
  
  type ext = ..
  type ext += Foo
  
  exception Exn
  
  class cls = object end
  class cls' = cls
  class type ct = object end

Source pages require a parent:

  $ odoc compile -c module-a -c src-source root.mld

Compile the modules:

  $ ocamlc -c a.ml -bin-annot

Compile the pages without --source:

  $ odoc compile a.cmt
  $ odoc link -I . a.odoc
  $ odoc html-generate --indent -o html a.odocl

No source links are generated in the documentation:

  $ ! grep source_link html/A/index.html -B 2

Now, compile the pages with the --source option:

  $ printf "a.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ odoc compile -I . --source-name a.ml --source-parent-file src-source.odoc a.cmt
  $ odoc link -I . a.odoc
  $ odoc html-generate --source a.ml --indent -o html a.odocl

Source links generated in the documentation:

  $ grep source_link html/A/index.html -B 2
    <header class="odoc-preamble">
     <h1>Module <code><span>A</span></code>
      <a href="../root/source/a.ml.html" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../root/source/a.ml.html#type-t" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="../root/source/a.ml.html#val-x" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="../root/source/a.ml.html#val-y" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-z">
       <a href="#val-z" class="anchor"></a>
       <a href="../root/source/a.ml.html#val-z" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-A">
       <a href="#module-A" class="anchor"></a>
       <a href="../root/source/a.ml.html#module-A" class="source_link">Source
  --
      <div class="spec module anchored" id="module-B">
       <a href="#module-B" class="anchor"></a>
       <a href="../root/source/a.ml.html#module-A" class="source_link">Source
  --
      <div class="spec module-type anchored" id="module-type-T">
       <a href="#module-type-T" class="anchor"></a>
       <a href="../root/source/a.ml.html#module-type-T" class="source_link">
  --
      <div class="spec module-type anchored" id="module-type-U">
       <a href="#module-type-U" class="anchor"></a>
       <a href="../root/source/a.ml.html#module-type-U" class="source_link">
  --
      <div class="spec type anchored" id="type-ext">
       <a href="#type-ext" class="anchor"></a>
       <a href="../root/source/a.ml.html#type-ext" class="source_link">Source
  --
      <div class="spec type extension anchored" id="extension-decl-Foo">
       <a href="#extension-decl-Foo" class="anchor"></a>
       <a href="../root/source/a.ml.html#ext-Foo" class="source_link">Source
  --
      <div class="spec exception anchored" id="exception-Exn">
       <a href="#exception-Exn" class="anchor"></a>
       <a href="../root/source/a.ml.html#ext-Exn" class="source_link">Source
  --
      <div class="spec class anchored" id="class-cls">
       <a href="#class-cls" class="anchor"></a>
       <a href="../root/source/a.ml.html#type-hash-cls" class="source_link">
  --
      <div class="spec class anchored" id="class-cls'">
       <a href="#class-cls'" class="anchor"></a>
       <a href="../root/source/a.ml.html#type-hash-cls'" class="source_link">
  --
      <div class="spec class-type anchored" id="class-type-ct">
       <a href="#class-type-ct" class="anchor"></a>
       <a href="../root/source/a.ml.html#type-hash-ct" class="source_link">

Ids generated in the source code:

  $ cat html/root/source/a.ml.html | tr '> ' '\n\n' | grep '^id'
  id="L1"
  id="L2"
  id="L3"
  id="L4"
  id="L5"
  id="L6"
  id="L7"
  id="L8"
  id="L9"
  id="L10"
  id="L11"
  id="L12"
  id="L13"
  id="L14"
  id="L15"
  id="L16"
  id="L17"
  id="L18"
  id="L19"
  id="L20"
  id="type-t"
  id="val-x"
  id="local_x_21"
  id="val-y"
  id="local_y_31"
  id="val-z"
  id="local_z_45"
  id="local_a_47"
  id="module-A"
  id="def-6"
  id="module-type-T"
  id="module-type-U"
  id="type-ext"
  id="ext-Foo"
  id="ext-Exn"
  id="type-hash-cls"
  id="type-hash-cls'"
  id="type-hash-ct"
