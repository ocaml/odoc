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

Compile the modules:

  $ ocamlc -c a.ml -bin-annot

Compile the pages:

  $ odoc compile --impl a.ml a.cmt
  $ odoc link -I . a.odoc
  $ odoc html-generate --indent -o html a.odocl

Source links generated in the documentation:

  $ grep source_link html/A/index.html -B 2
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="A.ml.html#def-A0" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="A.ml.html#def-A1" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="A.ml.html#def-A2" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-z">
       <a href="#val-z" class="anchor"></a>
       <a href="A.ml.html#def-A3" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-A">
       <a href="#module-A" class="anchor"></a>
       <a href="A.ml.html#def-A5" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-B">
       <a href="#module-B" class="anchor"></a>
       <a href="A.ml.html#def-A5" class="source_link">Source</a>
  --
      <div class="spec module-type anchored" id="module-type-T">
       <a href="#module-type-T" class="anchor"></a>
       <a href="A.ml.html#def-A7" class="source_link">Source</a>
  --
      <div class="spec module-type anchored" id="module-type-U">
       <a href="#module-type-U" class="anchor"></a>
       <a href="A.ml.html#def-A8" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-ext">
       <a href="#type-ext" class="anchor"></a>
       <a href="A.ml.html#def-A9" class="source_link">Source</a>
  --
      <div class="spec type extension anchored" id="extension-decl-Foo">
       <a href="#extension-decl-Foo" class="anchor"></a>
       <a href="A.ml.html#def-A10" class="source_link">Source</a>
  --
      <div class="spec exception anchored" id="exception-Exn">
       <a href="#exception-Exn" class="anchor"></a>
       <a href="A.ml.html#def-A11" class="source_link">Source</a>
  --
      <div class="spec class anchored" id="class-cls">
       <a href="#class-cls" class="anchor"></a>
       <a href="A.ml.html#def-A12" class="source_link">Source</a>
  --
      <div class="spec class anchored" id="class-cls'">
       <a href="#class-cls'" class="anchor"></a>
       <a href="A.ml.html#def-A14" class="source_link">Source</a>
  --
      <div class="spec class-type anchored" id="class-type-ct">
       <a href="#class-type-ct" class="anchor"></a>
       <a href="A.ml.html#def-A15" class="source_link">Source</a>

Ids generated in the source code:

  $ cat html/A/A.ml.html | tr '> ' '\n\n' | grep '^id'
  id="selfpat-*_284"
  id="def-A0"
  id="L1"
  id="L2"
  id="L3"
  id="x_268"
  id="def-A1"
  id="L4"
  id="y_269"
  id="def-A2"
  id="L5"
  id="z_270"
  id="def-A3"
  id="a_272"
  id="L6"
  id="def-A5"
  id="L7"
  id="def-A6"
  id="L8"
  id="L9"
  id="def-A7"
  id="L10"
  id="def-A8"
  id="L11"
  id="L12"
  id="def-A9"
  id="L13"
  id="L14"
  id="def-A10"
  id="L15"
  id="def-A11"
  id="L16"
  id="L17"
  id="L18"
  id="def-A12"
  id="L19"
  id="def-A14"
  id="L20"
  id="def-A15"
