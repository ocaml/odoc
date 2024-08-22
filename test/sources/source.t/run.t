Files containing some values:

  $ cat a.ml
  type t = string
  
  type truc = A | B
  
  let xazaz = A
  
  module Yoyo = struct
    type bli = Aa | Bb
  end
  
  let segr = Yoyo.Aa
  
  let x = 2
  let y = x + 1
  let z a = if x = 1 || true then x + y else a
  let z' a = if x = 1 || true then x + y else a
  
  module A = struct end
  module B = A
  
  module type T = sig end
  module type U = T
  
  type ext = ..
  type ext += Foo | Bar
  
  exception Exn
  
  class cls = object end
  class cls' = cls
  class type ct = object end
  
  let x _ = raise Exn
  
  module X : sig
    type t
  end = struct
    type t = int
  end
  
  type a1 = int
  and a2 = a1
  
  module F (M : sig
    module A : sig end
  end) =
  struct
    module B = M.A
  end
  
  module FM = F (struct
    module A = struct end
  end)
  
  module FF (A : sig end) (B : sig end) = struct end
  module FF2 (A : sig
    module E : sig end
  end) (A : sig
    module F : sig end
  end) =
  struct end
  
  (* here is a comment *)
  let x = fun x -> function A | (* other comment *) B -> 3
  
  
  (** This is the docstring of this very important custom operator *)
  let ( *.+%) = (+)
  
  let a = 3
  
  let b = 5
  
  let c = 8
  
  let x = a * b *.+% c 
  
  let b = a / c
  
  let x = a mod b
  
  let list = [a ; c; b; 1; 2; 3; 4; 5; 6; 7; 8]
  
  let string = "lorem ipsum"
  
  let string2 = "truc"

Compile the modules:

  $ ocamlc -c a.ml -bin-annot

Compile the pages with the --source option:

  $ odoc compile-impl -I . --source-id src/a.ml a.cmt
  $ odoc compile -I . a.cmt
  $ odoc link -I . a.odoc
  $ odoc link -I . impl-a.odoc
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate-source --impl impl-a.odocl --indent -o html a.ml
  $ odoc support-files -o html

Source links generated in the documentation:

  $ grep source_link html/A/index.html -B 2
    <header class="odoc-preamble">
     <h1>Module <code><span>A</span></code>
      <a href="../src/a.ml.html" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../src/a.ml.html#type-t" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-truc">
       <a href="#type-truc" class="anchor"></a>
       <a href="../src/a.ml.html#type-truc" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-xazaz">
       <a href="#val-xazaz" class="anchor"></a>
       <a href="../src/a.ml.html#val-xazaz" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-Yoyo">
       <a href="#module-Yoyo" class="anchor"></a>
       <a href="../src/a.ml.html#module-Yoyo" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-segr">
       <a href="#val-segr" class="anchor"></a>
       <a href="../src/a.ml.html#val-segr" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="../src/a.ml.html#val-y" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-z">
       <a href="#val-z" class="anchor"></a>
       <a href="../src/a.ml.html#val-z" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-z'">
       <a href="#val-z'" class="anchor"></a>
       <a href="../src/a.ml.html#val-z'" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-A">
       <a href="#module-A" class="anchor"></a>
       <a href="../src/a.ml.html#module-A" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-B">
       <a href="#module-B" class="anchor"></a>
       <a href="../src/a.ml.html#module-A" class="source_link">Source</a>
  --
      <div class="spec module-type anchored" id="module-type-T">
       <a href="#module-type-T" class="anchor"></a>
       <a href="../src/a.ml.html#module-type-T" class="source_link">Source</a>
  --
      <div class="spec module-type anchored" id="module-type-U">
       <a href="#module-type-U" class="anchor"></a>
       <a href="../src/a.ml.html#module-type-U" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-ext">
       <a href="#type-ext" class="anchor"></a>
       <a href="../src/a.ml.html#type-ext" class="source_link">Source</a>
  --
      <div class="spec type extension anchored" id="extension-decl-Foo">
       <a href="#extension-decl-Foo" class="anchor"></a>
       <a href="../src/a.ml.html#extension-Foo" class="source_link">Source</a>
  --
      <div class="spec exception anchored" id="exception-Exn">
       <a href="#exception-Exn" class="anchor"></a>
       <a href="../src/a.ml.html#exception-Exn" class="source_link">Source</a>
  --
      <div class="spec class anchored" id="class-cls">
       <a href="#class-cls" class="anchor"></a>
       <a href="../src/a.ml.html#class-cls" class="source_link">Source</a>
  --
      <div class="spec class anchored" id="class-cls'">
       <a href="#class-cls'" class="anchor"></a>
       <a href="../src/a.ml.html#class-cls'" class="source_link">Source</a>
  --
      <div class="spec class-type anchored" id="class-type-ct">
       <a href="#class-type-ct" class="anchor"></a>
       <a href="../src/a.ml.html#class-type-ct" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-X">
       <a href="#module-X" class="anchor"></a>
       <a href="../src/a.ml.html#module-X" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-a1">
       <a href="#type-a1" class="anchor"></a>
       <a href="../src/a.ml.html#type-a1" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-a2">
       <a href="#type-a2" class="anchor"></a>
       <a href="../src/a.ml.html#type-a2" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-F">
       <a href="#module-F" class="anchor"></a>
       <a href="../src/a.ml.html#module-F" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-FM">
       <a href="#module-FM" class="anchor"></a>
       <a href="../src/a.ml.html#module-FM" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-FF">
       <a href="#module-FF" class="anchor"></a>
       <a href="../src/a.ml.html#module-FF" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-FF2">
       <a href="#module-FF2" class="anchor"></a>
       <a href="../src/a.ml.html#module-FF2" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-(*.+%)">
       <a href="#val-(*.+%)" class="anchor"></a>
       <a href="../src/a.ml.html#val-(*.+%)" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-a">
       <a href="#val-a" class="anchor"></a>
       <a href="../src/a.ml.html#val-a" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-c">
       <a href="#val-c" class="anchor"></a>
       <a href="../src/a.ml.html#val-c" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-b">
       <a href="#val-b" class="anchor"></a>
       <a href="../src/a.ml.html#val-b" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="../src/a.ml.html#val-x" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-list">
       <a href="#val-list" class="anchor"></a>
       <a href="../src/a.ml.html#val-list" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-string">
       <a href="#val-string" class="anchor"></a>
       <a href="../src/a.ml.html#val-string" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-string2">
       <a href="#val-string2" class="anchor"></a>
       <a href="../src/a.ml.html#val-string2" class="source_link">Source</a>

Ids generated in the source code:

  $ cat html/src/a.ml.html | tr '> ' '\n\n' | grep '^id'
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
  id="L21"
  id="L22"
  id="L23"
  id="L24"
  id="L25"
  id="L26"
  id="L27"
  id="L28"
  id="L29"
  id="L30"
  id="L31"
  id="L32"
  id="L33"
  id="L34"
  id="L35"
  id="L36"
  id="L37"
  id="L38"
  id="L39"
  id="L40"
  id="L41"
  id="L42"
  id="L43"
  id="L44"
  id="L45"
  id="L46"
  id="L47"
  id="L48"
  id="L49"
  id="L50"
  id="L51"
  id="L52"
  id="L53"
  id="L54"
  id="L55"
  id="L56"
  id="L57"
  id="L58"
  id="L59"
  id="L60"
  id="L61"
  id="L62"
  id="L63"
  id="L64"
  id="L65"
  id="L66"
  id="L67"
  id="L68"
  id="L69"
  id="L70"
  id="L71"
  id="L72"
  id="L73"
  id="L74"
  id="L75"
  id="L76"
  id="L77"
  id="L78"
  id="L79"
  id="L80"
  id="L81"
  id="L82"
  id="L83"
  id="L84"
  id="L85"
  id="type-t"
  id="type-truc"
  id="type-truc.constructor-A"
  id="type-truc.constructor-B"
  id="val-xazaz"
  id="module-Yoyo"
  id="module-Yoyo.type-bli"
  id="module-Yoyo.type-bli.constructor-Aa"
  id="module-Yoyo.type-bli.constructor-Bb"
  id="val-segr"
  id="val-{x}1/shadowed/(src-a.ml)"
  id="val-y"
  id="val-z"
  id="local_a_1"
  id="val-z'"
  id="local_a_2"
  id="module-A"
  id="module-B"
  id="module-type-T"
  id="module-type-U"
  id="type-ext"
  id="extension-Foo"
  id="extension-Bar"
  id="exception-Exn"
  id="class-cls"
  id="class-cls'"
  id="class-type-ct"
  id="val-{x}2/shadowed/(src-a.ml)"
  id="module-X"
  id="module-X.type-t"
  id="module-X.type-t"
  id="type-a1"
  id="type-a2"
  id="module-F"
  id="module-F.argument-1-M.module-A"
  id="module-F.module-B"
  id="module-FM"
  id="local_A_3"
  id="module-FF"
  id="module-FF2"
  id="module-FF2.argument-1-A.module-E"
  id="module-FF2.argument-2-A.module-F"
  id="val-{x}3/shadowed/(src-a.ml)"
  id="local_x_4"
  id="val-(*.+%)"
  id="val-a"
  id="val-{b}4/shadowed/(src-a.ml)"
  id="val-c"
  id="val-{x}5/shadowed/(src-a.ml)"
  id="val-b"
  id="val-x"
  id="val-list"
  id="val-string"
  id="val-string2"

Html generation for implementation and mld/interface uses different commands

  $ odoc html-generate-source --indent -o html a.odocl
  odoc: required option --impl is missing
  Usage: odoc html-generate-source [OPTION]… FILE.ml
  Try 'odoc html-generate-source --help' or 'odoc --help' for more information.
  [2]
  $ odoc html-generate-source --indent -o html --impl a.odocl a.ml
  ERROR: Expected an implementation unit
  [1]
  $ odoc html-generate-source --indent -o html --impl impl-a.odocl
  odoc: required argument FILE.ml is missing
  Usage: odoc html-generate-source [OPTION]… FILE.ml
  Try 'odoc html-generate-source --help' or 'odoc --help' for more information.
  [2]
  $ odoc html-generate-source --indent -o html a.ml
  odoc: required option --impl is missing
  Usage: odoc html-generate-source [OPTION]… FILE.ml
  Try 'odoc html-generate-source --help' or 'odoc --help' for more information.
  [2]
  $ odoc html-generate --source a.ml --indent -o html impl-a.odocl
  odoc: unknown option '--source'.
  Usage: odoc html-generate [OPTION]… FILE.odocl
  Try 'odoc html-generate --help' or 'odoc --help' for more information.
  [2]

Compiling without --source-id makes it impossible to generate the source:

  $ odoc compile-impl -I . a.cmt
  $ odoc compile -I . a.cmt
  $ odoc link -I . a.odoc
  $ odoc link -I . impl-a.odoc
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate-source --impl impl-a.odocl --indent -o html a.ml
  ERROR: The implementation unit was not compiled with --source-id.
  [1]
