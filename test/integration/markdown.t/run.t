  $ ocamlc -c -bin-annot intf.mli markup.mli
  $ odoc compile intf.cmti
  $ odoc compile markup.cmti
  $ odoc link intf.odoc
  $ odoc link markup.odoc
  $ odoc markdown-generate intf.odocl -o markdown --generate-links
  $ odoc markdown-generate markup.odocl -o markdown --generate-links

  $ find markdown
  markdown
  markdown/Intf.md
  markdown/Markup.md
  markdown/Intf.M.md
  markdown/Intf.N.md

  $ cat markdown/Intf.md
  Intf
  
  Module Intf
  
  Synopsis.
  
  Rest of preamble.
  
  Floating comment at the top.
  
  <a id="type-t"></a>
  
  ###### type t
  
  Doc for `type t`
  
  <a id="val-x"></a>
  
  ###### val x :
  
  [t](#type-t)
  
  Doc for `val x`
  
  <a id="type-a"></a>
  
  ###### type a =
  
  [t](#type-t)
  
  Type alias
  
  <a id="type-b"></a>
  
  ###### type b =
  
  <a id="type-b.A"></a>
  
  ######    | A
  
  Doc for `A`
  
  <a id="type-b.B"></a>
  
  ######    | B
  
  Doc for `B`
  
  Doc for `type b`
  
  <a id="type-c"></a>
  
  ###### type c = {
  
  <a id="type-c.a"></a>
  
  ######    a : int ;
  
  Doc for `a`
  
  <a id="type-c.b"></a>
  
  ######    b : int ;
  
  Doc for `b`
  
  }
  
  Doc for `type c`
  
  <a id="val-y"></a>
  
  ###### val y :
  
  [ `One | `Two ]
  
  Polymorphic variant.
  
  Floating comment.
  
  <a id="val-z"></a>
  
  ###### val z :
  
  [t](#type-t) -> ( [t](#type-t) -> [t](#type-t) ) -> foo : [t](#type-t) -> ? bar : [t](#type-t) -> [ `One of [t](#type-t) ] -> [t](#type-t) * [t](#type-t)
  
  Type complicated enough to be rendered differently.
  
  <a id="module-M"></a>
  
  ###### module M
  
  Outer doc for `M`
  
  <a id="module-N"></a>
  
  ###### module N
  
  Doc for `N`
  
  <a id="module-type-S"></a>
  
  ###### module type S = sig
  
  <a id="type-t"></a>
  
  ######    type t
  
  end
  
  Doc for `S`

  $ cat markdown/Markup.md
  Markup
  
  Module Markup
  
  # This is a heading
  
  ## This has a label
  
  ---
  
  arrow (->) in a doc comment
  
  foo:bar : a raw markup
  
  ## Label
  
  ---
  
  [test_two](href)
  
  [**test**](href)
  
  [test two foo](href)
  
  [**barz**](href)
  
  ```
  verbatim
  text
  ```
  
  See if listness is preserved.
  
  This is an _interface_ with **all** of the _module system_ features. This documentation demonstrates:
  
  - comment formatting
  
  - unassociated comments
  
  - documentation sections
  
  - module system documentation including
    
    1. submodules
    
    2. module aliases
    
    3. module types
    
    4. module type aliases
    
    5. modules with signatures
    
    6. modules with aliased signatures
  
  A numbered list:
  
  1. 3
  
  2. 2
  
  3. 1
  
  David Sheets is the author.
  
  @author: David Sheets
  
  p1
  
  p2
  
  p3
  
  - a
  
  - b
  
  This is where I begin my thing from.
  
  1. one
  
  2. two
  
  - Mon
  
  - Tue
