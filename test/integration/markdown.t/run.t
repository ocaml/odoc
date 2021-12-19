  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc markdown-generate test.odocl -o markdown
  $ cat markdown/Test.md
  Test
  
  Module Test
  
  # This is a heading
  
  ## This has a label
  
  ---
  
  arrow (->) in a doc comment
  
  ######       val concat : string -> string -> string
  
  foo:bar : a raw markup
  
  ######       type t
  
  Doc for `type t`
  
  ######       type a = t
  
  ######       type y'
  
  ######       module type Foo' = sig
  
  ######           type foo
  
  ######       end
  
  ######       module Bar : sig ... end
  
  ######       module type Bar' = sig
  
  ######           type bar'
  
  ######       end
  
  ######       module type Foo = sig
  
  ######           type foo
  
  ######           type bar'
  
  ######           module type Foo' = sig
  
  ######               type foo'
  
  ######               type days =
  
  ######                   | Mon
  
  Docs for `days`
  
  ######               type num = [
  
  ######                   | `One
  
  Docs for `` `One``
  
  ######               ]
  
  ######           end
  
  ######       end
  
  ######       type other_names = {
  
  ######           given : string ;
  
  ######           nickname : string ;
  
  ######       }
  
  ######       type name = {
  
  ######           fname : string ;
  
  Docs for `fname`
  
  ######           lname : string ;
  
  ######           others : other_names ;
  
  ######       }
  
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
  
  ######       module Foo : sig ... end
  
  The end foo end keyword in doc comment.
  
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
  $ odoc markdown-generate test.odocl -o markdown --generate-links
  $ cat markdown/Test.md
  Test
  
  Module Test
  
  # This is a heading
  
  ## This has a label
  
  ---
  
  arrow (->) in a doc comment
  
  <a id="val-concat"></a>
  
  ######       val concat : string -> string -> string
  
  foo:bar : a raw markup
  
  <a id="type-t"></a>
  
  ######       type t
  
  Doc for `type t`
  
  <a id="type-a"></a>
  
  ######       type a = [t](#type-t)
  
  <a id="type-y'"></a>
  
  ######       type y'
  
  <a id="module-type-Foo'"></a>
  
  ######       module type Foo' = sig
  
  <a id="type-foo"></a>
  
  ######           type foo
  
  ######       end
  
  <a id="module-Bar"></a>
  
  ######       module Bar : sig ... end
  
  <a id="module-type-Bar'"></a>
  
  ######       module type Bar' = sig
  
  <a id="type-bar'"></a>
  
  ######           type bar'
  
  ######       end
  
  <a id="module-type-Foo"></a>
  
  ######       module type Foo = sig
  
  <a id="type-foo"></a>
  
  ######           type foo
  
  <a id="type-bar'"></a>
  
  ######           type bar'
  
  <a id="module-type-Foo'"></a>
  
  ######           module type Foo' = sig
  
  <a id="type-foo'"></a>
  
  ######               type foo'
  
  <a id="type-days"></a>
  
  ######               type days =
  
  <a id="type-days.Mon"></a>
  
  ######                   | Mon
  
  Docs for `days`
  
  <a id="type-num"></a>
  
  ######               type num = [
  
  <a id="type-num.One"></a>
  
  ######                   | `One
  
  Docs for `` `One``
  
  ######               ]
  
  ######           end
  
  ######       end
  
  <a id="type-other_names"></a>
  
  ######       type other_names = {
  
  <a id="type-other_names.given"></a>
  
  ######           given : string ;
  
  <a id="type-other_names.nickname"></a>
  
  ######           nickname : string ;
  
  ######       }
  
  <a id="type-name"></a>
  
  ######       type name = {
  
  <a id="type-name.fname"></a>
  
  ######           fname : string ;
  
  Docs for `fname`
  
  <a id="type-name.lname"></a>
  
  ######           lname : string ;
  
  <a id="type-name.others"></a>
  
  ######           others : [other_names](#type-other_names) ;
  
  ######       }
  
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
  
  <a id="module-Foo"></a>
  
  ######       module Foo : sig ... end
  
  The end foo end keyword in doc comment.
  
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
