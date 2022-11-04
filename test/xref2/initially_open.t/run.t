Test the 'initially opened module' feature.

This causes module to be opened and in the environment at all times. Module, Module Type and
Type paths that can be simplified will be. By default 'Stdlib' is opened initially, therefore
paths that would otherwise be rendered as 'Stdlib.X.Y' will be rendered as 'X.Y'.

We test the feature by creating 2 modules, one referencing the other, and odoc compiling twice,
once with the module opened and once without.

  $ cat to_open.mli
  type t

  $ cat other.mli
  type t = To_open.t
  

  $ ocamlc -c -bin-annot to_open.mli
  $ ocamlc -c -bin-annot other.mli
  $ odoc compile --package x to_open.cmti


Run 'normally', without opening the module 'To_open':

  $ odoc compile --package x -I . other.cmti
  $ odoc link -I . other.odoc
  $ odoc html-generate -o . other.odocl --indent
  $ grep To_open x/Other/index.html
        <span> = <a href="../To_open/index.html#type-t">To_open.t</a></span>

Now try again, this time opening 'To_open', and expect the rendered link to be simplified:

  $ odoc compile --package x -I . other.cmti --open To_open
  $ odoc link -I . other.odoc
  $ odoc html-generate -o . other.odocl --indent
  $ grep To_open x/Other/index.html
        <span> = <a href="../To_open/index.html#type-t">t</a></span>

