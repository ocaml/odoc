Check that we strengthen even the modules introduced in an include

  $ cat test.mli
  module type X = sig
    module Y : sig
      type t
    end
  end
  
  module Z : sig
    include X
  end
  
  module ZZ : sig
    include module type of struct include Z end
  end
  

In this example, module ZZ includes the strengthened contents of
Z. If the contents of the include in Z weren't strengthened, we would
end up with module Y in ZZ which would be unrelated do the Y in Z,
which is incorrect. Instead, we should end up with an alias in ZZ.
We can check this by looking for the expansion of Y in ZZ - if
the file 'html/x/Test/ZZ/Y/index.html' is there then the module has
_not_ been strengthened.

  $ ocamlc -bin-annot -c test.mli
  $ odoc compile --package x test.cmti
  $ odoc link test.odoc
  $ odoc html-generate test.odocl -o html
  $ find html/x/Test/ZZ
  html/x/Test/ZZ
  html/x/Test/ZZ/index.html

