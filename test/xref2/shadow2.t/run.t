  $ cat a.mli
  module type B1 = sig
    module A : sig
      type t
    end
  end
  
  include B1
  
  module type B2 = sig
    module A : sig
      include module type of struct include A end
      type u
    end
  end
  
  include B2
  
  module type B3 = sig
    module A : sig
      include module type of struct include A end
      type v
    end
  end
  
  include B3
  

In the above file we can see a chain of `B` modules that each contain
an `A` module extended from the previous. In each case the `A` is shadowed
except the last.

  $ ocamlc -c -bin-annot a.mli
  $ odoc compile a.cmti

Everything should link correctly.

  $ odoc link a.odoc

