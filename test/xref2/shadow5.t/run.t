  $ cat a.mli
  module type Y = sig
    type t = int
    val y : t
    include sig
      type nonrec t = t
      val z : t
    end with type t := t
  end
  
  module type Z = sig
    include Y
  
    type nonrec t = t
  end
  

Here we have module type Z that causes shadowing of the type `t` from the included module type Y.
The test here is for a bug that existed where the shadowed items in the `include sig ... end with ...`
were being incorrectly shadowed themselves - ie, the `include sig type nonrec t = t ... end was being
rewritten to be `include sig type nonrec {t}1/... = t .. end` which is incorrect.

  $ ocamlc -bin-annot -c a.mli
  $ odoc compile a.cmti
  $ odoc link a.odoc

The odocl file ought to show that, within Z, the expansion of module type Y contains a shadowed
type `t`, but in the subsequent include, the type `t` within the signature _isn't_ mangled.

  $ odoc_print a.odocl --short --show-include-expansions
  module type Y = 
    sig
      type t = int
      val y : t
      include sigtype t = t
                   val z : tend with [t = t] (sig : val z : t end)
    end
  module type Z = 
    sig
      include Y
        (sig :
          type {t}1 = int
          val y : {t}1
          include sigtype {t}1 = {t}1
                       val z : {t}1end with [t = {t}1]
            (sig : type {t}1 = {t}1 val z : {t}1 end)
         end)
      type t = {t}1
    end
  $ odoc html-generate a.odocl -o html
  $ odoc support-files -o html


For comparison, another test case that didn't have the bug:

  $ cat b.mli
  module type X = sig
    type t
    val z : t
  end
  
  module type Y = sig
    type t = int
    val y : t
    include X with type t := t
  end
  
  module type Z = sig
    include Y
  
    type nonrec t = t
  end
  
  $ ocamlc -bin-annot -c b.mli
  $ odoc compile b.cmti
  $ odoc link b.odoc
  $ odoc_print b.odocl --short --show-include-expansions
  module type X = sig type t val z : t end
  module type Y = 
    sig type t = int val y : t include X with [t = t] (sig : val z : t end) end
  module type Z = 
    sig
      include Y
        (sig :
          type {t}1 = int
          val y : int
          include X with [t = int] (sig : val z : int end)
         end)
      type t = int
    end
