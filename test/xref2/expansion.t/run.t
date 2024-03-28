This test checks expansions. We can check what odoc has calculated against what the compiler outputs.
In addition, the expansion for the alias Test after the link phase should be the same as the expansion for S.

  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -i test.mli
  module H : sig type t end
  module S :
    sig
      module type X = sig module M : sig type t end end
      module X : sig module M : sig type t = int end end
      module Y : sig type t end
      module Z : sig type t end
      module A : X
      module B = H
    end
  module Test = S
  $ odoc compile test.cmti
  $ odoc_print --short --show-expansions test.odoc
   module H : sig type t end
  module S : 
    sig
      module type X = sig module M : sig type t end end
      module X : X with M.t = int (sig : module M : sig type t = int end end)
      module Y : sig type t end
      module Z : module type of Y (sig : type t end)
      module A : X (sig : module M : sig type t end end)
      module B = H
    end 
  module Test = S
  $ odoc link test.odoc
  $ odoc_print --short --show-expansions test.odocl
   module H : sig type t end
  module S : 
    sig
      module type X = sig module M : sig type t end end
      module X : X with M.t = int (sig : module M : sig type t = int end end)
      module Y : sig type t end
      module Z : module type of Y (sig : type t end)
      module A : X (sig : module M : sig type t end end)
      module B = H
    end 
  module Test = S
    (sig :
      module type X = sig module M : sig type t end end
      module X : X with M.t = int (sig : module M : sig type t = int end end)
      module Y : sig type t end
      module Z : module type of Y (sig : type t end)
      module A : X (sig : module M : sig type t end end)
      module B = H (sig : type t end)
     end)
