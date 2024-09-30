More advanced uses of `module type of`, including using functors.

  $ cat a.mli
  module X : sig type t end
  $ cat b.mli
  module X : sig type t end
  
  module type X1 = module type of X
  
  module type X2 = module type of A
  
  module type X3 = module type of A.X
  
  module Y : functor (A : sig type t end) -> sig type t end
  
  module type Foo = sig
      module X : sig type t end
  
      module Y : functor (A : sig type t end) -> sig module Z = X end
  
      module type Z = module type of Y
  
      module X' : module type of struct include X end
  end
  
  module type X4 = module type of Y
  
  module SubX : sig
      type t
      type u
  end
  
  module type X5 = Foo with module X := SubX
  
  module type X6 = module type of A.X
  

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli

Omitted for stability:
$ ocamlc -i b.mli

  $ odoc compile a.cmti
  $ odoc compile -I . b.cmti
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc

The expansions below should match OCaml's calculated signatures above.

  $ odoc_print b.odocl -r X1 --short --show-expansions
  module type B.X1 = module type of B.X
    (sig : type t end)
  $ odoc_print b.odocl -r X2 --short --show-expansions
  module type B.X2 = module type of A
    (sig : module X : sig type t end end)
  $ odoc_print b.odocl -r X3 --short --show-expansions
  module type B.X3 = module type of A.X
    (sig : type t end)
  $ odoc_print b.odocl -r X4 --short --show-expansions
  module type B.X4 = module type of B.Y
    (functor: (A/0 : 
    sig type t end) -> (sig : type t end))
  $ odoc_print b.odocl -r X5 --short --show-expansions
  module type B.X5 = B.Foo with X := B.SubX
    (sig :
      module Y : (A/3 : sig type t end) -> sig module Z = B.SubX end
      module type Z = module type of Y (functor: (A/6 : 
        sig type t end) -> (sig : module Z = B.SubX end))
      module X' : module type of struct include B.SubX end
        (sig : type t = B.SubX.t end)
     end)
  $ odoc_print b.odocl -r X6 --short --show-expansions
  module type B.X6 = module type of A.X
    (sig : type t end)
 
