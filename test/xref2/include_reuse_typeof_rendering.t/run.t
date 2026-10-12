Members that come from `include module type of X`, where X is an alias to
a module containing an include, render as aliases ([Make = X0.Make])
whether or not a `with` substitution is applied, so [User] and [User2]
render the same way. In particular, [u] keeps its equation and renders as
[type u = X0.u] in both, since `module type of <alias>` is fully
strengthened by the compiler.
The include in X0 is what matters here: members declared directly in X0
render as aliases in any case.

  $ ocamlc -c -bin-annot repro.mli
  $ odoc compile repro.cmti
  $ odoc link repro.odoc

  $ odoc_print --short --show-include-expansions repro.odocl
  module type S = 
    sig
      type t
      type u
      val v : u
      module type H = sig val h : int end
      module Make : (X/11 : H) -> sig val mk : t end
    end
  module X0 : 
    sig
      include S
        (sig :
          type t
          type u
          val v : u
          module type H = sig val h : int end
          module Make : (X/20 : H) -> sig val mk : t end
         end)
    end module X = X0
  module User : 
    sig
      type t = X.t
      include module type of X with [t(params ) = X.t]
        (sig :
          include S with [t(params ) = X.t]
            (sig :
              type u = X0.u
              val v : u
              module type H = X0.H
              module Make = X0.Make
             end)
         end)
    end
  module User2 : 
    sig
      include module type of X
        (sig :
          include S
            (sig :
              type t = X0.t
              type u = X0.u
              val v : u
              module type H = X0.H
              module Make = X0.Make
             end)
         end)
    end
