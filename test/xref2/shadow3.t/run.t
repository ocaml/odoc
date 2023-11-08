3 module shadowing! modules A and B are identical and contain shadowed modules. 
Module `C` then includes them both, causing further shadowing.

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot c.mli
  $ ocamlc -i c.mli
  module type B = B.B
  module type B1 = B.B1
  module A : sig type t = B.A.t type b = B.A.b end

  $ odoc compile a.cmti
  File "a.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(A).{A}1,true) end Unresolved module path identifier(root(A).{A}1,true) (Lookup failure (module): root(A).{A}1)
  File "a.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(A).{A}1,true) end Unresolved module path identifier(root(A).{A}1,true) (Lookup failure (module): root(A).{A}1)
  $ odoc compile b.cmti
  File "b.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(B).{A}1,true) end Unresolved module path identifier(root(B).{A}1,true) (Lookup failure (module): root(B).{A}1)
  File "b.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(B).{A}1,true) end Unresolved module path identifier(root(B).{A}1,true) (Lookup failure (module): root(B).{A}1)
  $ odoc compile -I . c.cmti
  File "c.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(C).{A}4,true) end Unresolved module path identifier(root(C).{A}4,true) (Lookup failure (module): root(C).{A}4)
 
  $ odoc_print --short --show-include-expansions c.odoc 
  include module type of struct include A end
    (sig :
      module type {B}5 = A.B
      include {B}5 (sig : module {A}11 = A.A end)
      module type {B1}7 = A.B1
      include {B1}7 (sig : module {A}12 = A.A end)
     end)
  include module type of struct include B end
    (sig :
      module type B = B.B
      include B (sig : module {A}17 = B.A end)
      module type B1 = B.B1
      include B1 (sig : module {A}18 = B.A end)
     end)
  module A : 
    sig
      include module type of struct include C.{A}4 end
        (sig : type t = B.A.t type b = B.A.b end)
    end
