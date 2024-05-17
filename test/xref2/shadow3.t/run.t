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
  $ odoc compile b.cmti
  $ odoc compile -I . c.cmti
 
  $ odoc_print --short --show-include-expansions c.odoc 
  include module type of struct include A end
    (sig :
      module type {B}1/shadowed/(77138ec86b57ad030798718720da7ae8) = A.B
      include {B}1/shadowed/(77138ec86b57ad030798718720da7ae8)
        (sig :
          module {A}1/shadowed/(7a5745e369ca21540586d74c63c97108) = A.A
         end)
      module type {B1}2/shadowed/(77138ec86b57ad030798718720da7ae8) = A.B1
      include {B1}2/shadowed/(77138ec86b57ad030798718720da7ae8)
        (sig :
          module {A}3/shadowed/(77138ec86b57ad030798718720da7ae8) = A.A
         end)
     end)
  include module type of struct include B end
    (sig :
      module type B = B.B
      include B
        (sig :
          module {A}1/shadowed/(1ebdf715261163b09f55f3a423e7a0b0) = B.A
         end)
      module type B1 = B.B1
      include B1
        (sig :
          module {A}4/shadowed/(77138ec86b57ad030798718720da7ae8) = B.A
         end)
     end)
  module A : 
    sig
      include module type of struct include {A}4/shadowed/(77138ec86b57ad030798718720da7ae8) end
        (sig :
          include module type of struct include B.{A}1/shadowed/(1ebdf715261163b09f55f3a423e7a0b0) end
            (sig :
              type t = {A}4/shadowed/(77138ec86b57ad030798718720da7ae8).t
             end)
          type b = B.A.b
         end)
    end
