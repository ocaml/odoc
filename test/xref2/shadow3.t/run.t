3 module shadowing! modules A and B are identical and contain shadowed modules. 
Module `C` then includes them both, causing further shadowing.

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot c.mli
  $ ocamlc -i c.mli
  module type B = B.B
  module A : sig type t = B.A.t type b = B.A.b end

  $ odoc compile a.cmti --unique-id AAAA
  $ odoc compile b.cmti --unique-id BBBB
  $ odoc compile -I . c.cmti --unique-id CCCC
 
  $ odoc_print --short --show-include-expansions c.odoc 
  include module type of struct include A end
    (sig :
      module type {B}1/shadowed/(CCCC) = A.B
      include {B}1/shadowed/(CCCC)
        (sig : module {A}1/shadowed/(AAAA) = A.A end)
      module type B1 := 
        sig
          module A : 
            sig
              include module type of struct include {A}1/shadowed/(AAAA) end
                (sig :
                  include module type of struct include A.{A}1/shadowed/(AAAA) end
                    (sig : type t = {A}1/shadowed/(AAAA).t end)
                  type a = A.A.a
                 end)
              type a
            end
        end
      include B1 (sig : module {A}2/shadowed/(CCCC) = A.A end)
     end)
  include module type of struct include B end
    (sig :
      module type B = B.B
      include B (sig : module {A}1/shadowed/(BBBB) = B.A end)
      module type B1 := 
        sig
          module A : 
            sig
              include module type of struct include {A}1/shadowed/(BBBB) end
                (sig :
                  include module type of struct include B.{A}1/shadowed/(BBBB) end
                    (sig : type t = {A}1/shadowed/(BBBB).t end)
                  type b = B.A.b
                 end)
              type b
            end
        end
      include B1 (sig : module {A}3/shadowed/(CCCC) = B.A end)
     end)
  module A : 
    sig
      include module type of struct include {A}3/shadowed/(CCCC) end
        (sig :
          include module type of struct include B.{A}1/shadowed/(BBBB) end
            (sig : type t = {A}3/shadowed/(CCCC).t end)
          type b = B.A.b
         end)
    end
