A bug report from JS. This code was causing a loop / stack overflow. Success is
simply finishing!

  $ ocamlc -c import.mli -bin-annot
  $ ocamlc -c a.mli -bin-annot

  $ odoc compile import.cmti -I .
  $ odoc compile a.cmti -I .

  $ odoc link import.odoc -I .
  $ odoc link a.odoc -I .

  $ odoc_print --short a.odocl --show-expansions --show-include-expansions
  open [  ]
  include Import.S
    (sig :
      include Import.S0
        (sig :
          module {Thing}1/shadowed/(3a2d2fba08409314e6d44caea0e32a6c) : 
            sig module Config : sig  end end
         end)
      module {Thing}1/shadowed/(6dc74508933c72a27f1a6b60f24a7e4f) : 
        sig
          module Config =
            {Thing}1/shadowed/(3a2d2fba08409314e6d44caea0e32a6c).Config
            (sig :  end)
        end
     end)
  module Thing : sig  end


