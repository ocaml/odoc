A bug report from JS. This code was causing a loop / stack overflow. Success is
simply finishing!

  $ ocamlc -c import.mli -bin-annot
  $ ocamlc -c a.mli -bin-annot

  $ odoc compile import.cmti -I . --unique-id IIII
  $ odoc compile a.cmti -I . --unique-id AAAA

  $ odoc link import.odoc -I .
  $ odoc link a.odoc -I .

  $ odoc_print --short a.odocl --show-expansions --show-include-expansions
  open [  ]
  include Import.S
    (sig :
      include Import.S0
        (sig :
          module {Thing}1/shadowed/(IIII) : sig module Config : sig  end end
         end)
      module {Thing}1/shadowed/(AAAA) : 
        sig module Config = {Thing}1/shadowed/(IIII).Config (sig :  end) end
     end)
  module Thing : sig  end


