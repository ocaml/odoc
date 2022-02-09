(* Compatibility module for Cmdliner *)

(* Cmdliner 1.1.0 has deprecated the 'traditional' Term and Arg modules,
   but is only available for OCaml 4.08 and above. This compatibility
   module will work on 1.0.4 and 1.1.0 for all supported versions of
   the OCaml compiler without causing deprecation alerts *)

[@@@ocaml.warning "-3"]
module Term = struct
  include Cmdliner.Term

  type info2 = Cmdliner.Term.info
  let info = info
  let name = name
  let eval_choice = eval_choice
  let eval = eval
  let exit = Cmdliner.Term.exit
end

module Arg = struct
  include Cmdliner.Arg

  type 'a converter2 = 'a Cmdliner.Arg.converter
  let env_var = env_var
  let pconv = pconv
end
[@@@ocaml.warning "+3"]

