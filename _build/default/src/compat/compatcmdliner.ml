(* Compatibility module for Cmdliner *)

(* Cmdliner 1.1.0 has deprecated the 'traditional' Term and Arg modules,
   but is only available for OCaml 4.08 and above. This compatibility
   module will work on 1.0.4 and 1.1.0 for all supported versions of
   the OCaml compiler without causing deprecation alerts *)

[@@@ocaml.warning "-3"]

module Term = struct
  open Cmdliner.Term

  type 'a t = 'a Cmdliner.Term.t

  type info = Cmdliner.Term.info

  let info = info

  let name = name

  let eval_choice = eval_choice

  let eval = eval

  let exit = Cmdliner.Term.exit

  let ( $ ) = ( $ )

  let const = const
end

module Arg = struct
  open Cmdliner.Arg

  type 'a conv = 'a Cmdliner.Arg.conv

  let env_var = env_var

  let pconv = pconv

  let string = string

  let value = value

  let opt = opt

  let opt_all = opt_all

  let info = info

  let some = some

  let flag = flag

  let required = required

  let pos = pos

  let pos_all = pos_all

  let file = file

  let dir = dir

  let bool = bool

  let ( & ) = ( & )

  let conv = conv
  let conv_parser = conv_parser
  let conv_printer = conv_printer

  let non_empty = non_empty
end

[@@@ocaml.warning "+3"]
