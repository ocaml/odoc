(* TODO Remove; this is needed for testing in the meantime. *)
exception InvalidReference of string
exception Expected_reference_to_a_module_but_got of string

module Paths = Model.Paths

val parse : string -> Paths.Reference.any
val read_path_longident : string -> [< Paths.Path.kind > `Module ] Paths.Path.t
val read_mod_longident : string -> Paths.Reference.module_
