(** A functor whose result aliases its argument, applied by
    [include functor].

    OxCaml 5.4 rejects such an alias, as OCaml does, so this is kept apart from
    the [oxcaml.mli] case and runs on earlier versions of OxCaml only. *)

module Make (T : sig
  type t
  type 'a p
  module X : sig type v end
  module type S
  val via_module_type_include : unit
end) : sig
  module Aliased = T
  (** The input module itself. It should contain everything from the top
      level module *)
end

(**/**)
module type To_include_module_type = sig
  val via_module_type_include : unit
end
(**/**)

type t
type 'a p
module X : sig type v end
module type S = sig type u end
include To_include_module_type

include functor module type of Make
