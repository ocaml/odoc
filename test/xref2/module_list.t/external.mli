(** Doc for [External]. *)

(** Doc for [X]. *)
module X : sig
  type t
end

module Resolve_synopsis : sig
  (** {!t}

      This reference should be resolved when included the list in [Main]. *)

  type t
end

(** Edge case: forward references. The synopsis can't be fully linked in this
    case because we don't have the needed informations. This specific case is
    fine, though.

    {!modules:Main.Resolve_synopsis} *)
