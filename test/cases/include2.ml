(** Comment about X that should not appear when including X below. *)
module X = struct
  type t = int
end

include X

module Y = struct
  (** Top-comment of Y. *)

  type t
end

module Y_include_synopsis = struct
  (** The [include Y] below should have the synopsis from [Y]'s top-comment
      attached to it. *)

  include Y
end

module Y_include_doc = struct
  include Y
  (** Doc attached to [include Y]. [Y]'s top-comment shouldn't appear here. *)
end
