module T : sig
  type t
end

module type T' = sig
  type t
end

type t

module Through_open : sig
  (** . *)

  (** {{!t} Before-open} *)

  open T

  (** {{!t} After-open} *)
end

module Through_include : sig
  (** . *)

  (** {{!t} Before-include} *)

  include T'

  (** {{!t} After-include} *)
end
