(** A doc comment at the beginning of a module is considered to be that
    module's doc. *)

(** Doc of [T], part 1. *)
module type T = sig
  (** Doc of [T], part 2. *)

  type t
end

module Include_inline : sig
  include T
  (** @inline *)
end

(** Doc of [Include_inline], part 1. *)
module Include_inline' : sig
  (** Doc of [Include_inline], part 2. *)

  include T
  (** part 3
      @inline *)
end

module type Include_inline_T = sig
  include T
  (** @inline *)
end

(** Doc of [Include_inline_T'], part 1. *)
module type Include_inline_T' = sig
  (** Doc of [Include_inline_T'], part 2. *)

  include T
  (** part 3
      @inline *)
end
