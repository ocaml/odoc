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

module M : sig

  (** Doc of [M] *)
end

module M' : sig end
(** Doc of [M'] from outside *)

(** Doc of [M''], part 1. *)
module M'' : sig

  (** Doc of [M''], part 2. *)
end

module Alias : T
(** Doc of [Alias]. *)

(** Doc of [c1], part 1. *)
class c1 :
  int
  -> object

       (** Doc of [c1], part 2. *)
     end

(** Doc of [ct], part 1. *)
class type ct =
  object

    (** Doc of [ct], part 2. *)
  end

class c2 : ct
(** Doc of [c2]. *)

module Ref_in_synopsis : sig
  (** {!t}.

      This reference should resolve in the context of this module, even when
      used as a synopsis. *)

  type t
end

module Comments_on_open : sig
  module M : sig
    type t
  end

  open M
      (** 
          {2:sec Section}

          Comments attached to open are treated as floating comments.
         Referencing {!section-sec} {!M.t} works *)
end
