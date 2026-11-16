kind_ k = value mod portable

type u

type t : k

type t2 = u

(** A kind abbreviation in a hidden module. *)
module Hidden__ : sig
  kind_ hk = value mod contended
end

type h : Hidden__.hk

module type S = sig
  kind_ sk = value mod portable

  type t : sk
end

module F (X : S) : sig
  type t : X.sk
end

module Arg : S

module App : module type of F (Arg)

module Local : sig
  kind_ lk = value mod portable

  type t
end

type l : Local.lk

type 'a w : immutable_data with u

module C : sig
  type t : Local.lk

  type t2 = Local.t
end

module D : sig
  include module type of C
end
