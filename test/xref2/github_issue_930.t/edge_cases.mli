(** Edge case tests for GitHub issue #930 *)

(** Multiple TypeSubstitutions in same signature *)
module type Multi_subst = sig
  type 'a t := unit
  type 'b u := int

  val f : bool t
  val g : string u
end

(** Deeply nested includes - 5 levels deep *)
module type Level1 = sig
  type 'a t := unit
  val x : int t
end

module type Level2 = sig
  type t
  include Level1
end

module type Level3 = sig
  include Level2
end

module type Level4 = sig
  include Level3
end

module type Level5 = sig
  include Level4
end

(** Multiple paths to same signature *)
module type Multipath_base = sig
  type 'a t := unit
  val x : int t
end

module type Multipath_via_a = sig
  include Multipath_base
end

module type Multipath_via_b = sig
  include Multipath_base
end

module type Multipath_use1 = sig
  include Multipath_via_a
end

module type Multipath_use2 = sig
  include Multipath_via_b
end

(** TypeSubstitution with record field types *)
module type With_record = sig
  type 'a t := unit
  type r = { field : int t }
end
