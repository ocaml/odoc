(** Test for GitHub issue #930: Crash when substituting for the same name at
    different arities.

    The original issue was that odoc crashed with [Invalid_argument("List.fold_left2")]
    when a signature had a destructive type substitution (type 'a t := unit) followed
    by an include that re-introduces a type with the same name but different arity.
*)

(** Original MWE from issue #930 *)
module type S1 = sig
  type t0

  type 'a t := unit
  (** Destructive substitution - 'a t becomes unit *)

  val x : t0 t
end

module type S2 = sig
  type t
  (** This [t] has arity 0, different from S1's ['a t] *)

  include S1 with type t0 := t
end

module type S3 = sig
  type t1

  include S2 with type t := t1
end

(** Updated MWE from issue #930 - simpler reproduction *)
module type Simple = sig
  type 'a t := unit

  include sig
    type 'a t

    val f : int t
  end
  with type 'a t := 'a t
end

(** Issue #1385 - related case with nested includes *)
module type Creators_base = sig
  type ('a, 'b, 'c) t
  type ('a, 'b, 'c) concat

  include sig
    type ('a, 'b, 'c) t
    val concat : (('a, 'p1, 'p2) t, 'p1, 'p2) concat -> ('a, 'p1, 'p2) t
  end
  with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

module type S0_with_creators_base = sig
  type t
  include Creators_base with type ('a, _, _) t := t and type ('a, _, _) concat := t
end
