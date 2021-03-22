(** Main module of this test. *)

module X = Test__x

(** An other example that is not an unit for comparison.
    @canonical Test.Y *)
module Test__y = struct
  type t
end

module Y = Test__y

(** An example with the tag inside the sig. *)
module Test__z = struct
  (** @canonical Test.Z *)

  type t
end

module Z = Test__z
