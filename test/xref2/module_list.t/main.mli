(** {!modules:External External.X Main Internal Internal.Y Z F Type_of
    Type_of_str With_type Alias C1 C2 Inline_include Starts_with_open
    Resolve_synopsis External.Resolve_synopsis} *)

(** Doc for [Internal].

    An other paragraph*)
module Internal : sig
  module Y : sig end
  (** Doc for Internal.[X]. An other sentence. *)

  module C1 : sig end
  (** Doc for [C1].

      @canonical Main.C1 *)

  module C2 : sig

    (* Doc for [C2]. *)
  end
  (** @canonical Main.C2 *)
end

module Z : sig

  (** Doc for [Z]. *)
end

module F () : sig

  (** Doc for [F ()]. *)
end

module Type_of : module type of F ()

(* Without the extra blank lines in sig/struct, OCaml<4.06 doesn't see the doc
   comments. *)

module Type_of_str : module type of struct

  (** Doc of [Type_of_str]. *)
end

module type T = sig
  (** Doc for [T]. *)

  type t
end

module With_type : T with type t = int

module Alias = External.X
module C1 = Internal.C1
module C2 = Internal.C2

module Inline_include : sig
  include T
  (** @inline *)
end

module Resolve_synopsis : sig
  (** This should be resolved when included: {!Main.Resolve_synopsis.t}. These
      shouldn't: {!t} {!Resolve_synopsis.t} *)

  type t
end
