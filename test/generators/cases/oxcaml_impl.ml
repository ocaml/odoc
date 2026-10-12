let[@zero_alloc] add b x y = if b then x + y else x

module To_be_included = struct
  let[@zero_alloc] add b x y = if b then x + y else x
  (* [add] has a zero alloc annotation that it shouldn't loose *)
end

module Including = struct
  include To_be_included
end

(* The declarations below exercise reading modes and modalities from an
   implementation's [.cmt] file, as opposed to an interface's [.cmti]. *)

type opaque

(** {1 Modalities} *)

(** {2 Modalities on record fields} *)

type modalities_record = {
  f_global : opaque @@ global;  (** Locality modality. *)
  f_portable : opaque @@ portable;  (** Portability modality. *)
  f_multi : opaque @@ global portable;  (** Multiple modalities. *)
  f_plain : opaque;  (** No modality, for reference. *)
}

(** {2 Modalities on constructor arguments} *)

type modalities_variant =
  | A of string @@ global  (** Constructor argument with [global] modality. *)
  | B of (int -> int) @@ portable
      (** Function constructor argument with modality. *)
  | C of int @@ portable * string @@ global
      (** Per-element modalities in a constructor tuple. *)

(** {1 Modes} *)

(** {2 Modes in type definitions} *)

type mode_alias = int @ local -> int
(** Type alias for an arrow with a mode on its argument. *)

type mode_record = {
  fn : int @ local -> int;  (** Field whose type is an arrow with a mode. *)
  fn_both : int @ local -> int @ local;  (** Modes on both sides. *)
}

type mode_cstr =
  | Mc_arrow of (int @ local -> int)
      (** Constructor argument is a parenthesized arrow with a mode. *)
  | Mc_nested of ((int @ local -> int) -> unit)
      (** Nested arrow: higher-order with a mode on the inner argument. *)

(** {2 Modes on values} *)

let mode_arg : int @ local -> int = fun x -> x
(** Mode on a function argument, via a type annotation. *)

let mode_multi : string @ local once -> string @ local once = fun x -> x
(** Multiple modes on argument and return. *)

(** {1 Include functor on structures} *)

module Include_functor = struct
(** This module demonstrates the [include functor] functionality.  [Make] uses
    its argument, so [included] has to come out equal to [t] rather than
    abstract. *)
  module Make (T : sig type t end) = struct type included = T.t end
  type t
  include functor Make
end

module Include_functor_desugared = struct
(** This module is the desugared version from above *)
  module Make (T : sig type t end) = struct type included = T.t end
  type t
  module DUMMY__ = struct
    type nonrec t = t
  end
  include Make(DUMMY__)
end

module Resolve_functor = struct
  module F ( I : sig type t end ) = struct
    type myt = I.t
  end

  module M = struct
    type t = float
    include functor F
  end
end

module Multiple_include_functors = struct
(** Two [include functor]s in the same structure, with an item defined between
    them. *)
  module First (T : sig type t end) = struct type first = T.t end

  module Second (T : sig type t type first type between end) = struct
    type second = T.first
    type third = T.between
  end

  type t
  include functor First
  type between
  include functor Second
end

module Include_functor_not_last = struct
(** An [include functor] that is not the last item of the structure. *)
  module Make (T : sig type t end) = struct type included = T.t end

  type t
  include functor Make
  type after = string
end

module Anonymous_functor = struct
(** The functor is defined inline, so there is no path for odoc to apply: it
    has to bind the functor to a module first, as it does for every
    [include functor] in a signature. *)
  type t
  include functor (functor (T : sig type t end) -> struct type included = T.t end)
end
