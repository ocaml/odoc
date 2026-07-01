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
