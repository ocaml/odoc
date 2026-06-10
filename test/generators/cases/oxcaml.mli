val f : int -> ('a . 'a -> 'a) -> unit
(** Polymorphic arguments require parentheses *)

(** Unboxed types have a trailing hash '#' *)

type pt = { x : int ; y : float32# }
type segment = { start : pt# ; stop : pt# }

(** {1 Layouts} *)

type t_any : any
(** Layout [any]. *)

type t_value_or_null : value_or_null
(** Layout [value_or_null]. *)

type t_float64 : float64
(** Layout [float64]. *)

type t_float32 : float32
(** Layout [float32]. *)

type t_word : word
(** Layout [word]. *)

type t_bits64 : bits64
(** Layout [bits64]. *)

type t_bits32 : bits32
(** Layout [bits32]. *)

type t_vec128 : vec128
(** Layout [vec128]. *)

type t_void : void
(** Layout [void]. *)

(** {1 Kind abbreviations} *)

type t_value : value
(** [value] is the default kind, so the annotation is not rendered. *)

type t_immediate : immediate
(** Kind abbreviation [immediate]. *)

type t_immediate64 : immediate64
(** Kind abbreviation [immediate64]. *)

type t_immutable_data : immutable_data
(** Kind abbreviation [immutable_data]. *)

type t_sync_data : sync_data
(** Kind abbreviation [sync_data]. *)

type t_mutable_data : mutable_data
(** Kind abbreviation [mutable_data]. *)

(** {1 Kind annotations with modalities} *)

type t_portable : value mod portable
(** Kind annotation with a modality. *)

type t_contended : value mod contended
(** Kind annotation with a different modality. *)

type t_multi_mod : value mod portable contended
(** Kind annotation with multiple modalities. *)

type t_everything : float64 mod everything
(** The [everything] bounds abbreviation. *)

(** {1 Kind annotations on parameterized types} *)

type ('a : immediate) imm_param
(** A type parameter with a kind constraint. *)

type ('a : float64) float_param
(** A type parameter with a different kind constraint. *)

type ('a : immediate, 'b : float64) multi_kind
(** Multiple kind-constrained parameters. *)

(** {1 Kind annotations with [with] constraints} *)

type 'a t_with : immediate with 'a
(** Kind annotation with a [with] constraint. *)

type 'a t_with_modalities : immutable_data with 'a @@ portable contended
(** Kind annotation with a [with] constraint and modalities. *)

(** {1 Kind annotations on type aliases} *)

type t_alias : immediate = int
(** Has both a kind annotation and a manifest. *)

(** {1 Kind-constrained polymorphism in values} *)

val poly_immediate : ('a : immediate). 'a -> 'a
(** Kind constraint on a polymorphic type variable. *)

val poly_float64 : ('a : float64). 'a -> 'a
(** Kind constraint on a polymorphic type variable with a different kind. *)

(** {1 Parenthesization of product kinds} *)

type t_many_modalities : value mod global aliased many contended portable forkable unyielding immutable stateless external_
(** A [mod] kind annotation with many modalities. *)

type t_outer_mod : float64 & immediate mod portable
(** Should render as [(float64 & immediate) mod portable]. *)

type t_inner_mod : float64 & (immediate mod portable)
(** Should render as [float64 & (immediate mod portable)]. *)

(** {1 Kind abbreviations} *)

kind_ my_abbrev = value_or_null mod non_null global
(** Declares a kind abbreviation named [my_abbrev]. *)

type t_abbrev : my_abbrev mod immutable
(** A type with an abbreviated kind. *)

(** {1 Zero alloc} *)

val add : bool -> int -> int -> int [@@zero_alloc]
(** Zero allocation bindings have an extension attribute attached.
    See https://oxcaml.org/documentation/miscellaneous-extensions/zero_alloc_check/
 *)

val add_opt : bool -> int -> int -> int [@@zero_alloc opt]
(** Like [add] but with an [opt] attribute.
 *)

val add_strict : bool -> int -> int -> int [@@zero_alloc strict]
(** Like [add] but with a [strict] attribute.
 *)

val add_strict_opt : bool -> int -> int -> int [@@zero_alloc strict opt]
(** Like [add] but with a [strict] and [opt] attributes.
 *)

val add_opt_strict : bool -> int -> int -> int [@@zero_alloc opt strict]
(** Like [add] but with a [strict] and [opt] attributes in reverse order.
 *)

val[@zero_alloc] alt_syntax : int -> int
(** Alternative syntax for zero alloc annotation *)

val curried_zero_alloc : int -> int -> int [@@zero_alloc arity 1]
(** Function that returns a function that is [zero_alloc].
 *)

(** {1 Modalities} *)

type opaque

type modalities_all = {
  f_global : opaque @@ global;
      (** Locality modality. *)
  f_local : opaque @@ local;
      (** Locality modality (local is not rendered). *)
  f_unique : opaque @@ unique;
      (** Uniqueness modality (unique is not rendered). *)
  f_aliased : opaque @@ aliased;
      (** Uniqueness modality. *)
  f_many : opaque @@ many;
      (** Linearity modality. *)
  f_once : opaque @@ once;
      (** Linearity modality (once is not rendered). *)
  f_portable : opaque @@ portable;
      (** Portability modality. *)
  f_nonportable : opaque @@ nonportable;
      (** Portability modality (nonportable is not rendered). *)
  f_uncontended : opaque @@ uncontended;
      (** Contention modality (uncontended is not rendered). *)
  f_contended : opaque @@ contended;
      (** Contention modality. *)
  f_unyielding : opaque @@ unyielding;
      (** Yield modality. *)
  f_yielding : opaque @@ yielding;
      (** Yield modality (yielding is not rendered). *)
  f_forkable : opaque @@ forkable;
      (** Fork modality. *)
  f_unforkable : opaque @@ unforkable;
      (** Fork modality (unforkable is not rendered). *)
  f_stateless : opaque @@ stateless;
      (** Statefulness modality. *)
  f_stateful : opaque @@ stateful;
      (** Statefulness modality (stateful is not rendered). *)
  f_immutable : opaque @@ immutable;
      (** Visibility modality. *)
  f_read_write : opaque @@ read_write;
      (** Visibility modality (read_write is not rendered). *)
  f_no_modality : opaque;
      (** No modality, for reference. *)
}

(** {1 Multiple modalities on a field} *)

type modalities_multi = {
  a : opaque @@ global portable;
      (** Field with [global portable] modalities. *)
}

(** {1 Modalities on tuple and function fields} *)

type modalities_tuple = {
  f : int * string @@ portable;
      (** Tuple field with modality. *)
}

type modalities_fn = {
  g : int -> int @@ portable;
      (** Function field with modality. *)
}

(** {1 Modalities on constructor arguments} *)

type modalities_cstr =
  | A of string @@ global
      (** Constructor argument with [global] modality. *)
  | B of (int -> int) @@ portable
      (** Function constructor argument with modality. *)
  | C of int * string @@ portable
      (** Tuple constructor argument with modality. *)
  | D of int @@ portable * string @@ global
      (** Per-element modalities in a constructor tuple. *)
  | E of { x : int @@ portable ; y : string @@ global }
      (** Per-element modalities in a constructor record. *)
  | F
      (** Constant constructor. *)

type 'a modalities_gadt =
  | A : string @@ global -> [`a] modalities_gadt
      (** Constructor argument with [global] modality. *)
  | B : (int -> int) @@ portable -> [`b] modalities_gadt
      (** Function constructor argument with modality. *)
  | C : int * string @@ portable -> [`c] modalities_gadt
      (** Tuple constructor argument with modality. *)
  | D : int @@ portable * string @@ global -> [`d] modalities_gadt
      (** Per-element modalities in a constructor tuple. *)
  | E : { x : int @@ portable ; y : string @@ global } -> [`e] modalities_gadt
      (** Per-element modalities in a constructor record. *)
  | F : [`f] modalities_gadt
      (** Constant constructor. *)

(** {2 Modalities on values} *)

val portable_fn : (int -> int) @@ portable
(** Value with [portable] modality. *)

(** {2 Modalities on module declarations} *)

module type S = sig
  type s = { a : int }
  val x : int
  val f : string -> bool
  val portable : string -> string array @@ portable
  val contended : string -> bytes @@ contended

  (** [uncontended] and [nonportable] are the defaults (not rendered). *)

  val uncontended : string -> bytes @@ uncontended
  val nonportable : string -> string array @@ nonportable
end

module M1 : S
(** Module without modality. *)

module M2 : S @@ portable
(** Module with [portable] modality. The modality is applied to
    all value members of [M2]. *)

module M3 : sig @@ contended
  type s
  val f : string -> bool
  val portable : string -> string array @@ portable
  val nonportable : string -> string array @@ nonportable
  val uncontended : string -> bytes @@ uncontended
end
(** [contended] modality applied to all definitions in the module, except the
    ones which have already specified this axis. *)
