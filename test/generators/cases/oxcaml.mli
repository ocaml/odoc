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
