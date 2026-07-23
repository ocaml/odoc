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

type ('k, 'cmp) comparator
(** Helper used in the [with] constraints below. *)

type ('k, 'v, 'cmp) t_with_constr
  : immutable_data with 'k with 'v with ('k, 'cmp) comparator
(** A [with] constraint whose right-hand side is a parameterized type
    constructor (as found in [base]'s [Map] module). *)

type 'a t_with_app : immutable_data with 'a list
(** A [with] constraint whose right-hand side applies a type constructor. *)

type 'a t_with_arrow : immutable_data with ('a -> 'a)
(** A [with] constraint whose right-hand side is an arrow type. *)

type 'a t_with_arrow_modes : immutable_data with ('a -> 'a @ local)
(** A [with] constraint whose right-hand side is an arrow type carrying a
    mode. *)

type 'a t_with_tuple : immutable_data with ('a * int)
(** A [with] constraint whose right-hand side is a tuple type. *)

module M_with : sig
  type 'a t : immutable_data
end
(** Helper module used in the [with] constraint below. *)

type 'a t_with_dot : immutable_data with 'a M_with.t
(** A [with] constraint referring to a type through a module path
    ([M.t]). *)

module type Arg_with = sig
  type s
end

module F_with (X : Arg_with) : sig
  type 'a t : immutable_data
end
(** Helper functor used in the [with] constraint below. *)

module X_with : Arg_with

type 'a t_with_functor : immutable_data with 'a F_with(X_with).t
(** A [with] constraint referring to a type through a functor application
    ([F(X).t]). *)

type 'a t_with_variant : immutable_data with [ `Foo of 'a | `Bar ]
(** A [with] constraint whose right-hand side is a polymorphic variant. *)

type 'a t_with_object : immutable_data with < foo : 'a >
(** A [with] constraint whose right-hand side is an object type. *)

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

(** {1 Modes} *)

val mode_arg : int @ local -> int
(** Mode on a function argument. *)

val mode_ret : int -> int @ local
(** Mode on a function return. *)

val mode_both : int @ local -> int @ local
(** Modes on both argument and return. *)

val mode_multi : string @ local once -> string @ local unique
(** Multiple modes on argument and return. *)

val mode_multi_flipped : string @ once local -> string @ unique local
(** Same as [mode_multi], to show that modes order is normalized. *)

val mode_labeled : x:int @ local -> int
(** Mode on a labeled argument. *)

val mode_optional : ?x:int @ local -> unit -> int
(** Mode on an optional argument. *)

val mode_higher_order : ('a -> 'b) @ local -> 'a -> 'b
(** Mode on a higher-order function argument. *)

val mode_arrow_result : int -> (int -> int) @ local
(** Mode on a result that is itself an arrow. The arrow must be parenthesized so
    the mode does not appear to bind to the inner return type. *)

(** {2 Curry-implied result modes}

    Closing over an argument constrains the partial-application closure across
    several axes, not just locality. When the result mode is the one currying
    implies from the argument, it is suppressed (as the compiler does). *)

val curry_once : (int -> int) @ once -> int -> int
(** [once] argument: the implied [once] result mode is suppressed. *)

val curry_portable : (int -> int) @ portable -> int -> int
(** [portable] argument: the implied result mode is suppressed. *)

val curry_contended : (int -> int) @ contended -> int -> int
(** [contended] argument: the implied result mode is suppressed. *)

(** {2 Result modes that are kept}

    A result mode is only suppressed when it is exactly the one currying
    implies. An explicit mode on a different axis is kept (and the arrow result
    is parenthesized). *)

val keep_portable : int @ local -> (int -> int) @ portable
(** [portable] on the result is not implied by a [local] argument, so it is kept. *)

val keep_once : int @ local -> (int -> int) @ once
(** [once] on the result is not implied by a [local] argument, so it is kept. *)

val keep_over_once : (int -> int) @ once -> (int -> int) @ portable
(** The curry-implied [once] is suppressed, but the explicit [portable] is kept. *)

val keep_over_local : (int -> int) @ local -> (int -> int) @ portable
(** The curry-implied [local] is suppressed, but the explicit [portable] is kept. *)

val keep_portable_over_nonportable : (int -> int) @ nonportable -> (int -> int) @ portable
(** The [nonportable] argument mode is the default and dropped, while the
    explicit [portable] result, not implied by currying, is kept. *)

(** {2 All mode axes} *)

val mode_global : int @ global -> unit
(** Locality mode (legacy, not rendered). *)

val mode_local : int @ local -> unit
(** Locality mode. *)

val mode_aliased : int @ aliased -> unit
(** Uniqueness mode (legacy, not rendered). *)

val mode_unique : int @ unique -> unit
(** Uniqueness mode. *)

val mode_many : int @ many -> unit
(** Linearity mode (legacy, not rendered). *)

val mode_once : int @ once -> unit
(** Linearity mode. *)

val mode_portable : int @ portable -> unit
(** Portability mode. *)

val mode_shareable : int @ shareable -> unit
(** Portability mode (intermediate value). *)

val mode_nonportable : int @ nonportable -> unit
(** Portability mode (legacy, not rendered). *)

val mode_uncontended : int @ uncontended -> unit
(** Contention mode (legacy, not rendered). *)

val mode_shared : int @ shared -> unit
(** Contention mode. *)

val mode_contended : int @ contended -> unit
(** Contention mode. *)

val mode_yielding : int @ yielding -> unit
(** Yield mode. *)

val mode_unyielding : int @ unyielding -> unit
(** Yield mode (legacy, not rendered). *)

val mode_forkable : int @ forkable -> unit
(** Fork mode (identity on a non-[local] argument, not rendered). *)

val mode_local_forkable : int @ local forkable -> unit
(** Fork mode, rendered because the argument is also [local]. *)

val mode_unforkable : int @ unforkable -> unit
(** Fork mode. *)

val mode_local_unforkable : int @ local unforkable -> unit
(** Fork mode (identity for a [local] argument, not rendered). *)

val mode_stateless : int @ stateless -> unit
(** Statefulness mode. *)

val mode_stateful : int @ stateful -> unit
(** Statefulness mode (identity when [portability] is at its default, not rendered). *)

val mode_immutable : int @ immutable -> unit
(** Visibility mode. *)

val mode_read : int @ read -> unit
(** Visibility mode. *)

val mode_read_write : int @ read_write -> unit
(** Visibility mode (legacy, not rendered). *)

val mode_static : int @ static -> unit
(** Staticity mode. *)

val mode_dynamic : int @ dynamic -> unit
(** Staticity mode (legacy, not rendered). *)

(** {2 Cross-axis suppression}

    Some axes have a default value that is implied by another axis; the implied
    value is suppressed when rendering. *)

val mode_local_yielding : int @ local yielding -> unit
(** [yielding] is the default for [local], so it is not rendered. *)

val mode_local_unyielding : int @ local unyielding -> unit
(** [unyielding] is non-default for [local], so it is rendered. *)

val mode_immutable_contended : int @ immutable contended -> unit
(** [contended] is the default for [immutable], so it is not rendered. *)

val mode_immutable_uncontended : int @ immutable uncontended -> unit
(** [uncontended] is non-default for [immutable], so it is rendered. *)

val mode_stateless_portable : int @ stateless portable -> unit
(** [portable] is the default for [stateless], so it is not rendered. *)

val mode_stateful_portable : int @ stateful portable -> unit
(** [portable] is non-default for [stateful], so it is rendered. *)

(** {2 Modes in type definitions} *)

type mode_alias = int @ local -> int
(** Type alias for an arrow with a mode on its argument. *)

type mode_record = {
  fn : int @ local -> int;  (** Record field whose type is an arrow with a mode. *)
  fn_both : int @ local -> int @ local;  (** Arrow field with modes on both sides. *)
  mutable mfn : int @ local -> int;  (** Mutable arrow field with a mode. *)
}

type mode_cstr =
  | Mc_arrow of (int @ local -> int)
      (** Constructor argument is a parenthesized arrow with a mode. *)
  | Mc_nested of ((int @ local -> int) -> unit)
      (** Nested arrow: higher-order with a mode on the inner argument. *)
  | Mc_gadt : ('a @ once -> 'a) -> mode_cstr
      (** GADT constructor *)
