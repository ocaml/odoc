val f : int -> ('a . 'a -> 'a) -> unit
(** Polymorphic arguments require parentheses *)

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

(** {1 Kind annotations on type aliases} *)

type t_alias : immediate = int
(** Has both a kind annotation and a manifest. *)

(** {1 Kind-constrained polymorphism in values} *)

val poly_immediate : ('a : immediate). 'a -> 'a
(** Kind constraint on a polymorphic type variable. *)

val poly_float64 : ('a : float64). 'a -> 'a
(** Kind constraint on a polymorphic type variable with a different kind. *)

(** {1 Modalities} *)

type opaque

type modalities_all = {
  f_global : opaque @@ global;
      (** Locality modality. *)
  f_local : opaque @@ local;
      (** Locality modality (identity, not rendered). *)
  f_unique : opaque @@ unique;
      (** Uniqueness modality. *)
  f_aliased : opaque @@ aliased;
      (** Uniqueness modality (identity, not rendered). *)
  f_many : opaque @@ many;
      (** Linearity modality. *)
  f_once : opaque @@ once;
      (** Linearity modality (identity, not rendered). *)
  f_portable : opaque @@ portable;
      (** Portability modality. *)
  f_nonportable : opaque @@ nonportable;
      (** Portability modality (identity, not rendered). *)
  f_uncontended : opaque @@ uncontended;
      (** Contention modality (identity, not rendered). *)
  f_contended : opaque @@ contended;
      (** Contention modality. *)
  f_unyielding : opaque @@ unyielding;
      (** Yield modality. *)
  f_yielding : opaque @@ yielding;
      (** Yield modality (identity, not rendered). *)
  f_forkable : opaque @@ forkable;
      (** Fork modality. *)
  f_unforkable : opaque @@ unforkable;
      (** Fork modality (identity, not rendered). *)
  f_stateless : opaque @@ stateless;
      (** Statefulness modality. *)
  f_stateful : opaque @@ stateful;
      (** Statefulness modality (identity, not rendered). *)
  f_immutable : opaque @@ immutable;
      (** Visibility modality. *)
  f_read_write : opaque @@ read_write;
      (** Visibility modality (identity, not rendered). *)
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
  | E
      (** Constant constructor. *)

(** {1 Modalities on values} *)

val portable_fn : (int -> int) @@ portable
(** Value with [portable] modality. *)

(** {1 Modalities on module declarations} *)

module type S = sig
  val x : int
  val f : string -> bool
  type s = { a : int }
end

module M1 : S
(** Module without modality. *)

module M2 : S @@ portable
(** Module with [portable] modality. The modality is applied to
    all value members of [M2]. *)

module M3 : sig @@ contended
  val f : string -> bool
  type s
end
(** [contended] modality applied to all definitions in the module. *)
