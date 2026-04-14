val f : int -> ('a . 'a -> 'a) -> unit
(** Polymorphic arguments require parentheses *)

(** {1 Kind annotations on abstract types} *)

type t_immediate : immediate
(** Kind annotation on abstract type. *)

type t_value : value
(** [value] is the default kind, so the annotation is not rendered. *)

type t_any : any
(** A type with the [any] kind. *)

type t_float64 : float64
(** A type with the [float64] kind. *)

type t_bits32 : bits32
(** A type with the [bits32] kind. *)

type t_bits64 : bits64
(** A type with the [bits64] kind. *)

type t_word : word
(** A type with the [word] kind. *)

type t_void : void
(** A type with the [void] kind. *)

(** {1 Kind annotations with modalities} *)

type t_portable : value mod portable
(** Kind annotation with a modality. *)

type t_contended : value mod contended
(** Kind annotation with a different modality. *)

type t_multi_mod : value mod portable contended
(** Kind annotation with multiple modalities. *)

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
