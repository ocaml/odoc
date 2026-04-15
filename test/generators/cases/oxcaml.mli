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
