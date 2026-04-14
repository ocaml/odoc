val f : int -> ('a . 'a -> 'a) -> unit
(** Polymorphic arguments require parentheses *)

(** {1 Kind annotations on abstract types}

    These type declarations have kind annotations that constrain their layout.
    Currently, odoc does not render the kind annotations. *)

type t_immediate : immediate
(** Should be [type t_immediate : immediate] *)

type t_value : value
(** Should be [type t_value : value] *)

type t_any : any
(** Should be [type t_any : any] *)

type t_float64 : float64
(** Should be [type t_float64 : float64] *)

type t_bits32 : bits32
(** Should be [type t_bits32 : bits32] *)

type t_bits64 : bits64
(** Should be [type t_bits64 : bits64] *)

type t_word : word
(** Should be [type t_word : word] *)

type t_void : void
(** Should be [type t_void : void] *)

(** {1 Kind annotations with modalities} *)

type t_portable : value mod portable
(** Should be [type t_portable : value mod portable] *)

type t_contended : value mod contended
(** Should be [type t_contended : value mod contended] *)

type t_multi_mod : value mod portable contended
(** Should be [type t_multi_mod : value mod portable contended] *)

(** {1 Kind annotations on parameterized types} *)

type ('a : immediate) imm_param
(** The kind constraint on ['a] is not rendered. *)

type ('a : float64) float_param
(** The kind constraint on ['a] is not rendered. *)

type ('a : immediate, 'b : float64) multi_kind
(** Multiple kind-constrained parameters. *)

(** {1 Kind annotations with [with] constraints} *)

type 'a t_with : immediate with 'a
(** Should be [type 'a t_with : immediate with 'a] *)

(** {1 Kind annotations on type aliases} *)

type t_alias : immediate = int
(** Has both a kind annotation and a manifest. *)

(** {1 Kind-constrained polymorphism in values} *)

val poly_immediate : ('a : immediate). 'a -> 'a
(** The kind constraint on ['a] is not rendered. *)

val poly_float64 : ('a : float64). 'a -> 'a
(** The kind constraint on ['a] is not rendered. *)
