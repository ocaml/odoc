(** Raw latex primitives:
    - macro and environment definitions
    - text escaping
*)

type pr = Format.formatter -> unit
(** {1 Helper types } *)

type 'a with_options = ?options:pr list -> 'a

type ('a, 'b) tr = 'a Fmt.t -> 'b Fmt.t

type 'a t = ('a, 'a) tr

(** {1 Helper functions } *)
module Escape : sig
  val text : code_hyphenation:bool -> string -> string

  val ref : string Fmt.t
end

val break : Types.break_hierarchy Fmt.t

val rightarrow : pr

val label : string Fmt.t

val verbatim : string Fmt.t

val pageref_star : string Fmt.t

val hyperref : string -> 'a t

val href : string -> 'a t

val ref : string Fmt.t

val url : string Fmt.t

val footnote : string Fmt.t

val emph : 'a t

val bold : 'a t

val subscript : 'a t

val superscript : 'a t

val section : 'a t

val subsection : 'a t

val subsubsection : 'a t

val paragraph : 'a t

val enumerate : 'a t

val itemize : 'a t

val description : ('a, ('a * 'a) list) tr

val item : 'a t with_options

val small_table : ('a, Types.alignment list option * 'a list list) tr

val input : Fpath.t Fmt.t

(** {1 Required OCaml-specific primitives }
    All the macro should be implemented as "ocaml"-suffixed macro
    in the latex preamble
  *)

val inline_code : 'a t
(** {2 Code block customization} *)

val code_fragment : 'a t

val code_block : 'a t

(** {2 Package-dependent primitives }*)

val indent : 'a t
(** expected to be implemented with changepage/adjustwidth*)

val ocamltabular : column_desc:pr -> 'a t
(** Any tabular implementation that works well with at most 10 rows *)

(** {2 Tags } *)

val ocamltag : string -> 'a t
(** tag (e.g keyword, type-var, ...) are rendered to
{v \ocamltag{tagname}{content} v}
*)

(** {2 Math mode} *)

val math : string Fmt.t

val equation : string Fmt.t
