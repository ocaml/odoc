(** Raw latex primitives:
    - macro and environment definitions
    - text escaping
*)

(** {1 Helper types } *)
type pr = Format.formatter -> unit
type 'a with_options = ?options:pr list -> 'a
type ('a,'b) tr = 'a Fmt.t -> 'b Fmt.t
type 'a t = ('a,'a) tr

(** {1 Helper functions } *)
module Escape: sig
  val text: code_hyphenation:bool -> string -> string
  val ref: string Fmt.t
end

val break: Types.break_hierarchy Fmt.t
val rightarrow: pr

val label: string Fmt.t
val verbatim: string Fmt.t


val pageref_star: string Fmt.t
val hyperref: string -> 'a t
val href: string -> 'a t
val ref: string Fmt.t
val url: string Fmt.t
val footnote: string Fmt.t

val emph: 'a t
val bold: 'a t
val subscript: 'a t
val superscript: 'a  t

val section: 'a t
val subsection: 'a t
val subsubsection: 'a t
val paragraph: 'a t

val enumerate: 'a t
val itemize: 'a t
val description: ('a, ('a * 'a) list) tr
val item: 'a t with_options

val small_table: ('a, 'a list list) tr


val input: Fpath.t Fmt.t

(** {1 Required OCaml-specific primitives }
    All the macro should be implemented as "ocaml"-suffixed macro
    in the latex preamble
  *)

(** {2 Code block customization} *)
val inline_code: 'a t
val code_fragment: 'a t
val code_block: 'a t

(** {2 Package-dependent primitives }*)

val indent: 'a t
(** expected to be implemented with changepage/adjustwidth*)

val longtable: column_desc:pr -> 'a t
(** any table implementation that can be split on multiple pages, e.g. longtable*)


(** {2 Tags } *)

val ocamltag: string -> 'a t
(** tag (e.g keyword, type-var, ...) are rendered to
{v \ocamltag{tagname}{content} v}
*)
