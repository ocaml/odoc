(** The goal of this module is to allow to describe a markdown document and
        to print it.
        A markdown document is composed of [blocks]. *)

type inlines
(** Inlines elements are rendered one after the other, separated by spaces,
        but not by empty line. *)

val ( ++ ) : inlines -> inlines -> inlines
(** Combine inlines. *)

val join : inlines -> inlines -> inlines

type blocks
(** A block is composed of [inlines]. Blocks are separated by an empty line. *)

val ordered_list : blocks list -> blocks

val unordered_list : blocks list -> blocks

val ( +++ ) : blocks -> blocks -> blocks
(** Alias for {!blocks} *)

val blocks : blocks -> blocks -> blocks
(** Combine blocks. *)

val block_separator : blocks
(** A horizontal line between a heading and the body. *)

val text : string -> inlines
(** Some inline elements *)

val line_break : inlines

val noop : inlines

val bold : inlines -> inlines

val italic : inlines -> inlines

val superscript : inlines -> inlines

val subscript : inlines -> inlines

val link : href:string -> inlines -> inlines
(** Arbitrary link. *)

val anchor' : string -> inlines

val raw_markup : string -> blocks

val code_span : string -> string

val paragraph : inlines -> blocks

val code_block : string -> blocks

val quote_block : blocks -> blocks

val heading : int -> inlines -> blocks

val pp_blocks : Format.formatter -> blocks -> unit
(** Renders a markdown document. *)
