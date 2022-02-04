(** The goal of this module is to allow to describe a markdown document and to
    print it. A markdown document is composed of {!blocks}, see {!pp_blocks}. *)

(** {2 Inline elements} *)

type inlines
(** Inlines elements are rendered one after the other, separated by spaces. *)

val ( ++ ) : inlines -> inlines -> inlines
(** Combine inlines, render a breakable space between two inlines. *)

val join : inlines -> inlines -> inlines
(** Join inlines without spaces in between. *)

val text : string -> inlines
(** An arbitrary string. *)

val line_break : inlines

val noop : inlines
(** Nothing. Isn't separated by spaces: [noop ++ x = x ++ noop = x]. *)

val bold : inlines -> inlines

val italic : inlines -> inlines

val superscript : inlines -> inlines

val subscript : inlines -> inlines

val link : href:string -> inlines -> inlines
(** Arbitrary link. *)

val anchor' : string -> inlines

(** {2 Block elements} *)

type blocks
(** Blocks are separated by an empty line. *)

val ordered_list : blocks list -> blocks

val unordered_list : blocks list -> blocks

val ( +++ ) : blocks -> blocks -> blocks
(** Alias for {!blocks} *)

val blocks : blocks -> blocks -> blocks
(** Combine blocks. *)

val block_separator : blocks
(** A horizontal line. *)

val raw_markup : string -> blocks

val code_span : string -> inlines

val paragraph : inlines -> blocks

val code_block : inlines -> blocks

val quote_block : blocks -> blocks

val heading : int -> inlines -> blocks

val noop_block : blocks
(** No blocks. [noop_block +++ x = x +++ noop_block = x]. *)

val pp_blocks : Format.formatter -> blocks -> unit
(** Renders a markdown document. *)
