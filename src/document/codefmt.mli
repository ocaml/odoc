open Types

type t

val elt : Inline.t -> t

val entity : Inline.entity -> t

val ignore : t -> t

val span : ?attr:string -> t -> t

val txt : string -> t

val noop : t

val cut : t

val sp : t

val list : ?sep:t -> f:('a -> t) -> 'a list -> t

val box_hv : t -> t

val box_hv_no_indent : t -> t

val render : t -> Source.t

val code : ?attr:string list -> t -> Inline.t

val documentedSrc : t -> DocumentedSrc.t

val codeblock : ?attr:Class.t -> t -> Block.t

val keyword : string -> t

module Infix : sig
  val ( ++ ) : t -> t -> t
end
