open Types

type t

val pf : t -> ('a, t, unit) format -> 'a

val elt : Inline.t -> t -> unit

val entity : Inline.entity -> t -> unit

val ignore : (t -> unit) -> t -> unit

val span : ?attr:string -> (t -> unit) -> t -> unit

val txt : string -> t -> unit

val noop : t -> unit

val cut : t -> unit

val sp : t -> unit

val list : ?sep:(t -> unit) -> f:('a -> t -> unit) -> 'a list -> t -> unit

val box_hv : (t -> unit) -> t -> unit

val box_hv_no_indent : (t -> unit) -> t -> unit

val render : (t -> unit) -> Source.t

val code : ?attr:string list -> (t -> unit) -> Inline.t

val documentedSrc : (t -> unit) -> DocumentedSrc.t

val codeblock : ?attr:Class.t -> (t -> unit) -> Block.t

val keyword : string -> t -> unit

module Infix : sig
  val ( ++ ) : (t -> unit) -> (t -> unit) -> t -> unit
end
