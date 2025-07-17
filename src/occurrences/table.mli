type t
type impl_src = {filepath: string list; line_number: int}
type item = { direct : int; indirect : int ; impl_src : impl_src option}
type key = string
type identifier = Odoc_model.Paths.Identifier.t

val v : unit -> t
val v_item : unit -> item

val add : ?quantity:int -> t -> identifier -> impl_src option -> unit

val iter : (key -> item -> unit) -> t -> unit

val get : t -> identifier -> item option

val merge_into: src:t -> dst:t -> unit

module Deftbl : sig
  type key = Odoc_model.Paths.Identifier.SourceLocation.t
  type item = impl_src
  type t

  val v : unit -> t

  val add : t -> key -> item -> unit

  val get : t -> key -> item option
end
