module type S = sig
  type t

  val copy : t -> t
end

module Make : functor (Element : S) -> sig
  module H : Hashtbl.S with type key = Element.t

  val cache : Element.t H.t
  val clear : unit -> unit
  val memo : H.key -> Element.t
end
