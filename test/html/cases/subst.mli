module type S = sig
  type 'a t
  val init : string t
end

module M : functor (T:sig type t end) ->S with type 'b t := ('b * T.t) list
