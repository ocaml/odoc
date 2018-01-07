module type Arg =
sig
  type t
end

module F : functor (Arg : Arg) ->
sig
  type t = Arg.t
end
