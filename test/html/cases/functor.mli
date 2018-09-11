module type S =
sig
  type t
end

(* NOTE(@ostera): re-enable this bit after 4.02.x support is dropped *)
(* module type S1 = S -> S *)
module type S1 = functor (_ : S) -> S

module F1 : functor (Arg : S) -> S

module F2 : functor (Arg : S) -> (S with type t = Arg.t)

module F3 : functor (Arg : S) ->
sig
  type t = Arg.t
end

module F4 (Arg : S) : S
