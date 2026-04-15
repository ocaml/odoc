module type X = sig type t val x : int end

type m = (module X)

val f0 : m -> unit

val f55 : (module M : X) -> M.t

val f' : (module M : X with type t = int) -> int

val f'' : (module X with type t = int) -> int

module type Y = sig type 'a t val return : 'a -> 'a t end

val g : (module M : Y) -> int M.t

val g' : (module M : Y) -> int

val g'' : (module Y) -> int

val map2: ('a. 'a -> 'a) -> 'a * 'b -> 'a * 'b
