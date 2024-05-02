module X = struct
  let x = 1

  (** some comment *)
end

module Y = X
module Z = Y

module L = Stdlib.List

module type X = sig
  val x : int
end

module type Y = X
module type Z = Y

module type L = module type of Stdlib.List
