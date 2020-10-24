module X : sig type t end
module Y : module type of struct include X end
module Z : module type of Y

