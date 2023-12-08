module X : sig type t end

module type X1 = module type of X

module type X2 = module type of A

module type X3 = module type of A.X

module Y : functor (A : sig type t end) -> sig type t end

module type Foo = sig
    module X : sig type t end

    module Y : functor (A : sig type t end) -> sig module Z = X end

    module type Z = module type of Y

    module X' : module type of struct include X end
end

module type X4 = module type of Y

module SubX : sig
    type t
    type u
end

module type X5 = Foo with module X := SubX

module type X6 = module type of A.X

