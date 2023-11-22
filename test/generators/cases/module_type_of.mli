module type S = sig
    module type T = sig
        type t
    end

    module M : T

    module N : module type of struct include M end
end

module X : sig
    type t
    type u
end

module T : S with module M = X
