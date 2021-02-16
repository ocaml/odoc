(** @canonical Test.Y *)
module type X = sig
        type t
end

module type Y = X

module type Z = X

