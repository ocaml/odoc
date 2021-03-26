module type X = sig
  module Y : sig
    type t
  end
end

module Z : sig
  include X
end

module ZZ : sig
  include module type of struct include Z end
end

