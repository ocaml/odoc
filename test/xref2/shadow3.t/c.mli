include module type of struct include A end
include module type of struct include B end

module A : sig
  include module type of struct include A end
end

