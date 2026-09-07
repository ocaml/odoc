module type Pretty = sig
  type output
  val pp : int -> output
  val pp_hum : int -> output
end

module type Pretty_helpers = sig
  include Pretty with type output := string
  val default_indent : int
end

include Pretty_helpers

module Make (X : Pretty) : Pretty_helpers

module Private : sig
  val internal : int
end
