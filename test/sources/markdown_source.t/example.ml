type example_type = int

module ExampleModule = struct
  type inner_type = string

  (** This is a documented function *)
  let example_function x = x + 1

  let another_function s = String.length s
end

(** This is a documented value *)
let global_value = 42

(** This function demonstrates pattern matching *)
let pattern_match = function 0 -> "zero" | 1 -> "one" | _ -> "many"

exception CustomException of string

class example_class =
  object
    method greet name = "Hello, " ^ name
  end
