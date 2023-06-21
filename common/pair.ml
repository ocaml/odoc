type ('a, 'b) t = 'a * 'b

let to_string ?(start = "(") ?(end_ = ")") ?(sep = ", ") to_string_a to_string_b
    (a, b) =
  String.concat "" [ start; to_string_a a; sep; to_string_b b; end_ ]
