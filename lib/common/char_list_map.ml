module Self = Map.Make (struct
  type t = char list

  let compare = List.compare Char.compare
end)

include Self
