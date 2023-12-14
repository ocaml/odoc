include Map.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)
