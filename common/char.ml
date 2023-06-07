include Stdlib.Char
module Map = Map.Make (Stdlib.Char)
module Array_map = Array_map.Make (Stdlib.Char)

let hash : char -> int = Hashtbl.hash
