include Stdlib.Int
module Map = Map.Make (Stdlib.Int)

let hash : int -> int = Hashtbl.hash

let pp = Format.pp_print_int