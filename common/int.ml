include Stdlib.Int
module Map = Map.Make (Stdlib.Int)

let hash : int -> int = Hashtbl.hash
