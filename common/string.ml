include Stdlib.String

let hash : t -> int = Hashtbl.hash

let pp = Format.pp_print_string