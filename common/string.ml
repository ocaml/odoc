include Stdlib.String

let hash : t -> int = Hashtbl.hash
let pp = Format.pp_print_string

module Hashtbl = Hashtbl.Make (struct
  type nonrec t = t

  let equal = equal
  let hash = hash
end)

let rev str =
  let len = length str in
  init len (fun i -> get str (len - i - 1))
