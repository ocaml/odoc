include Stdlib.Option

module O = struct
  let ( let* ) = bind
  let ( let+ ) v f = map f v
end

let hash hash_a = function
  | Some a -> Hashtbl.hash (Some (hash_a a))
  | None -> Hashtbl.hash None

let pp = Format.pp_print_option
