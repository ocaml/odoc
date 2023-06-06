include Stdlib.List

let sort_map ~f ~compare li =
  li
  |> map (fun elt -> elt, f elt)
  |> sort (fun (_, wit) (_, wit') -> compare wit wit')
  |> map (fun (elt, _) -> elt)

let hash hash_a li = li |> map hash_a |> Hashtbl.hash
