type t = Elt.Array.t Int.Map.t
type elt = int * Elt.t

let is_empty = Int.Map.is_empty

let of_list li =
  List.fold_left
    (fun acc (count, elt) ->
      match Int.Map.find_opt count acc with
      | None -> Int.Map.add count (Elt.Set.singleton elt) acc
      | Some set -> Int.Map.add count (Elt.Set.add elt set) acc)
    Int.Map.empty li
  |> Int.Map.map (fun set -> set |> Elt.Set.to_seq |> Array.of_seq)

let pprint_elt (count, elt) =
  let open PPrint in
  OCaml.int count ^^ space ^^ Elt.pprint elt

let pprint t =
  let open PPrint in
  Int.Map.fold
    (fun i arr doc ->
      group
      @@ group (parens (OCaml.int i ^^ space ^^ align (Elt.Array.pprint arr)))
      ^^ break 1 ^^ doc)
    t empty
