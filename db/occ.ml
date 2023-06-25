module Int_map = Map.Make (Int)

type t = Elt.Array.t Int_map.t
type elt = int * Elt.t

let is_empty = Int_map.is_empty

let of_list li =
  List.fold_left
    (fun acc (count, elt) ->
      match Int_map.find_opt count acc with
      | None -> Int_map.add count (Elt.Set.singleton elt) acc
      | Some set -> Int_map.add count (Elt.Set.add elt set) acc)
    Int_map.empty li
  |> Int_map.map (fun set -> set |> Elt.Set.to_seq |> Array.of_seq)
