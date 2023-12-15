module Int_map = Map.Make (Int)

type t = Entry.Array.t Int_map.t
type elt = int * Entry.t

let find = Int_map.find_opt
let fold = Int_map.fold
let is_empty = Int_map.is_empty
let equal_elt (a_count, a) (b_count, b) = a_count = b_count && Entry.equal a b

(*
let of_list li =
  List.fold_left
    (fun acc (count, elt) ->
      let elts = try Int_map.find count acc with Not_found -> [] in
      Int_map.add count (elt :: elts) acc)
    Int_map.empty li
  |> Int_map.map Entry.Array.of_list
*)

let of_list li =
  List.fold_left
    (fun acc (count, elt) ->
      match Int_map.find_opt count acc with
      | None -> Int_map.add count (Entry.Set.singleton elt) acc
      | Some set -> Int_map.add count (Entry.Set.add elt set) acc)
    Int_map.empty li
  |> Int_map.map (fun set -> set |> Entry.Set.to_seq |> Array.of_seq)
