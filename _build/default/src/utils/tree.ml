module List = Odoc_list

type 'a tree = { node : 'a; children : 'a forest }
and 'a forest = 'a tree list

module type S = sig
  type 'a t

  val fold_left : f:('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val iter : f:('a -> unit) -> 'a t -> unit
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val to_json : ('a -> Json.json) -> 'a t -> Json.json
end

type 'a t = 'a tree

let rec to_json json_of { node; children } : Json.json =
  `Object [ ("node", json_of node); ("children", to_json_f json_of children) ]

and to_json_f json_of f = `Array (List.map (to_json json_of) f)

let leaf node = { node; children = [] }

let rec fold_left ~f acc { node; children } =
  let acc = f acc node in
  fold_left_forest ~f acc children

and fold_left_forest ~f acc forest = List.fold_left (fold_left ~f) acc forest

let rec iter ~f { node; children } =
  let () = f node in
  iter_forest ~f children

and iter_forest ~f forest = List.iter (iter ~f) forest

let rec map ~f { node; children } =
  let node = f node in
  let children = map_forest ~f children in
  { node; children }

and map_forest ~f forest = List.map (map ~f) forest

let rec filter_map ~f { node; children } =
  match f node with
  | None -> None
  | Some node ->
      let children = filter_map_forest ~f children in
      Some { node; children }

and filter_map_forest ~f forest = List.filter_map (filter_map ~f) forest

module Forest = struct
  type 'a t = 'a forest

  let fold_left = fold_left_forest
  let iter = iter_forest
  let map = map_forest
  let filter_map = filter_map_forest
  let to_json = to_json_f
end
