type 'a t = { node : 'a; children : 'a forest }
and 'a forest = 'a t list

let leaf node = { node; children = [] }

let rec fold_t fun_ acc { node; children } =
  let acc = fun_ acc node in
  fold_f fun_ acc children

and fold_f fun_ acc f = List.fold_left (fold_t fun_) acc f

let rec iter_t fun_ { node; children } =
  let () = fun_ node in
  iter_f fun_ children

and iter_f fun_ f = List.iter (iter_t fun_) f

let rec map_t fun_ { node; children } =
  let node = fun_ node in
  let children = map_f fun_ children in
  { node; children }

and map_f fun_ f = List.map (map_t fun_) f

let rec filter_map_t fun_ { node; children } =
  match fun_ node with
  | None -> None
  | Some node ->
      let children = filter_map_f fun_ children in
      Some { node; children }

and filter_map_f fun_ f = List.filter_map (filter_map_t fun_) f
