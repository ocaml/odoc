open Common
module M = Char.Map

type 'a t =
  | Leaf of char list * 'a
  | Node of
      { leaf : 'a option
      ; children : 'a t M.t
      }

let empty = Node { leaf = None; children = M.empty }

let rec add path leaf t =
  match t, path with
  | Node t, [] -> Node { t with leaf = Some (leaf t.leaf) }
  | Node t, p :: path ->
      let child =
        match M.find p t.children with
        | child -> add path leaf child
        | exception Not_found -> Leaf (path, leaf None)
      in
      Node { t with children = M.add p child t.children }
  | Leaf (x :: xs, outcome), y :: ys when x = y ->
      if xs = ys
      then Leaf (path, leaf (Some outcome))
      else
        Node
          { leaf = None
          ; children = M.singleton x (add ys leaf (Leaf (xs, outcome)))
          }
  | Leaf (x :: xs, outcome), y :: ys ->
      assert (x <> y) ;
      let children =
        M.add y (Leaf (ys, leaf None)) @@ M.singleton x (Leaf (xs, outcome))
      in
      Node { leaf = None; children }
  | Leaf ([], outcome), [] -> Leaf ([], leaf (Some outcome))
  | Leaf ([], outcome), y :: ys ->
      Node
        { leaf = Some outcome; children = M.singleton y (Leaf (ys, leaf None)) }
  | Leaf (y :: ys, outcome), [] ->
      Node
        { leaf = Some (leaf None)
        ; children = M.singleton y (Leaf (ys, outcome))
        }


let find path t = 
let rec loop i path t =
  match t, path with
  | _, [] ->
      Ok t
  | Node node, p :: path -> begin
      match M.find p node.children with
      | child -> loop (i + 1) path child
      | exception Not_found -> Error (`Stopped_at (i, t))
    end
  | Leaf (x :: xs, outcome), y :: ys when x = y -> loop (i + 1) ys (Leaf (xs, outcome))
  | _ ->
      Error (`Stopped_at (i, t))

  in 
  loop 0 path t

let rec fold_map merge transform t =
  match t with
  | Leaf (_, outcome) -> Some (transform outcome)
  | Node { leaf; children; _ } ->
      let leaf =
        match leaf with
        | None -> None
        | Some leaf -> Some (transform leaf)
      in
      M.fold
        (fun _ child acc ->
          let res = fold_map merge transform child in
          match acc, res with
          | None, opt | opt, None -> opt
          | Some acc, Some res -> Some (merge acc res))
        children leaf

let rec map_leaf ~f t =
  match t with
  | Leaf (v, outcome) -> Leaf (v, f outcome)
  | Node { leaf; children } ->
      let leaf = Option.map f leaf in
      let children = M.map (map_leaf ~f) children in
      Node { leaf; children }

let rec equal a_eq t1 t2 =
  if t1 == t2
  then true
  else
    match t1, t2 with
    | Leaf (chars, elt), Leaf (chars', elt') ->
        List.equal Char.equal chars chars' && a_eq elt elt'
    | Node { leaf; children }, Node { leaf = leaf'; children = children' } ->
        Option.equal a_eq leaf leaf' && M.equal (equal a_eq) children children'
    | _ -> false
