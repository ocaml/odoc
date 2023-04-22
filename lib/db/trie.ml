module type ELEMENT = sig
  type t

  val compare : t -> t -> int
end

module Make (E : ELEMENT) = struct
  module M = Map.Make (E)

  type 'a t =
    | Leaf of E.t list * 'a
    | Node of
        { leaf : 'a option
        ; mutable summary : 'a option
        ; children : 'a t M.t
        }

  let empty () = Node { leaf = None; summary = None; children = M.empty }

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
    | Leaf (x :: xs, outcome), y :: ys when E.compare x y = 0 ->
        if xs = ys
        then Leaf (path, leaf (Some outcome))
        else
          Node
            { leaf = None
            ; summary = None
            ; children = M.singleton x (add ys leaf (Leaf (xs, outcome)))
            }
    | Leaf (x :: xs, outcome), y :: ys ->
        assert (E.compare x y <> 0) ;
        let children =
          M.add y (Leaf (ys, leaf None)) @@ M.singleton x (Leaf (xs, outcome))
        in
        Node { leaf = None; summary = None; children }
    | Leaf ([], outcome), [] -> Leaf ([], leaf (Some outcome))
    | Leaf ([], outcome), y :: ys ->
        Node
          { leaf = Some outcome
          ; summary = None
          ; children = M.singleton y (Leaf (ys, leaf None))
          }
    | Leaf (y :: ys, outcome), [] ->
        Node
          { leaf = Some (leaf None)
          ; summary = None
          ; children = M.singleton y (Leaf (ys, outcome))
          }

  let rec find path t =
    match t, path with
    | _, [] -> t
    | Node node, p :: path -> begin
        match M.find p node.children with
        | child -> find path child
        | exception Not_found -> t
      end
    | Leaf (x :: xs, outcome), y :: ys when E.compare x y = 0 ->
        find ys (Leaf (xs, outcome))
    | _ -> t

  let rec summarize fn z t =
    match t with
    | Leaf (_, outcome) -> outcome
    | Node ({ leaf; children; _ } as it) ->
        let acc =
          match leaf with
          | None -> z
          | Some z -> z
        in
        let sum =
          M.fold (fun _ c acc -> fn acc (summarize fn z c)) children acc
        in
        it.summary <- Some sum ;
        sum
end
