open Common
module M = Char.Array_map

type 'a t =
  | Leaf of string * 'a
  | Node of
      { leaf : 'a option
      ; children : 'a t M.t
      }

let empty () = Node { leaf = None; children = M.of_seq Seq.empty }
let string_of_list li = li |> List.to_seq |> String.of_seq

let rec of_trie_gen trie_gen =
  match trie_gen with
  | Trie_gen.Leaf (chars, elt) -> Leaf (string_of_list chars, elt)
  | Trie_gen.Node { leaf; children } ->
      Node
        { leaf
        ; children =
            children |> Char.Map.to_seq |> Char.Array_map.of_seq
            |> Char.Array_map.map ~f:of_trie_gen
        }

let rec find ?(i = 0) path t =
  match t, path with
  | _, [] -> t
  | Node node, p :: path -> begin
      match M.find ~key:p node.children with
      | Some child -> find path child
      | None -> t
    end
  | Leaf (chars, _outcome), y :: ys when i < String.length chars && chars.[i] = y
    ->
      find ~i:(i + 1) ys t
  | _ -> t

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
        ~f:(fun ~key:_ ~acc child ->
          let res = fold_map merge transform child in
          match acc, res with
          | None, opt | opt, None -> opt
          | Some acc, Some res -> Some (merge acc res))
        ~init:leaf children

let rec map_leaf ~f t =
  match t with
  | Leaf (v, outcome) -> Leaf (v, f outcome)
  | Node { leaf; children } ->
      let leaf = Option.map f leaf in
      let children = M.map ~f:(map_leaf ~f) children in
      Node { leaf; children }
