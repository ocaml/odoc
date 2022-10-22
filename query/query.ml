module Parser = Query_parser
module Succ = Succ
module Storage = Db.Storage
open Db.Types

let inter_list = function
  | [] -> Succ.all
  | x :: xs -> List.fold_left Succ.inter x xs

let merge a b =
  Occ.merge
    (fun _ ox oy ->
      match ox, oy with
      | Some x, Some y -> Some (Succ.union (Succ.of_set x) y)
      | Some x, None -> Some (Succ.of_set x)
      | None, opt -> opt)
    a b

let collapse_trie t _acc =
  let open Db.Types.T in
  match t with
  | Leaf (_, outcome) -> outcome
  | Node { summary = Some s; _ } -> s
  | _ -> Occ.empty

let collapse_trie t =
  let r = collapse_trie t Occ.empty in
  let r = Occ.map Succ.of_set r in
  r

let collapse_triechar t _acc =
  let open Tchar in
  match t with
  | Leaf (_, outcome) -> outcome
  | Node { summary = Some s; _ } -> s
  | _ -> Elt_set.empty

let collapse_triechar t = Succ.of_set (collapse_triechar t Elt_set.empty)

let collapse_count ~count (t : Succ.t Occ.t) =
  Occ.fold
    (fun k x acc -> if k < count then acc else Succ.union x acc)
    t Succ.empty

let collapse_trie_with_poly name t =
  match name with
  | [ "POLY"; _ ] ->
      let open T in
      begin
        match t with
        | Leaf ([], s) | Node { leaf = Some s; _ } -> Occ.map Succ.of_set s
        | _ -> Occ.empty
      end
  | _ -> collapse_trie t

let sort x = x

let find_inter ~shards names =
  Lwt_list.fold_left_s
    (fun acc shard ->
      let db = shard.Storage.db in
      let r =
        sort @@ inter_list
        @@ List.map
             (fun (name, count) ->
               collapse_count ~count
               @@ collapse_trie_with_poly name
               @@ T.find name db)
             (regroup names)
      in
      let open Lwt.Syntax in
      let+ () = Lwt.pause () in
      Succ.union acc r)
    Succ.empty shards

let find_names ~shards names =
  let names = List.map (fun n -> List.rev (Db.list_of_string n)) names in
  Lwt_list.fold_left_s
    (fun acc shard ->
      let db_names = shard.Storage.db_names in
      let open Lwt.Syntax in
      let+ () = Lwt.pause () in
      let candidates =
        List.map
          (fun name ->
            let t = Tchar.find name db_names in
            collapse_triechar t)
          names
      in
      let candidates = inter_list candidates in
      Succ.union acc candidates)
    Succ.empty shards

let pp h set =
  Int_map.iter
    (fun cost values ->
      Elt_set.iter
        (fun value -> Format.fprintf h "(%i) %s\n%!" cost value.str_type)
        values)
    set

exception Abort of Elt.t list

let to_list results =
  let lst =
    try
      Int_map.fold
        (fun _ v acc ->
          let lst = List.rev_append (Elt_set.elements v) acc in
          if List.length lst > 200 then raise (Abort lst) ;
          lst)
        results []
    with Abort lst -> lst
  in
  List.rev lst
