module Elt = Db.Types.Elt

let is_substring ~sub s =
  let re = Re.(compile (seq [ rep any; str sub ])) in
  Re.execp re s

let score_name query_name name =
  if String.starts_with ~prefix:query_name name
     || String.ends_with ~suffix:query_name name
  then 1
  else if is_substring ~sub:("(" ^ query_name) name
          || is_substring ~sub:(query_name ^ ")") name
  then 1
  else if is_substring ~sub:("." ^ query_name) name
          || is_substring ~sub:(query_name ^ ".") name
  then 2
  else if is_substring ~sub:("_" ^ query_name) name
          || is_substring ~sub:(query_name ^ "_") name
  then 3
  else if is_substring ~sub:query_name name
  then 4
  else (* Matches only in the docstring are always worse *) 2000

let score_name query_name name =
  match score_name query_name name with
  | 4 ->
      let query_name_lower = String.lowercase_ascii query_name in
      let name_lower = String.lowercase_ascii name in
      3
      + (if query_name = query_name_lower then 0 else 100)
      + score_name query_name_lower name_lower
  | c -> c

let score_name query_name name =
  List.fold_left
    (fun acc query_name -> acc + score_name query_name name)
    0 query_name

let distance xs ys =
  let len_xs = List.length xs in
  let len_ys = List.length ys in
  let cache = Array.make_matrix (1 + len_xs) (1 + len_ys) (-1) in
  let rec memo i j xs ys =
    let r = cache.(i).(j) in
    if r >= 0
    then r
    else begin
      let r = go i j xs ys in
      cache.(i).(j) <- r ;
      r
    end
  and go i j xs ys =
    match xs, ys with
    | [], _ -> 0
    | [ "_" ], _ -> 0
    | _, [] -> List.length xs
    | x :: xs, y :: ys when String.ends_with ~suffix:x y ->
        memo (i + 1) (j + 1) xs ys
    | _, "->1" :: ys -> memo i (j + 1) xs ys
    | "->1" :: xs, _ -> 1 + memo (i + 1) j xs ys
    | _ :: xs', _ :: ys' ->
        7
        + min
            (memo (i + 1) (j + 1) xs' ys')
            (min (memo (i + 1) j xs' ys) (memo i (j + 1) xs ys'))
  in
  go 0 0 xs ys

let minimize = function
  | [] -> 0
  | arr ->
      let used = Array.make (List.length (List.hd arr)) false in
      let arr =
        Array.map (fun lst ->
            let lst = (1, None) :: List.mapi (fun i x -> x, Some i) lst in
            List.sort Stdlib.compare lst)
        @@ Array.of_list arr
      in
      Array.sort (fun xs ys -> Stdlib.compare xs ys) arr ;
      let heuristics = Array.make (Array.length arr + 1) 0 in
      for i = Array.length heuristics - 2 downto 0 do
        let best = fst (List.hd arr.(i)) in
        heuristics.(i) <- heuristics.(i + 1) + best
      done ;
      let best = ref 1000 in
      let limit = ref 0 in
      let rec go rem acc i =
        incr limit ;
        if !limit > 10_000
        then false
        else if rem <= 0
        then begin
          let score = acc + (1 * (Array.length arr - i)) in
          best := min score !best ;
          true
        end
        else if i >= Array.length arr
        then begin
          best := min !best (acc + (100 * rem)) ;
          true
        end
        else if acc + heuristics.(i) >= !best
        then true
        else
          let rec find = function
            | [] -> true
            | (cost, j) :: rest ->
                let ok =
                  match j with
                  | None ->
                      go rem
                        (acc + cost
                        + if rem > Array.length arr - i then 100 else 0)
                        (i + 1)
                  | Some j ->
                      if used.(j)
                      then true
                      else begin
                        used.(j) <- true ;
                        let ok = go (rem - 1) (acc + cost) (i + 1) in
                        used.(j) <- false ;
                        ok
                      end
                in
                if ok then find rest else false
          in
          find arr.(i)
      in
      let _ = go (Array.length used) 0 0 in
      !best

let score_type query_type paths =
  match paths, query_type with
  | _, [] | [], _ -> 0
  | _ ->
      let arr =
        List.map
          (fun p ->
            let p = List.rev p in
            List.map (fun q -> distance (List.rev q) p) query_type)
          paths
      in
      minimize arr

let list query_name query_type results =
  let results =
    List.map
      (fun a ->
        let open Elt in
        let name_cost = score_name query_name a.name in
        let type_cost =
          match a.kind with
          | Val { type_paths; _ } -> score_type query_type type_paths
          | Type | Module | ModuleType | Exception -> 0
        in
        let cost = a.Elt.cost + (2 * name_cost) + (800 * type_cost) in
        { a with cost })
      results
  in
  List.sort Elt.compare results
