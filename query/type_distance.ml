type step =
  | Type of string
  | Poly
  | Any
  | Arrow_left
  | Arrow_right
  | Product of
      { pos : int
      ; length : int
      }
  | Argument of
      { pos : int
      ; length : int
      }

module Sign = Db.Type_polarity.Sign

type t = step list list

let rev_concat lst = List.fold_left (fun acc xs -> List.rev_append xs acc) [] lst

let rec paths_of_type ~prefix t =
  match t with
  | Db.Typexpr.Poly _ -> [ Poly :: prefix ]
  | Any -> [ Any :: prefix ]
  | Arrow (a, b) ->
    let prefix_left = Arrow_left :: prefix in
    let prefix_right = Arrow_right :: prefix in
    List.rev_append
      (paths_of_type ~prefix:prefix_left a)
      (paths_of_type ~prefix:prefix_right b)
  | Constr (name, args) ->
    let prefix = Type name :: prefix in
    begin
      match args with
      | [] -> [ prefix ]
      | _ ->
        let length = List.length args in
        rev_concat
        @@ List.mapi
             (fun i arg ->
               let prefix = Argument { pos = i; length } :: prefix in
               paths_of_type ~prefix arg)
             args
    end
  | Tuple args ->
    let length = List.length args in
    rev_concat
    @@ List.mapi (fun i arg ->
      let prefix = Product { pos = i; length } :: prefix in
      paths_of_type ~prefix arg)
    @@ args
  | Unhandled -> []

let paths_of_type t = List.map List.rev @@ paths_of_type ~prefix:[] t

(* *)

let skip_entry _ = 10

let distance xs ys =
  let len_xs = List.length xs in
  let len_ys = List.length ys in
  let cache = Array.make_matrix (1 + len_xs) (1 + len_ys) (-1) in
  let inv = Db.Type_polarity.Sign.not in
  let rec memo ~xsgn ~ysgn i j xs ys =
    let r = cache.(i).(j) in
    if r >= 0
    then r
    else begin
      let r = go ~xsgn ~ysgn i j xs ys in
      cache.(i).(j) <- r ;
      r
    end
  and go ~xsgn ~ysgn i j xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _ -> 0
    | [ Any ], _ when xsgn = ysgn -> 0
    | [ Poly ], [ (Any | Poly) ] when xsgn = ysgn -> 0
    | Arrow_left :: xs, Arrow_left :: ys ->
      memo ~xsgn:(inv xsgn) ~ysgn:(inv ysgn) (i + 1) (j + 1) xs ys
    | x :: xs, y :: ys when x = y && xsgn = ysgn -> memo ~xsgn ~ysgn (i + 1) (j + 1) xs ys
    | _, Arrow_left :: ys -> 1 + memo ~xsgn ~ysgn:(inv ysgn) i (j + 1) xs ys
    | Arrow_left :: xs, _ -> 1 + memo ~xsgn:(inv xsgn) ~ysgn (i + 1) j xs ys
    | _, Arrow_right :: ys -> memo ~xsgn ~ysgn i (j + 1) xs ys
    | Arrow_right :: xs, _ -> memo ~xsgn ~ysgn (i + 1) j xs ys
    | _, [] -> 10_000
    | Product _ :: xs, Product _ :: ys -> 1 + memo ~xsgn ~ysgn (i + 1) (j + 1) xs ys
    | Argument _ :: xs, Argument _ :: ys -> 1 + memo ~xsgn ~ysgn (i + 1) (j + 1) xs ys
    | Product _ :: xs, ys -> 1 + memo ~xsgn ~ysgn (i + 1) j xs ys
    | xs, Product _ :: ys -> 1 + memo ~xsgn ~ysgn i (j + 1) xs ys
    | Type x :: xs', Type y :: ys' when xsgn = ysgn -> begin
      let skip_y = skip_entry y in
      match Name_cost.best_match ~sub:x y with
      | None -> skip_y + memo ~xsgn ~ysgn i (j + 1) xs ys'
      | Some (_, cost) -> (cost / 3) + memo ~xsgn ~ysgn (i + 1) (j + 1) xs' ys'
    end
    | xs, Type y :: ys' -> skip_entry y + memo ~xsgn ~ysgn i (j + 1) xs ys'
    | xs, Argument _ :: ys' -> memo ~xsgn ~ysgn i (j + 1) xs ys'
    | _, (Any | Poly) :: _ -> 10_000
  in
  let pos = Db.Type_polarity.Sign.Pos in
  go ~xsgn:pos ~ysgn:pos 0 0 xs ys

let minimize = function
  | [] -> 0
  | arr ->
    let used = Array.make (List.length (List.hd arr)) false in
    let arr =
      Array.map (fun lst ->
        let lst = List.mapi (fun i x -> x, i) lst in
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
        (* entry type is smaller than query type *)
        let score = acc + (1000 * (Array.length arr - i)) in
        best := min score !best ;
        true
      end
      else if i >= Array.length arr
      then begin
        (* query type is smaller than entry type *)
        let score = acc + (5 * rem) in
        best := min score !best ;
        true
      end
      else if acc + heuristics.(i) >= !best
      then true
      else begin
        let rec find = function
          | [] -> true
          | (cost, j) :: rest ->
            let continue =
              if used.(j)
              then true
              else begin
                used.(j) <- true ;
                let continue = go (rem - 1) (acc + cost) (i + 1) in
                used.(j) <- false ;
                continue
              end
            in
            if continue then find rest else false
        in
        find arr.(i)
      end
    in
    let _ = go (Array.length used) 0 0 in
    !best

let v ~query_paths ~entry =
  let entry_paths = paths_of_type entry in
  match entry_paths, query_paths with
  | _, [] | [], _ -> 0
  | _ ->
    let arr = List.map (fun p -> List.map (distance p) entry_paths) query_paths in
    minimize arr
