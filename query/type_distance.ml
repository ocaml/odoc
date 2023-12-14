module Type_path = struct
  module Sign = Db.Typepath.Sign

  type t = string list list

  let rev_concat lst =
    List.fold_left (fun acc xs -> List.rev_append xs acc) [] lst

  let rec of_typ ~ignore_any ~prefix ~sgn t =
    match t with
    | Db.Typexpr.Poly _ ->
        let poly = "POLY" in
        [ poly :: Sign.to_string sgn :: prefix ]
    | Any ->
        if ignore_any
        then [ prefix ]
        else
          let poly = "POLY" in
          [ poly :: Sign.to_string sgn :: prefix ]
    | Arrow (a, b) ->
        let prefix_left = "->0" :: prefix in
        let prefix_right = "->1" :: prefix in
        List.rev_append
          (of_typ ~ignore_any ~prefix:prefix_left ~sgn:(Sign.not sgn) a)
          (of_typ ~ignore_any ~prefix:prefix_right ~sgn b)
    | Constr (name, args) ->
        let prefix = name :: Sign.to_string sgn :: prefix in
        begin
          match args with
          | [] -> [ prefix ]
          | _ ->
              rev_concat
              @@ List.mapi
                   (fun i arg ->
                     let prefix = string_of_int i :: prefix in
                     of_typ ~ignore_any ~prefix ~sgn arg)
                   args
        end
    | Tuple args ->
        rev_concat
        @@ List.mapi (fun i arg ->
               let prefix = (string_of_int i ^ "*") :: prefix in
               of_typ ~ignore_any ~prefix ~sgn arg)
        @@ args
    | Unhandled -> []

  let hcons_tbl = Hashtbl.create 16
  let uid_generator = ref 0

  let rec hcons = function
    | [] -> -1, []
    | x :: xs -> (
        let uid_xs, xs = hcons xs in
        match Hashtbl.find hcons_tbl (uid_xs, x) with
        | xxs -> xxs
        | exception Not_found ->
            let uid = !uid_generator in
            uid_generator := uid + 1 ;
            let result = uid, x :: xs in
            Hashtbl.add hcons_tbl (uid_xs, x) result ;
            result)

  (** [of_typ t] is a [string list list] representing
    the type [t]. It allows to compute the distance between two types. It is
    stored in the database to sort results once they are obtained. *)
  let of_typ ~ignore_any typ =
    List.map
      (fun xs ->
        let _, xs = hcons xs in
        xs)
      (of_typ ~ignore_any ~prefix:[] ~sgn:Pos typ)
end

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

let length typ =
  typ |> Type_path.of_typ ~ignore_any:false |> List.concat |> List.map String.length |> List.fold_left ( + ) 0

let v ~query ~element =
  let query_paths = Type_path.of_typ ~ignore_any:false query in
  let element_paths = Type_path.of_typ ~ignore_any:false element in
  match element_paths, query_paths with
  | _, [] | [], _ -> 0
  | _ ->
      let arr =
        List.map
          (fun p ->
            let p = List.rev p in
            List.map (fun q -> distance (List.rev q) p) query_paths)
          element_paths
      in
      minimize arr
