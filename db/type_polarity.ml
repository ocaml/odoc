open Typexpr

module Sign = struct
  type t =
    | Pos
    | Neg

  let to_string = function
    | Pos -> "+"
    | Neg -> "-"

  let not = function
    | Pos -> Neg
    | Neg -> Pos
end

let rev_concat lst = List.fold_left (fun acc xs -> List.rev_append xs acc) [] lst

type t = string * int * Sign.t

let rec of_typ ~any_is_poly ~prefix ~sgn = function
  | Poly _ -> [ sgn, "POLY" :: prefix ]
  | Any -> if any_is_poly then [ sgn, "POLY" :: prefix ] else [ sgn, prefix ]
  | Arrow (a, b) ->
    List.rev_append
      (of_typ ~any_is_poly ~prefix ~sgn:(Sign.not sgn) a)
      (of_typ ~any_is_poly ~prefix ~sgn b)
  | Constr (name, args) -> begin
    let prefix = name :: prefix in
    match args with
    | [] -> [ sgn, prefix ]
    | _ ->
      rev_concat
      @@ List.mapi
           (fun i arg ->
             let prefix = string_of_int i :: prefix in
             of_typ ~any_is_poly ~prefix ~sgn arg)
           args
  end
  | Tuple args -> rev_concat @@ List.map (of_typ ~any_is_poly ~prefix ~sgn) @@ args
  | Unhandled -> []

let regroup lst =
  let h = Hashtbl.create 16 in
  List.iter
    (fun v ->
      let count =
        try Hashtbl.find h v with
        | Not_found -> 0
      in
      Hashtbl.replace h v (count + 1))
    lst ;
  Hashtbl.to_seq h

let of_typ ~any_is_poly t =
  t
  |> of_typ ~any_is_poly ~prefix:[] ~sgn:Pos
  |> List.map (fun (polarity, path) -> polarity, String.concat " " path)
  |> regroup
  |> Seq.map (fun ((polarity, path), count) -> path, count, polarity)
