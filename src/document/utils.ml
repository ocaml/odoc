let option_of_result = function Result.Ok x -> Some x | Result.Error _ -> None

let rec flatmap ?sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs -> (
      let hd = f x in
      let tl = flatmap ?sep ~f xs in
      match sep with None -> hd @ tl | Some sep -> hd @ sep @ tl)

let rec skip_until ~p = function
  | [] -> []
  | h :: t -> if p h then t else skip_until ~p t

let split_at ~f lst =
  let rec loop acc = function
    | hd :: _ as rest when f hd -> (List.rev acc, rest)
    | [] -> (List.rev acc, [])
    | hd :: tl -> loop (hd :: acc) tl
  in
  loop [] lst

let rec compute_length_source (t : Types.Source.t) : int =
  let f (acc : int) = function
    | Types.Source.Elt t -> acc + compute_length_inline t
    | Types.Source.Tag (_, t) -> acc + compute_length_source t
  in
  List.fold_left f 0 t

and compute_length_inline (t : Types.Inline.t) : int =
  let f (acc : int) { Types.Inline.desc; _ } =
    match desc with
    | Text s -> acc + String.length s
    | Entity _e -> acc + 1
    | Linebreak -> 0 (* TODO *)
    | Styled (_, t) | Link (_, t) | InternalLink { content = t; _ } ->
        acc + compute_length_inline t
    | Source s -> acc + compute_length_source s
    | Math _ -> assert false
    | Raw_markup _ -> assert false
    (* TODO *)
  in
  List.fold_left f 0 t
