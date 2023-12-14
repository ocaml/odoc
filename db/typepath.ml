open Typexpr

let regroup lst =
  String_list_map.bindings
  @@ List.fold_left
       (fun acc s ->
         let count = try String_list_map.find s acc with Not_found -> 0 in
         String_list_map.add s (count + 1) acc)
       String_list_map.empty lst

module Sign = struct
  type t =
    | Pos
    | Neg
    | Unknown

  let to_string = function
    | Pos -> "+"
    | Neg -> "-"
    | Unknown -> "+"

  let not = function
    | Pos -> Neg
    | Neg -> Pos
    | Unknown -> Unknown
end

let rev_concat lst =
  List.fold_left (fun acc xs -> List.rev_append xs acc) [] lst

let rec tails = function
  | [] -> []
  | _ :: xs as lst -> lst :: tails xs

module For_suffix_tree = struct
  type t = string list list

  let all_type_names name =
    name |> String.split_on_char '.' |> tails |> List.map (String.concat ".")

  let rec of_typ ~ignore_any ~all_names ~prefix ~sgn = function
    | Poly _ -> [ "POLY" :: Sign.to_string sgn :: prefix ]
    | Any ->
        if ignore_any
        then [ prefix ]
        else [ "POLY" :: Sign.to_string sgn :: prefix ]
    | Arrow (a, b) ->
        List.rev_append
          (of_typ ~ignore_any ~all_names ~prefix ~sgn:(Sign.not sgn) a)
          (of_typ ~ignore_any ~all_names ~prefix ~sgn b)
    | Constr (name, args) ->
        name
        |> (if all_names then all_type_names else fun name -> [ name ])
        |> List.map (fun name ->
               let prefix = name :: Sign.to_string sgn :: prefix in
               begin
                 match args with
                 | [] -> [ prefix ]
                 | _ ->
                     rev_concat
                     @@ List.mapi
                          (fun i arg ->
                            let prefix = string_of_int i :: prefix in
                            of_typ ~ignore_any ~all_names ~prefix ~sgn arg)
                          args
               end)
        |> rev_concat
    | Tuple args ->
        rev_concat
        @@ List.map (of_typ ~ignore_any ~all_names ~prefix ~sgn)
        @@ args
    | Unhandled -> []

  (** [of_typ ~ignore_any ~prefix ~sgn t] is a representation of [t] that
    encodes the polarity of the elements of the type : in [string -> int] [int]
    is positive and [string] negative.
    It is registered in the database and search-base type uses this to obtain
    results that fit the type asked for by the user. *)
  let of_typ ~ignore_any ~all_names t =
    of_typ ~ignore_any ~all_names ~prefix:[] ~sgn:Pos t
end


