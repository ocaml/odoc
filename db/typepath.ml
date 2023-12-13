open Types

type typ =
  | Arrow of typ * typ
  | Constr of string * typ list
  | Tuple of typ list
  | Poly of string
  | Any
  | Unhandled
[@@deriving show]

let rec show = function
  | Arrow (a, b) -> show_parens a ^ " -> " ^ show b
  | Constr (t, []) -> t
  | Constr (t, [ x ]) -> show_parens x ^ " " ^ t
  | Constr (t, xs) -> "(" ^ show_list xs ^ ") " ^ t
  | Tuple xs -> show_tuple xs
  | Poly "" -> "'_"
  | Poly name -> "'" ^ name
  | Any -> "_"
  | Unhandled -> "???"

and show_parens t =
  match t with
  | Arrow _ | Tuple _ -> "(" ^ show t ^ ")"
  | _ -> show t

and show_list = function
  | [] -> failwith "show_list: empty"
  | [ x ] -> show x
  | x :: xs -> show x ^ ", " ^ show_list xs

and show_tuple = function
  | [] -> failwith "show_tuple: empty"
  | [ x ] -> show x
  | x :: xs -> show_parens x ^ " * " ^ show_tuple xs

let regroup lst =
  Types.String_list_map.bindings
  @@ List.fold_left
       (fun acc s ->
         let count = try String_list_map.find s acc with Not_found -> 0 in
         String_list_map.add s (count + 1) acc)
       String_list_map.empty lst

type sgn =
  | Pos
  | Neg
  | Unknown

let string_of_sgn = function
  | Pos -> "+"
  | Neg -> "-"
  | Unknown -> "+"

let sgn_not = function
  | Pos -> Neg
  | Neg -> Pos
  | Unknown -> Unknown

type for_suffix_tree = string list list
type for_distance = string list list

let rev_concat lst =
  List.fold_left (fun acc xs -> List.rev_append xs acc) [] lst

let rec tails = function
  | [] -> []
  | _ :: xs as lst -> lst :: tails xs

module For_suffix_tree = struct
  type t = string list list

  let all_type_names name = tails (String.split_on_char '.' name)

  let rec of_typ ~ignore_any ~all_names ~prefix ~sgn = function
    | Poly _ -> [ "POLY" :: string_of_sgn sgn :: prefix ]
    | Any ->
        if ignore_any
        then [ prefix ]
        else [ "POLY" :: string_of_sgn sgn :: prefix ]
    | Arrow (a, b) ->
        List.rev_append
          (of_typ ~ignore_any ~all_names ~prefix ~sgn:(sgn_not sgn) a)
          (of_typ ~ignore_any ~all_names ~prefix ~sgn b)
    | Constr (name, args) ->
        name
        |> (if all_names then all_type_names else fun name -> [ [ name ] ])
        |> List.map (fun name ->
               let name = String.concat "." name in
               let prefix = name :: string_of_sgn sgn :: prefix in
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

module For_distance = struct
  type t = string list list

  let rec of_typ ~ignore_any ~prefix ~sgn t =
    match t with
    | Poly _ ->
        let poly = "POLY" in
        [ poly :: string_of_sgn sgn :: prefix ]
    | Any ->
        if ignore_any
        then [ prefix ]
        else
          let poly = "POLY" in
          [ poly :: string_of_sgn sgn :: prefix ]
    | Arrow (a, b) ->
        let prefix_left = "->0" :: prefix in
        let prefix_right = "->1" :: prefix in
        List.rev_append
          (of_typ ~ignore_any ~prefix:prefix_left ~sgn:(sgn_not sgn) a)
          (of_typ ~ignore_any ~prefix:prefix_right ~sgn b)
    | Constr (name, args) ->
        let prefix = name :: string_of_sgn sgn :: prefix in
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
