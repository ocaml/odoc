type t =
  | Arrow of t * t
  | Constr of string * t list
  | Tuple of t list
  | Poly of string
  | Any
[@@deriving show]

let rec paths_arrow ~prefix ~sgn = function
  | Poly _ -> [ "POLY" :: Db.Types.string_of_sgn sgn :: prefix ]
  | Any -> [ prefix ]
  | Arrow (a, b) ->
      let prefix_left = "->0" :: prefix in
      let prefix_right = "->1" :: prefix in
      List.rev_append
        (paths_arrow ~prefix:prefix_left ~sgn:(Db.Types.sgn_not sgn) a)
        (paths_arrow ~prefix:prefix_right ~sgn b)
  | Constr (name, args) ->
      let prefix = name :: Db.Types.string_of_sgn sgn :: prefix in
      begin
        match args with
        | [] -> [ prefix ]
        | _ ->
            List.concat
            @@ List.mapi
                 (fun i arg ->
                   let prefix = string_of_int i :: prefix in
                   paths_arrow ~prefix ~sgn arg)
                 args
      end
  | Tuple args ->
      List.concat
      @@ List.mapi
           (fun i arg ->
             let prefix = (string_of_int i ^ "*") :: prefix in
             paths_arrow ~prefix ~sgn arg)
           args

let rec paths ~prefix ~sgn = function
  | Poly _ -> [ "POLY" :: Db.Types.string_of_sgn sgn :: prefix ]
  | Any -> [ prefix ]
  | Arrow (a, b) ->
      paths ~prefix ~sgn:(Db.Types.sgn_not sgn) a @ paths ~prefix ~sgn b
  | Constr (name, args) ->
      let prefix = name :: Db.Types.string_of_sgn sgn :: prefix in
      begin
        match args with
        | [] -> [ prefix ]
        | _ ->
            List.concat
            @@ List.mapi
                 (fun i arg ->
                   let prefix = string_of_int i :: prefix in
                   paths ~prefix ~sgn arg)
                 args
      end
  | Tuple args ->
      List.concat @@ List.map (fun arg -> paths ~prefix ~sgn arg) args

let rec show = function
  | Arrow (a, b) -> show_parens a ^ " -> " ^ show b
  | Constr (t, []) -> t
  | Constr (t, [ x ]) -> show_parens x ^ " " ^ t
  | Constr (t, xs) -> "(" ^ show_list xs ^ ") " ^ t
  | Tuple xs -> show_tuple xs
  | Poly "" -> "'_"
  | Poly name -> "'" ^ name
  | Any -> "_"

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
