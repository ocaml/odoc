type t =
  | Arrow of t * t
  | Constr of string * t list
  | Tuple of t list
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

  let size typ = typ |> show  |> String.length