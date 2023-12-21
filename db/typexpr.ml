type t =
  | Arrow of t * t
  | Constr of string * t list
  | Tuple of t list
  | Poly of string
  | Any
  | Unhandled

let table = Hashtbl.create 256

let cache t =
  match Hashtbl.find_opt table t with
  | Some t -> t
  | None ->
    Hashtbl.add table t t ;
    t

let arrow a b = cache (Arrow (a, b))
let constr name args = cache (Constr (name, args))
let tuple args = cache (Tuple args)
let poly name = cache (Poly name)
let any = Any
let unhandled = Unhandled

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
  | [ x ] -> show_parens x
  | x :: xs -> show_parens x ^ " * " ^ show_tuple xs

let size typ = typ |> show |> String.length
