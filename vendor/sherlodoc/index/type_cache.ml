open Db.Typexpr
module H = Hashtbl.Make (Db.Typexpr)

type t = Db.Typexpr.t -> Db.Typexpr.t

let make () =
  let table = H.create 256 in
  fun t ->
    match H.find_opt table t with
    | Some t -> t
    | None ->
      H.add table t t ;
      t

let rec of_odoc ~cache otyp =
  match otyp with
  | Odoc_model.Lang.TypeExpr.Var _str -> Any
  | Any -> Any
  | Arrow (_lbl, left, right) -> cache (Arrow (of_odoc ~cache left, of_odoc ~cache right))
  | Constr (name, args) ->
    cache (Constr (Typename.to_string name, List.map (of_odoc ~cache) args))
  | Tuple li -> cache (Tuple (List.map (of_odoc ~cache) li))
  | _ -> Unhandled
