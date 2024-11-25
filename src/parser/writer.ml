type partial_warning = filename:string -> Warning.t
type +'a t = Writer of ('a * partial_warning list)

let return : 'a -> 'a t = fun x -> Writer (x, [])

let bind : 'a t -> ('a -> 'b t) -> 'b t =
 fun (Writer (node, warnings)) f ->
  let (Writer (next, next_warnings)) = f node in
  Writer (next, warnings @ next_warnings)

let map : ('a -> 'b) -> 'a t -> 'b t = fun f w -> bind w (fun x -> return (f x))

let seq_right : 'a t -> 'b t -> 'b t =
 fun (Writer (_, ws)) (Writer (x, ws2)) -> Writer (x, ws @ ws2)

let seq_left : 'a t -> 'b t -> 'a t =
 fun (Writer (x, ws)) (Writer (_, ws2)) -> Writer (x, ws @ ws2)

module Prelude = struct
  let return = return
  let ( >>= ) = bind
  let ( let* ) = bind
  let ( and* ) = bind
  let ( let+ ) = map
  let ( >|= ) = map
  let ( *> ) = seq_right
  let ( <* ) = seq_left
end

let warning warning (Writer (n, ws)) = Writer (n, warning :: ws)

let sequence : 'a t list -> 'a list t =
 fun xs ->
  let rec go nodes warnings = function
    | Writer (n, ws) :: xs -> go (n :: nodes) (ws @ warnings) xs
    | [] -> Writer (nodes, warnings)
  in
  go [] [] xs

let map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
 fun f (Writer (a, ws)) (Writer (b, wsb)) -> Writer (f a b, wsb @ ws)

let traverse : ('a -> 'b t) -> 'a list -> 'b list t =
 fun f xs -> sequence (List.map f xs)

let with_warning node warning = Writer (node, [ warning ])

let ensure : ('a -> bool) -> partial_warning -> 'a t -> 'a t =
 fun pred warning (Writer (x, ws) as self) ->
  if pred x then self else Writer (x, warning :: ws)

let run : filename:string -> Ast.t t -> Ast.t * Warning.t list =
 fun ~filename (Writer (tree, warnings)) ->
  (tree, List.map (fun f -> f ~filename) warnings)
