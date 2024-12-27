(** An implementation of the Writer monad for parser error reporting *)

type +'a t = Writer of ('a * warning list)
and warning = InputNeeded of (string -> Warning.t) | Warning of Warning.t

let run_warning : input:string -> warning -> Warning.t =
 fun ~input warning ->
  match warning with InputNeeded f -> f input | Warning w -> w

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
  let ( let+ ) w f = map f w
  let ( <$> ) = map
  let ( *> ) = seq_right
  let ( <* ) = seq_left
end

let warning warning (Writer (n, ws)) = Writer (n, warning :: ws)

let sequence : 'a t list -> 'a list t =
 fun xs ->
  let go (ns, ws) (Writer (n, w)) = (n :: ns, w @ ws) in
  let xs, ws = List.fold_left go ([], []) xs in
  Writer (List.rev xs, ws)

let sequence_loc : 'a t Loc.with_location -> 'a Loc.with_location t =
 fun { value; location } -> map (Loc.at location) value

let map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
 fun f (Writer (a, ws)) (Writer (b, wsb)) -> Writer (f a b, wsb @ ws)

let traverse : ('a -> 'b t) -> 'a list -> 'b list t =
 fun f xs -> sequence (List.map f xs)

let with_warning node warning = Writer (node, [ warning ])

let ensure : ('a -> bool) -> warning -> 'a t -> 'a t =
 fun pred warning (Writer (x, ws) as self) ->
  if pred x then self else Writer (x, warning :: ws)

let run : input:string -> Ast.t t -> Ast.t * Warning.t list =
 fun ~input (Writer (tree, warnings)) ->
  (tree, List.map (run_warning ~input) warnings)

let unwrap : 'a t -> 'a = fun (Writer (x, _)) -> x
let unwrap_located : 'a Loc.with_location t -> 'a =
 fun (Writer (Loc.{ value; _ }, _)) -> value
