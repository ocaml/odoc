type child = Page of string | Dir of string

type line =
  | Children_order of child Location_.with_location list
  | KV of string * string
  | V of string

type children_order = child Location_.with_location list Location_.with_location

type t = { children_order : children_order option }

let empty = { children_order = None }

let apply fm line =
  match (line.Location_.value, fm) with
  | Children_order children_order, { children_order = None } ->
      { children_order = Some (Location_.same line children_order) }
  | Children_order _, { children_order = Some _ } ->
      (* TODO raise warning about duplicate children field *) fm
  | KV _, _ | V _, _ -> (* TODO raise warning *) fm

let parse_child c =
  if Astring.String.is_suffix ~affix:"/" c then
    let c = String.sub c 0 (String.length c - 1) in
    Dir c
  else Page c

let parse s =
  let entries =
    s.Location_.value
    |> Astring.String.cuts ~sep:"\n"
    |> List.map (fun l ->
           let v =
             Astring.String.cut ~sep:":" l |> function
             | Some ("children", v) ->
                 let refs =
                   v
                   |> Astring.String.fields ~empty:false
                   |> List.map parse_child
                   |> List.map (Location_.same s)
                 in
                 Children_order refs
             | Some (k, v) -> KV (k, v)
             | None -> V l
           in
           Location_.same s v)
  in
  List.fold_left apply empty entries

module Sexp_pattern = struct
  open Sexplib
  open Sexp.Annotated

  let point ({ line; col; offset = _ } : pos) : Location_.point =
    { line; column = col }

  let span { start_pos; end_pos } =
    Location_.{ file = "TODO"; start = point start_pos; end_ = point end_pos }

  let span_of_sexp sexp =
    match sexp with
    | List (range, _, _) -> span range
    | Atom (range, _) -> span range

  let ( let* ) = Result.bind

  let str expected f Location_.{ value = real; location } =
    if expected = real then f ()
    else Error (Error.make "Expected %S got %S" expected real location)

  (* let ( ||| ) pat1 pat2 sexp =
     match pat1 sexp with Error _msg -> pat2 sexp | Ok v -> Ok v *)

  let atom f sexp =
    match sexp with
    | Atom (range, Sexp.Atom str) ->
        let span = span range in
        f (Location_.at span str)
    | _ -> Error (Error.make "Expected list" (span_of_sexp sexp))

  let list f sexp =
    match sexp with
    | List (range, li, _) ->
        let span = span range in
        f (Location_.at span li)
    | _ -> Error (Error.make "Expected list" (span_of_sexp sexp))

  let rec result_list_map f li =
    match li with
    | [] -> Ok []
    | elt :: li ->
        let* elt = f elt in
        let* li = result_list_map f li in
        Ok (elt :: li)

  let accept = Result.ok

  let accept_map f a = Result.ok (f a)
end

(*
(children (a b c d))
*)

let of_sexp_opt sexp =
  match sexp with
  | None -> Ok empty
  | Some sexp ->
      let open Sexp_pattern in
      list
        (function
          | { value = [ sexp1; sexp2 ]; location } ->
              let* () = atom (str "children" accept) sexp1 in
              let* children =
                list
                  (fun { value = li; location = _ } ->
                    result_list_map
                      (atom
                         (accept_map (fun Location_.{ value = str; location } ->
                              Location_.at location
                                (if str.[String.length str - 1] = '/' then
                                   Dir
                                     (String.sub str 0 (String.length str - 1))
                                 else Page str))))
                      li)
                  sexp2
              in
              Ok { children_order = Some (Location_.at location children) }
          | _ ->
              Error
                (Error.make "Expected two elements in list" (span_of_sexp sexp)))
        sexp
