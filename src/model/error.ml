type full_location_payload = {
  file : string;
  location : Location_.span;
  error : string;
}

type filename_only_payload = {
  file : string;
  error : string;
}

type t = [
  | `With_location of full_location_payload
  | `With_filename_only of filename_only_payload
]

type 'a with_warnings = {
  result : 'a;
  warnings : t list;
}

let to_string : t -> string = function
  | `With_location {file; location; error} ->
    let location =
      if location.start.line = location.end_.line then
        Printf.sprintf "line %i, characters %i-%i"
          location.start.line
          location.start.column
          location.end_.column
      else
        Printf.sprintf "line %i, character %i to line %i, character %i"
          location.start.line
          location.start.column
          location.end_.line
          location.end_.column
    in
    Printf.sprintf "File \"%s\", %s:\n%s" file location error

  | `With_filename_only {file; error} ->
    Printf.sprintf "File \"%s\":\n%s" file error

exception Conveyed_by_exception of t

let convey_by_exception : ('a, t) result -> 'a = function
  | Ok v -> v
  | Error e -> raise (Conveyed_by_exception e)

let catch_conveyed_by_exception : (unit -> 'a) -> ('a, t) result = fun f ->
  try Ok (f ())
  with Conveyed_by_exception e -> Error e

(* TODO This is a temporary measure until odoc is ported to handle warnings
   throughout. *)
let shed_warnings : 'a with_warnings -> 'a = fun with_warnings ->
  with_warnings.warnings
  |> List.iter (fun warning -> warning |> to_string |> prerr_endline);
  with_warnings.result
