type location = {
  line : int;
  column : int;
}

type location_span = {
  start : location;
  end_ : location;
}

type with_location_payload = {
  file : string;
  location : location_span;
  error : string;
}

type t = [
  | `With_location of with_location_payload
]

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

exception Conveyed_by_exception of t

let get_value_or_convey_error_by_exception : ('a, t) result -> 'a = function
  | Ok v -> v
  | Error e -> raise (Conveyed_by_exception e)
