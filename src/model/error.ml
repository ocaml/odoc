open Result

type full_location_payload = Odoc_parser.Error.t = {
  location : Location_.span;
  message : string;
}

type filename_only_payload = { file : string; message : string }

type t =
  [ `With_full_location of Odoc_parser.Error.t
  | `With_filename_only of filename_only_payload ]

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let kmake k ?suggestion format =
  format
  |> kasprintf (fun message ->
         match suggestion with
         | None -> k message
         | Some suggestion -> k (message ^ "\nSuggestion: " ^ suggestion))

let make ?suggestion format =
  let k message location = `With_full_location { location; message } in
  kmake k ?suggestion format

let filename_only ?suggestion format =
  let k message file = `With_filename_only { file; message } in
  kmake k ?suggestion format

let to_string = function
  | `With_full_location { location; message } ->
      let location_string =
        if location.start.line = location.end_.line then
          Printf.sprintf "line %i, characters %i-%i" location.start.line
            location.start.column location.end_.column
        else
          Printf.sprintf "line %i, character %i to line %i, character %i"
            location.start.line location.start.column location.end_.line
            location.end_.column
      in
      Printf.sprintf "File \"%s\", %s:\n%s" location.file location_string
        message
  | `With_filename_only { file; message } ->
      Printf.sprintf "File \"%s\":\n%s" file message

exception Conveyed_by_exception of t

let raise_exception error = raise (Conveyed_by_exception error)

let catch f = try Ok (f ()) with Conveyed_by_exception error -> Error error

type warning = {
  w : t;
  non_fatal : bool;
      (** If [true], the warning won't be made fatal in [warn_error] mode. *)
}

type 'a with_warnings = { value : 'a; warnings : warning list }

let with_ref r f =
  let saved = !r in
  try
    let v = f () in
    r := saved;
    v
  with e ->
    r := saved;
    raise e

let raised_warnings = ref []

let raise_warnings' warnings =
  raised_warnings := List.rev_append warnings !raised_warnings

let raise_warning ?(non_fatal = false) w =
  raised_warnings := { w; non_fatal } :: !raised_warnings

let raise_warnings with_warnings =
  raise_warnings' with_warnings.warnings;
  with_warnings.value

let catch_warnings f =
  with_ref raised_warnings (fun () ->
      raised_warnings := [];
      let value = f () in
      let warnings = List.rev !raised_warnings in
      { value; warnings })

type 'a with_errors_and_warnings = ('a, t) Result.result with_warnings

let catch_errors_and_warnings f = catch_warnings (fun () -> catch f)

let print_warnings = List.iter (fun w -> prerr_endline (to_string w.w))

(* When there is warnings. *)
let handle_warn_error ~warn_error warnings ok =
  print_warnings warnings;
  let maybe_fatal = List.exists (fun w -> not w.non_fatal) warnings in
  if maybe_fatal && warn_error then Error (`Msg "Warnings have been generated.")
  else Ok ok

let handle_warnings ~warn_error ww =
  handle_warn_error ~warn_error ww.warnings ww.value

let handle_errors_and_warnings ~warn_error = function
  | { value = Error e; warnings } ->
      print_warnings warnings;
      Error (`Msg (to_string e))
  | { value = Ok ok; warnings } -> handle_warn_error ~warn_error warnings ok

let unpack_warnings ww = (ww.value, List.map (fun w -> w.w) ww.warnings)

let t_of_parser_t : Odoc_parser.Error.t -> t =
 fun x -> (`With_full_location x :> t)

let raise_parser_warnings { Odoc_parser.Error.value; warnings } =
  (* Parsing errors may be fatal. *)
  let non_fatal = false in
  raise_warnings'
    (List.map (fun p -> { w = t_of_parser_t p; non_fatal }) warnings);
  value
