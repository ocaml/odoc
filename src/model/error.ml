open Result

let enable_missing_root_warning = ref false

type full_location_payload = Odoc_parser.Warning.t = {
  location : Location_.span;
  message : string;
}

type filename_only_payload = { file : string; message : string }

type t =
  [ `With_full_location of Odoc_parser.Warning.t
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

let _to_string =
  let pp_prefix ppf = function
    | Some p -> Format.fprintf ppf "%s: " p
    | None -> ()
  in
  fun ?prefix -> function
    | `With_full_location { location; message } ->
        if String.compare location.file "" != 0 then
          Format.asprintf "%a:@\n%a%s" Location_.pp location pp_prefix prefix
            message
        else Format.asprintf "%a%s" pp_prefix prefix message
    | `With_filename_only { file; message } ->
        Format.asprintf "File \"%s\":@\n%a%s" file pp_prefix prefix message

let to_string e = _to_string e

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

let raise_errors_and_warnings we =
  match raise_warnings we with Ok x -> x | Error e -> raise_exception e

let catch_errors_and_warnings f = catch_warnings (fun () -> catch f)

let print_error ?prefix t = prerr_endline (_to_string ?prefix t)

let print_errors = List.iter print_error

type warnings_options = { warn_error : bool; print_warnings : bool }

let print_warnings ~warnings_options warnings =
  if warnings_options.print_warnings then
    List.iter
      (fun w ->
        let prefix =
          if warnings_options.warn_error && not w.non_fatal then "Error"
          else "Warning"
        in
        print_error ~prefix w.w)
      warnings

(* When there is warnings. *)
let handle_warn_error ~warnings_options warnings ok =
  print_warnings ~warnings_options warnings;
  let maybe_fatal = List.exists (fun w -> not w.non_fatal) warnings in
  if maybe_fatal && warnings_options.warn_error then
    Error (`Msg "Warnings have been generated.")
  else Ok ok

let handle_warnings ~warnings_options ww =
  handle_warn_error ~warnings_options ww.warnings ww.value

let handle_errors_and_warnings ~warnings_options = function
  | { value = Error e; warnings } ->
      print_warnings ~warnings_options warnings;
      Error (`Msg (to_string e))
  | { value = Ok ok; warnings } ->
      handle_warn_error ~warnings_options warnings ok

let unpack_warnings ww = (ww.value, List.map (fun w -> w.w) ww.warnings)

let t_of_parser_t : Odoc_parser.Warning.t -> t =
 fun x -> (`With_full_location x :> t)

let raise_parser_warnings v =
  (* Parsing errors may be fatal. *)
  let warnings = Odoc_parser.warnings v in
  let non_fatal = false in
  raise_warnings'
    (List.map (fun p -> { w = t_of_parser_t p; non_fatal }) warnings);
  Odoc_parser.ast v
