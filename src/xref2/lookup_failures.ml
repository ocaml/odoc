let strict_mode = ref false

type kind = [ `Root | `Internal | `Warning ]

type loc = Odoc_model.Location_.span option

type 'a with_failures = 'a * (kind * string * loc) list

let failure_acc = ref []

let loc_acc = ref None

let add ~kind f = failure_acc := (kind, f, !loc_acc) :: !failure_acc

let catch_failures f =
  let prev = !failure_acc in
  failure_acc := [];
  let r = f () in
  let failures = !failure_acc in
  failure_acc := prev;
  (r, List.rev failures)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

(** Report a lookup failure to the enclosing [catch_failures] call. *)
let report ?(kind = `Internal) fmt = kasprintf (add ~kind) fmt

(** Like [report] above but may raise the exception [exn] if strict mode is enabled *)
let report_important ?(kind = `Internal) exn fmt =
  if !strict_mode then raise exn else kasprintf (add ~kind) fmt

let with_location loc f =
  let prev_loc = !loc_acc in
  loc_acc := Some loc;
  let r = f () in
  loc_acc := prev_loc;
  r

let handle_failures ~warn_error ~filename (r, failures) =
  let open Odoc_model in
  let error ~loc msg =
    match loc with
    | Some loc -> Error.make "%s" msg loc
    | None -> Error.filename_only "%s" msg filename
  in
  let handle_failure ~warnings = function
    | `Internal, msg, loc -> Error.warning warnings (error ~loc msg)
    | `Warning, msg, loc -> Error.warning warnings (error ~loc msg)
    | `Root, msg, loc -> prerr_endline (Error.to_string (error ~loc msg))
  in
  Error.accumulate_warnings (fun warnings ->
      List.iter (handle_failure ~warnings) failures;
      r)
  |> Error.handle_warnings ~warn_error
