let strict_mode = ref false

type kind = [ `Root | `Internal ]

type 'a with_failures = 'a * (kind * string) list

let failure_acc = ref []

let add ~kind f = failure_acc := (kind, f) :: !failure_acc

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

let handle_failures ~warn_error ~filename (r, failures) =
  let open Odoc_model in
  let error msg = Error.filename_only "%s" msg filename in
  let handle_failure ~warnings = function
    | `Internal, msg -> Error.warning warnings (error msg)
    | `Root, msg -> prerr_endline (Error.to_string (error msg))
  in
  Error.accumulate_warnings (fun warnings ->
      List.iter (handle_failure ~warnings) failures;
      r)
  |> Error.handle_warnings ~warn_error
