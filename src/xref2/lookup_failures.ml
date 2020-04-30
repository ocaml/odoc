let strict_mode = ref false

type 'a with_failures = 'a * string list

let failure_acc = ref []

let add f = failure_acc := f :: !failure_acc

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
let report fmt = kasprintf add fmt

(** Like [report] above but may raise the exception [exn] if strict mode is enabled *)
let report_important exn fmt =
  if !strict_mode then raise exn else kasprintf add fmt

let pp = Format.pp_print_string

let pp_failures ppf fs = List.iter (Format.fprintf ppf "%a@\n" pp) fs

let to_warning ~filename (r, failures) =
  let open Odoc_model.Error in
  accumulate_warnings (fun warnings ->
      match failures with
      | [] -> r
      | _ :: _ ->
          let failures = List.sort_uniq (String.compare) failures in
          warning warnings
            (filename_only "The following lookup failures occurred:@\n%a"
               pp_failures failures filename);
          r)
