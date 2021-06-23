open Odoc_model

let loc_acc = ref None

let acc = ref []

let with_ref r x f =
  let saved = !r in
  r := x;
  let v = f () in
  let x = !r in
  r := saved;
  (v, x)

let with_location' loc f = fst (with_ref loc_acc (Some loc) f)

let add f = acc := f :: !acc

(** Raise a single message for root errors. *)
let raise_root_errors ~filename failures =
  let roots =
    List.fold_left
      (fun acc -> function `Root name -> name :: acc | `Warning _ -> acc)
      [] failures
    |> List.sort_uniq String.compare
  in
  match roots with
  | [] -> ()
  | _ :: _ ->
      Error.raise_warning ~non_fatal:true
        (Error.filename_only "Couldn't find the following modules:@;<1 2>@[%a@]"
           Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
           roots filename)

(** Raise the other warnings. *)
let raise_warnings ~filename failures =
  List.iter
    (function
      | `Root _ -> ()
      | `Warning (msg, loc, non_fatal) ->
          let err =
            match loc with
            | Some loc -> Error.make "%s" msg loc
            | None -> Error.filename_only "%s" msg filename
          in
          Error.raise_warning ~non_fatal err)
    failures

let catch_failures ~filename f =
  let r, failures = with_ref acc [] f in
  Error.catch_warnings (fun () ->
      raise_root_errors ~filename failures;
      raise_warnings ~filename failures;
      r)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let report ~non_fatal fmt =
  kasprintf (fun msg -> add (`Warning (msg, !loc_acc, non_fatal))) fmt

let report_internal fmt = report ~non_fatal:true fmt

let report_root ~name = add (`Root name)

let report_warning fmt = report ~non_fatal:false fmt

let with_location loc f = with_location' loc f
