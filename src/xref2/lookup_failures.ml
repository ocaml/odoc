open Odoc_model

type context = { c_loc : Location_.span option; c_context : string list }
(** Context added by {!with_location} and {!with_context}. *)

let context_acc = ref { c_loc = None; c_context = [] }

let acc = ref []

let with_ref r x f =
  let saved = !r in
  r := x;
  let v = f () in
  let x = !r in
  r := saved;
  (v, x)

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
      | `Warning (msg, context, non_fatal) ->
          let rec pp_context fmt = function
            | hd :: tl ->
                pp_context fmt tl;
                Format.fprintf fmt "%s@\n" hd
            | [] -> ()
          in
          let pp_failure fmt () =
            Format.fprintf fmt "%a%s" pp_context context.c_context msg
          in
          let err =
            match context.c_loc with
            | Some loc -> Error.make "%a" pp_failure () loc
            | None -> Error.filename_only "%a" pp_failure () filename
          in
          Error.raise_warning ~non_fatal err)
    failures

let catch_failures ~filename f =
  let r, failures = with_ref acc [] f in
  Error.catch_warnings (fun () ->
      if !Error.enable_missing_root_warning then
        raise_root_errors ~filename failures;
      raise_warnings ~filename failures;
      r)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let report ~non_fatal fmt =
  kasprintf (fun msg -> add (`Warning (msg, !context_acc, non_fatal))) fmt

let report_internal fmt = report ~non_fatal:true fmt

let report_root ~name = add (`Root name)

let report_warning fmt = report ~non_fatal:false fmt

let with_location loc f =
  fst (with_ref context_acc { !context_acc with c_loc = Some loc } f)

let with_context fmt =
  kasprintf
    (fun msg f ->
      let c = !context_acc in
      fst (with_ref context_acc { c with c_context = msg :: c.c_context } f))
    fmt
