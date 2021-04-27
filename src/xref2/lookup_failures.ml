open Odoc_model

type kind = [ `Root | `Internal | `Warning ]

let loc_acc = ref None

let with_location' loc f =
  let prev_loc = !loc_acc in
  loc_acc := Some loc;
  let r = f () in
  loc_acc := prev_loc;
  r

let add ~kind msg =
  let w =
    match !loc_acc with
    | Some (`Filename_only filename) -> Error.filename_only "%s" msg filename
    | Some (`Full_loc loc) -> Error.make "%s" msg loc
    | None -> failwith "Lookup_failures: Uncaught failure."
  in
  let non_fatal =
    match kind with `Internal | `Warning -> false | `Root -> true
  in
  Error.raise_warning ~non_fatal w

let catch_failures ~filename f =
  with_location' (`Filename_only filename) (fun () -> Error.catch_warnings f)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

(** Report a lookup failure to the enclosing [catch_failures] call. *)
let report ?(kind = `Internal) fmt =
  (* Render the message into a string first because [Error.kmake] is not
     exposed. *)
  kasprintf (add ~kind) fmt

let with_location loc f = with_location' (`Full_loc loc) f
