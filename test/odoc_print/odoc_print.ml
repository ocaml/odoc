(** Print .odocl files. *)

open Odoc_odoc
open Odoc_model_desc
open Or_error

let print_json_desc desc x =
  let yojson = Type_desc_to_yojson.to_yojson desc x in
  Yojson.Basic.pretty_print Format.std_formatter yojson

let run inp =
  let inp = Fpath.v inp in
  Page.load inp >>= fun page ->
  print_json_desc Lang_desc.page_t page;
  Compilation_unit.load inp >>= fun u ->
  print_json_desc Lang_desc.compilation_unit_t u;
  Ok ()

open Cmdliner

let a_inp =
  let doc = "Input file." in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PATH" [])

let term =
  let doc = "Print the content of .odoc files into a text format. For tests" in
  Term.(const run $ a_inp, info "odoc_print" ~doc)

let () =
  match Term.eval term with
  | `Ok (Ok ()) -> ()
  | `Ok (Error (`Msg msg)) ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | cmdliner_error -> Term.exit cmdliner_error
