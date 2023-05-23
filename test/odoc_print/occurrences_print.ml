module H = Hashtbl.Make (Odoc_model.Paths.Identifier)

let run inp =
  let ic = open_in_bin inp in
  let htbl = Marshal.from_channel ic in
  H.iter
    (fun id occ ->
      let id = String.concat "." (Odoc_model.Paths.Identifier.fullname id) in
      Format.printf "%s was used %d times\n" id occ)
    htbl

open Compatcmdliner

let a_inp =
  let doc = "Input file." in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PATH" [])

let term =
  let doc =
    "Print the content of occurrences files into a text format. For tests"
  in
  Term.(const run $ a_inp, info "occurrences_print" ~doc)

let () =
  match Term.eval term with
  | `Ok () -> ()
  | (`Version | `Help | `Error _) as x -> Term.exit x
