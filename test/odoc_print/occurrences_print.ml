open Odoc_utils

module H = Hashtbl.Make (Odoc_model.Paths.Identifier)

let run inp =
  let htbl : Odoc_occurrences.Table.t = Io_utils.unmarshal inp in
  Odoc_occurrences.Table.iter
    (fun id { Odoc_occurrences.Table.direct; indirect; _ } ->
      let id =
        String.concat ~sep:"." (Odoc_model.Paths.Identifier.fullname id)
      in
      Format.printf "%s was used directly %d times and indirectly %d times\n" id
        direct indirect)
    htbl

open Cmdliner

let a_inp =
  let doc = "Input file." in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PATH" [])

let cmd =
  let doc =
    "Print the content of occurrences files into a text format. For tests"
  in
  Cmd.v (Cmd.info "occurrences_print" ~doc) @@ Term.(const run $ a_inp)

let () = exit (Cmd.eval cmd)
