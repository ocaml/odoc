open Compatcmdliner

let run inp =
  let inp = Fpath.v inp in
  let index = Odoc_odoc.Odoc_file.load_index inp |> Result.get_ok in
  let rec tree_to_yojson
      ({ node; children } : Odoc_index.Entry.t Odoc_utils.Tree.t) :
      Yojson.Safe.t =
    let entry =
      Odoc_json_index.Json_display.of_entry node [] |> Odoc_html.Json.to_string
    in
    `Assoc
      [
        ("node", `String entry);
        ("children", `List (List.map tree_to_yojson children));
      ]
  in
  List.iter
    (fun s ->
      s |> tree_to_yojson
      |> Format.printf "%a" (fun f -> Yojson.Safe.pretty_print f))
    index

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
