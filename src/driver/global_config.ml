open Sexplib0

type deps = { packages : string list; libraries : string list }

type t = { deps : deps }

let empty = { deps = { libraries = []; packages = [] } }

(* The atoms of every [(name ...)] stanza, sorted. Anything else is ignored. *)
let parse s =
  let entries = Sexplib.Sexp.of_string_many s in
  let field name =
    List.concat_map
      (function
        | Sexp.List (Atom n :: items) when n = name ->
            List.filter_map (function Sexp.Atom s -> Some s | _ -> None) items
        | _ -> [])
      entries
    |> List.sort_uniq String.compare
  in
  { deps = { libraries = field "libraries"; packages = field "packages" } }

let load config_file =
  match Bos.OS.File.read config_file with
  | Error _ ->
      Logs.err (fun m ->
          m "Failed to read odoc-config file: %a" Fpath.pp config_file);
      empty
  | Ok s -> (
      try parse s
      with e ->
        Logs.err (fun m ->
            m "Failed to parse config file %a: %s" Fpath.pp config_file
              (Printexc.to_string e));
        empty)
