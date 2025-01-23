open Sexplib0

type deps = { packages : string list; libraries : string list }

type t = { deps : deps }

let empty = { deps = { libraries = []; packages = [] } }

module Ast = struct
  type item = Libraries of string list | Packages of string list

  type t = item list
end

let parse_string_list sexps =
  List.filter_map (function Sexp.Atom s -> Some s | _ -> None) sexps

let parse_entry (sexp : Sexp.t) =
  match sexp with
  | Atom _ -> None
  | List (Atom "libraries" :: libraries) ->
      Some (Ast.Libraries (parse_string_list libraries))
  | List (Atom "packages" :: pkgs) ->
      Some (Ast.Packages (parse_string_list pkgs))
  | _ -> None

let of_ast (ast : Ast.t) =
  let libs, pkgs =
    List.fold_left
      (fun (libs, pkgs) item ->
        match item with
        | Ast.Libraries l -> (l :: libs, pkgs)
        | Ast.Packages l -> (libs, l :: pkgs))
      ([], []) ast
  in
  let libraries, packages =
    let f x = x |> List.concat |> List.sort_uniq String.compare in
    (f libs, f pkgs)
  in
  { deps = { libraries; packages } }

let parse s =
  let entries = Sexplib.Sexp.of_string_many s in
  let ast = List.filter_map parse_entry entries in
  of_ast ast

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
