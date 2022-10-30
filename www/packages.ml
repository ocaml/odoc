type package =
  { category : string
  ; name : string
  ; description : string
  }

module M = Map.Make (String)

module S = Set.Make (struct
  type t = package

  let compare a b =
    String.compare
      (String.lowercase_ascii a.name)
      (String.lowercase_ascii b.name)
end)

let pretty = function
  | "ai" -> "Sciences"
  | "app" -> "Applications"
  | "ascii" -> "Formats: Restricted alphabet"
  | "audio" -> "Multimedia: Audio"
  | "bap" -> "PLT: Binary Analysis Platform"
  | "bench" -> "Benchmarking"
  | "bindings" -> "Various bindings"
  | "bio" -> "Sciences"
  | "build" -> "Tooling: Build systems"
  | "chemistry" -> "Sciences"
  | "cloud" -> "Cloud"
  | "color" -> "Multimedia: Images"
  | "compression" -> "Formats: Compression"
  | "conf" -> "conf"
  | "cordova" -> "Javascript: Cordova"
  | "crypto" -> "Cryptography"
  | "data" -> "Datastructures"
  | "databases" -> "Databases"
  | "dns" -> "Network: DNS"
  | "document" -> "Formats: Text document"
  | "documentation" -> "Tooling: Documentation"
  | "email" -> "Network: Email"
  | "format" -> "Formats"
  | "frp" -> "Reactive programming"
  | "graphics2d" -> "Graphics: 2D"
  | "graphics3d" -> "Graphics: 3D"
  | "gui" -> "GUI"
  | "hardcaml" -> "Hardcaml"
  | "http" -> "Network: HTTP client"
  | "ide" -> "Tooling: IDE"
  | "images" -> "Multimedia: Images"
  | "interoperability" -> "Interoperability"
  | "io" -> "I/O"
  | "irc" -> "Network: IRC"
  | "irmin" -> "Databases: Irmin"
  | "javascript" -> "Javascript"
  | "js" -> "Javascript"
  | "json" -> "Formats: Json"
  | "language" -> "Programming languages"
  | "linux" -> "System: Linux"
  | "log" -> "Logs"
  | "macos" -> "System: Mac Os"
  | "maths" -> "Maths"
  | "mirage" -> "Mirage"
  | "monads" -> "Stdlib monadic"
  | "monitor" -> "Monitoring"
  | "multimedia" -> "Multimedia"
  | "network" -> "Network"
  | "notebook" -> "Tooling: Toplevel / Notebook"
  | "opam" -> "Tooling: Opam / Packaging"
  | "packaging" -> "Tooling: Opam / Packaging"
  | "parser" -> "Parsers"
  | "plt" -> "PLT"
  | "ppx" -> "PPX"
  | "prover" -> "Theorem provers"
  | "retrocompatibility" -> "Stdlib retrocompatibility"
  | "science" -> "Sciences"
  | "security" -> "Cryptography"
  | "sexp" -> "Formats: Sexp"
  | "solver" -> "Constraint solvers"
  | "ssh" -> "Network: SSH"
  | "stdlib" -> "Stdlib extended"
  | "system" -> "System"
  | "terminal" -> "Terminal"
  | "tests" -> "Testing"
  | "text" -> "Text"
  | "tezos" -> "Tezos"
  | "time" -> "Date and Time"
  | "tooling" -> "Tooling"
  | "unix" -> "System: Unix"
  | "utils" -> "Stdlib complements"
  | "variants" -> "OCaml variants"
  | "video" -> "Multimedia: Video"
  | "web" -> "Web server"
  | "windows" -> "System: Windows"
  | "xen" -> "Xen"
  | "xml" -> "Formats: Xml"
  | "" -> "--- TODO ---"
  | other ->
      Format.printf "TODO: missing category name %S@." other ;
      other

let unescape str =
  let str = String.trim str in
  let buf = Buffer.create (String.length str) in
  for i = 0 to String.length str - 1 do
    let chr = str.[i] in
    if not (chr = '\'' || chr = '"') then Buffer.add_char buf chr
  done ;
  Buffer.contents buf

let load filename =
  let h = open_in filename in
  let rec go acc =
    match input_line h with
    | exception End_of_file -> acc
    | line ->
        let package =
          match String.split_on_char '\t' line with
          | [ category; name; description ] ->
              { category = pretty category
              ; name
              ; description = unescape description
              }
          | [ name; description ] ->
              { category = pretty ""; name; description = unescape description }
          | _ -> failwith (Printf.sprintf "invalid package: %S" line)
        in
        let set = try M.find package.category acc with Not_found -> S.empty in
        let set = S.add package set in
        let acc = M.add package.category set acc in
        go acc
  in
  let result = go M.empty in
  close_in h ;
  result

let packages =
  List.fold_left
    (fun acc p -> M.remove p acc)
    (load "./static/packages.csv")
    [ "Tezos"; "conf" ]

open Tyxml.Html

let html =
  div
    ~a:[ a_class [ "categories" ] ]
    (M.bindings packages
    |> List.map (fun (category, packages) ->
           div
             ~a:[ a_class [ "category" ] ]
             [ h3 [ txt (if category = "" then "Not classified" else category) ]
             ; div
                 ~a:[ a_class [ "packages" ] ]
                 (S.elements packages
                 |> List.map (fun package ->
                        a
                          ~a:
                            [ a_href ("https://ocaml.org/p/" ^ package.name)
                            ; a_title package.description
                            ]
                          [ txt package.name ]))
             ]))
