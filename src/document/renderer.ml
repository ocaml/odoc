(** Standard definition and types for all renderers *)

type syntax = OCaml | Reason

let string_of_syntax = function OCaml -> "ml" | Reason -> "re"

type page = {
  filename : Fpath.t;
  content : Format.formatter -> unit;
  children : page list;
}

let traverse ~f t =
  let rec aux node =
    f node.filename node.content;
    List.iter aux node.children
  in
  List.iter aux t

type 'a t = { name : string; render : 'a -> Types.Page.t -> page list }

let document_of_page ~syntax v =
  match syntax with Reason -> Reason.page v | OCaml -> ML.page v

let document_of_compilation_unit ~syntax v =
  match syntax with
  | Reason -> Reason.compilation_unit v
  | OCaml -> ML.compilation_unit v

let document_of_source v =
  let open Types in
  let title = Fpath.to_string v in
  let preamble = [] in
  let url = { Url.Path.kind = `File; parent = None; name = title } in
  let items =
    match Fs.File.read v with
    | Ok content ->
        let lines = String.split_on_char '\n' content in
        let rec loop i acc = function
          | [] -> acc
          | h :: t ->
              let source = [ Source.Elt [ { attr = []; desc = Text h } ] ] in
              let anchor =
                Some
                  Url.Anchor.
                    {
                      page = url;
                      anchor = Format.sprintf "L%i" i;
                      kind = `File;
                    }
              in
              let item =
                Item.Declaration
                  { attr = []; anchor; content = [ Code source ]; doc = [] }
              in
              loop (i + 1) (item :: acc) t
        in
        List.rev (loop 1 [] lines)
    | Error _ -> []
  in
  { Page.preamble; items; url }
