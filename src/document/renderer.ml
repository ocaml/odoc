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

type input =
  | CU of Odoc_model.Lang.Compilation_unit.t
  | Page of Odoc_model.Lang.Page.t

type 'a t = {
  name : string;
  render : 'a -> Types.Document.t -> page list;
  extra_documents : 'a -> input -> Types.Document.t list;
}

let document_of_page ~syntax ?sidebar v =
  match syntax with
  | Reason -> Reason.page ?sidebar v
  | OCaml -> ML.page ?sidebar v

let documents_of_source_tree ~syntax v =
  match syntax with Reason -> Reason.source_tree v | OCaml -> ML.source_tree v

let documents_of_implementation ~syntax v =
  match syntax with
  | Reason -> Reason.implementation v
  | OCaml -> ML.implementation v

let document_of_compilation_unit ~syntax ?sidebar v =
  match syntax with
  | Reason -> Reason.compilation_unit ?sidebar v
  | OCaml -> ML.compilation_unit ?sidebar v
