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
  extra_documents : 'a -> input -> syntax:syntax -> Types.Document.t list;
}

let document_of_page ~syntax v =
  match syntax with Reason -> Reason.page v | OCaml -> ML.page v

let documents_of_source_tree ~syntax v =
  match syntax with Reason -> Reason.source_tree v | OCaml -> ML.source_tree v

let document_of_compilation_unit ~syntax v =
  match syntax with
  | Reason -> Reason.compilation_unit v
  | OCaml -> ML.compilation_unit v

let document_of_source ~syntax =
  match syntax with
  | Reason -> Reason.source_page (* Currently, both functions are equivalent *)
  | OCaml -> ML.source_page
