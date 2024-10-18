(** Standard definition and types for all renderers *)

type syntax = OCaml | Reason

let string_of_syntax = function OCaml -> "ml" | Reason -> "re"

type page = {
  filename : Fpath.t;
  path : Url.Path.t;
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
  render : 'a -> Sidebar.t option -> Types.Document.t -> page list;
  filepath : 'a -> Url.Path.t -> Fpath.t;
}

let document_of_page ~syntax v =
  match syntax with Reason -> Reason.page v | OCaml -> ML.page v

let documents_of_implementation ~syntax v =
  match syntax with
  | Reason -> Reason.implementation v
  | OCaml -> ML.implementation v

let document_of_compilation_unit ~syntax v =
  match syntax with
  | Reason -> Reason.compilation_unit v
  | OCaml -> ML.compilation_unit v
