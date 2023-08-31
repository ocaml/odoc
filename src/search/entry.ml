open Odoc_model.Lang
open Odoc_model.Paths

let list_concat_map f l =
  let rec aux f acc = function
    | [] -> List.rev acc
    | x :: l ->
        let xs = f x in
        aux f (List.rev_append xs acc) l
  in
  aux f [] l

type type_decl_entry = {
  canonical : Path.Type.t option;
  equation : TypeDecl.Equation.t;
  representation : TypeDecl.Representation.t option;
}

type class_type_entry = { virtual_ : bool; params : TypeDecl.param list }

type method_entry = { private_ : bool; virtual_ : bool; type_ : TypeExpr.t }

type class_entry = { virtual_ : bool; params : TypeDecl.param list }

type type_extension_entry = {
  type_path : Path.Type.t;
  type_params : TypeDecl.param list;
  private_ : bool;
}

type constructor_entry = {
  args : TypeDecl.Constructor.argument;
  res : TypeExpr.t;
}

type field_entry = {
  mutable_ : bool;
  type_ : TypeExpr.t;
  parent_type : TypeExpr.t;
}

type instance_variable_entry = {
  mutable_ : bool;
  virtual_ : bool;
  type_ : TypeExpr.t;
}

type doc_entry = Paragraph | Heading | CodeBlock | MathBlock | Verbatim

type value_entry = { value : Value.value; type_ : TypeExpr.t }

type kind =
  | TypeDecl of type_decl_entry
  | Module
  | Value of value_entry
  | Doc of doc_entry
  | Exception of constructor_entry
  | Class_type of class_type_entry
  | Method of method_entry
  | Class of class_entry
  | TypeExtension of type_extension_entry
  | ExtensionConstructor of constructor_entry
  | ModuleType
  | Constructor of constructor_entry
  | Field of field_entry

module Html = Tyxml.Html

type t = {
  id : Odoc_model.Paths.Identifier.Any.t;
  doc : Odoc_model.Comment.docs;
  kind : kind;
}

let entry ~id ~doc ~kind =
  let id = (id :> Odoc_model.Paths.Identifier.Any.t) in
  { id; kind; doc }

let varify_params =
  List.mapi (fun i param ->
      match param.TypeDecl.desc with
      | Var name -> TypeExpr.Var name
      | Any -> Var (Printf.sprintf "tv_%i" i))

let entry_of_constructor id_parent params (constructor : TypeDecl.Constructor.t)
    =
  let args = constructor.args in
  let res =
    match constructor.res with
    | Some res -> res
    | None ->
        let params = varify_params params in
        TypeExpr.Constr
          ( `Identifier
              ((id_parent :> Odoc_model.Paths.Identifier.Path.Type.t), false),
            params )
  in
  let kind = Constructor { args; res } in
  entry ~id:constructor.id ~doc:constructor.doc ~kind

let entry_of_extension_constructor id_parent params
    (constructor : Extension.Constructor.t) =
  let args = constructor.args in
  let res =
    match constructor.res with
    | Some res -> res
    | None ->
        let params = varify_params params in
        TypeExpr.Constr (id_parent, params)
  in
  let kind = ExtensionConstructor { args; res } in
  entry ~id:constructor.id ~doc:constructor.doc ~kind

let entry_of_field id_parent params (field : TypeDecl.Field.t) =
  let params = varify_params params in
  let parent_type =
    TypeExpr.Constr
      ( `Identifier
          ((id_parent :> Odoc_model.Paths.Identifier.Path.Type.t), false),
        params )
  in
  let kind =
    Field { mutable_ = field.mutable_; type_ = field.type_; parent_type }
  in
  entry ~id:field.id ~doc:field.doc ~kind

let rec entries_of_docs id (d : Odoc_model.Comment.docs) =
  list_concat_map (entries_of_doc id) d

and entries_of_doc id d =
  match d.value with
  | `Paragraph _ -> [ entry ~id ~doc:[ d ] ~kind:(Doc Paragraph) ]
  | `Tag _ -> []
  | `List (_, ds) ->
      list_concat_map (entries_of_docs id) (ds :> Odoc_model.Comment.docs list)
  | `Heading (_, lbl, _) -> [ entry ~id:lbl ~doc:[ d ] ~kind:(Doc Heading) ]
  | `Modules _ -> []
  | `Code_block (_, _, o) ->
      let o =
        match o with
        | None -> []
        | Some o -> entries_of_docs id (o :> Odoc_model.Comment.docs)
      in
      entry ~id ~doc:[ d ] ~kind:(Doc CodeBlock) :: o
  | `Verbatim _ -> [ entry ~id ~doc:[ d ] ~kind:(Doc Verbatim) ]
  | `Math_block _ -> [ entry ~id ~doc:[ d ] ~kind:(Doc MathBlock) ]
  | `Table _ -> []

let entries_of_item id (x : Odoc_model.Fold.item) =
  match x with
  | CompilationUnit u -> (
      match u.content with
      | Module m -> [ entry ~id:u.id ~doc:m.doc ~kind:Module ]
      | Pack _ -> [])
  | TypeDecl td ->
      let kind =
        TypeDecl
          {
            canonical = td.canonical;
            equation = td.equation;
            representation = td.representation;
          }
      in
      let td_entry = entry ~id:td.id ~doc:td.doc ~kind in
      let subtype_entries =
        match td.representation with
        | None -> []
        | Some (Variant li) ->
            List.map (entry_of_constructor td.id td.equation.params) li
        | Some (Record fields) ->
            List.map (entry_of_field td.id td.equation.params) fields
        | Some Extensible -> []
      in
      td_entry :: subtype_entries
  | Module m -> [ entry ~id:m.id ~doc:m.doc ~kind:Module ]
  | Value v ->
      let kind = Value { value = v.value; type_ = v.type_ } in
      [ entry ~id:v.id ~doc:v.doc ~kind ]
  | Exception exc ->
      let res =
        match exc.res with
        | None -> TypeExpr.Constr (Odoc_model.Predefined.exn_path, [])
        | Some x -> x
      in
      let kind = Exception { args = exc.args; res } in
      [ entry ~id:exc.id ~doc:exc.doc ~kind ]
  | ClassType ct ->
      let kind = Class_type { virtual_ = ct.virtual_; params = ct.params } in
      [ entry ~id:ct.id ~doc:ct.doc ~kind ]
  | Method m ->
      let kind =
        Method { virtual_ = m.virtual_; private_ = m.private_; type_ = m.type_ }
      in
      [ entry ~id:m.id ~doc:m.doc ~kind ]
  | Class cl ->
      let kind = Class { virtual_ = cl.virtual_; params = cl.params } in
      [ entry ~id:cl.id ~doc:cl.doc ~kind ]
  | Extension te -> (
      match te.constructors with
      | [] -> []
      | c :: _ ->
          (* Type extension do not have an ID yet... we use the first
             constructor for the url. Unfortunately, this breaks the uniqueness
             of the ID in the search index... *)
          let type_entry =
            let kind =
              TypeExtension
                {
                  type_path = te.type_path;
                  type_params = te.type_params;
                  private_ = te.private_;
                }
            in
            entry ~id:c.id ~doc:te.doc ~kind
          in

          type_entry
          :: List.map
               (entry_of_extension_constructor te.type_path te.type_params)
               te.constructors)
  | ModuleType mt -> [ entry ~id:mt.id ~doc:mt.doc ~kind:ModuleType ]
  | Doc `Stop -> []
  | Doc (`Docs d) -> entries_of_docs id d
