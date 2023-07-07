open Odoc_model.Lang
open Odoc_model.Paths

type type_decl_entry = {
  txt : string;
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

type extra =
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
  extra : extra;
  html : Html_types.div Html.elt;
}

let entry ~id ~doc ~extra ~html =
  let id = (id :> Odoc_model.Paths.Identifier.Any.t) in
  { id; extra; doc; html }

let varify_params =
  List.mapi (fun i param ->
      match param.TypeDecl.desc with
      | Var name -> TypeExpr.Var name
      | Any -> Var (Printf.sprintf "tv_%i" i))

let entry_of_constructor id_parent params (constructor : TypeDecl.Constructor.t)
    =
  let html =
    Tyxml.Html.div ~a:[]
      [
        Tyxml.Html.txt
        @@ Generator.constructor
             (constructor.id :> Identifier.t)
             constructor.args constructor.res;
      ]
  in
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
  let extra = Constructor { args; res } in
  entry ~id:constructor.id ~doc:constructor.doc ~extra ~html

let entry_of_extension_constructor id_parent params
    (constructor : Extension.Constructor.t) =
  let html =
    Tyxml.Html.div ~a:[]
      [
        Tyxml.Html.txt
        @@ Generator.constructor
             (constructor.id :> Identifier.t)
             constructor.args constructor.res;
      ]
  in
  let args = constructor.args in
  let res =
    match constructor.res with
    | Some res -> res
    | None ->
        let params = varify_params params in
        TypeExpr.Constr (id_parent, params)
  in
  let extra = ExtensionConstructor { args; res } in
  entry ~id:constructor.id ~doc:constructor.doc ~extra ~html

let entry_of_field id_parent params (field : TypeDecl.Field.t) =
  let params = varify_params params in
  let parent_type =
    TypeExpr.Constr
      ( `Identifier
          ((id_parent :> Odoc_model.Paths.Identifier.Path.Type.t), false),
        params )
  in
  let extra =
    Field { mutable_ = field.mutable_; type_ = field.type_; parent_type }
  in
  let html = Html.div ~a:[] [] in
  entry ~id:field.id ~doc:field.doc ~extra ~html

let rec entries_of_docs id (d : Odoc_model.Comment.docs) =
  List.concat_map (entries_of_doc id) d

and entries_of_doc id d =
  let html = Html.div ~a:[] [] in
  match d.value with
  | `Paragraph _ -> [ entry ~id ~doc:[ d ] ~extra:(Doc Paragraph) ~html ]
  | `Tag _ -> []
  | `List (_, ds) ->
      List.concat_map (entries_of_docs id) (ds :> Odoc_model.Comment.docs list)
  | `Heading (_, lbl, _) ->
      [ entry ~id:lbl ~doc:[ d ] ~extra:(Doc Heading) ~html ]
  | `Modules _ -> []
  | `Code_block (_, _, o) ->
      let o =
        match o with
        | None -> []
        | Some o -> entries_of_docs id (o :> Odoc_model.Comment.docs)
      in
      entry ~id ~doc:[ d ] ~extra:(Doc CodeBlock) ~html :: o
  | `Verbatim _ -> [ entry ~id ~doc:[ d ] ~extra:(Doc Verbatim) ~html ]
  | `Math_block _ -> [ entry ~id ~doc:[ d ] ~extra:(Doc MathBlock) ~html ]
  | `Table _ -> []

let entries_of_item id (x : Odoc_model.Fold.item) =
  let html = Generator.html_of_entry x in
  match x with
  | CompilationUnit u -> (
      match u.content with
      | Module m -> [ entry ~id:u.id ~doc:m.doc ~extra:Module ~html ]
      | Pack _ -> [])
  | TypeDecl td ->
      let txt = Render.text_of_typedecl td in
      let extra =
        TypeDecl
          {
            txt;
            canonical = td.canonical;
            equation = td.equation;
            representation = td.representation;
          }
      in
      let td_entry = entry ~id:td.id ~doc:td.doc ~extra ~html in
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
  | Module m -> [ entry ~id:m.id ~doc:m.doc ~extra:Module ~html ]
  | Value v ->
      let extra = Value { value = v.value; type_ = v.type_ } in
      [ entry ~id:v.id ~doc:v.doc ~extra ~html ]
  | Exception exc ->
      let res =
        Option.value exc.res
          ~default:(TypeExpr.Constr (Odoc_model.Predefined.exn_path, []))
      in
      let extra = Exception { args = exc.args; res } in
      [ entry ~id:exc.id ~doc:exc.doc ~extra ~html ]
  | ClassType ct ->
      let extra = Class_type { virtual_ = ct.virtual_; params = ct.params } in
      [ entry ~id:ct.id ~doc:ct.doc ~extra ~html ]
  | Method m ->
      let extra =
        Method { virtual_ = m.virtual_; private_ = m.private_; type_ = m.type_ }
      in
      [ entry ~id:m.id ~doc:m.doc ~extra ~html ]
  | Class cl ->
      let extra = Class { virtual_ = cl.virtual_; params = cl.params } in
      [ entry ~id:cl.id ~doc:cl.doc ~extra ~html ]
  | Extension te -> (
      match te.constructors with
      | [] -> []
      | c :: _ ->
          (* Type extension do not have an ID yet... we use the first
             constructor for the url. Unfortunately, this breaks the uniqueness
             of the ID in the search index... *)
          let type_entry =
            let extra =
              TypeExtension
                {
                  type_path = te.type_path;
                  type_params = te.type_params;
                  private_ = te.private_;
                }
            in
            entry ~id:c.id ~doc:te.doc ~extra ~html
          in

          type_entry
          :: List.map
               (entry_of_extension_constructor te.type_path te.type_params)
               te.constructors)
  | ModuleType mt -> [ entry ~id:mt.id ~doc:mt.doc ~extra:ModuleType ~html ]
  | Doc `Stop -> []
  | Doc (`Docs d) -> entries_of_docs id d
