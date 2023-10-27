let json_of_args (args : Odoc_model.Lang.TypeDecl.Constructor.argument) =
  match args with
  | Tuple tel ->
      `Object
        [
          ("kind", `String "Tuple");
          ( "vals",
            `Array (List.map (fun te -> `String (Render.text_of_type te)) tel)
          );
        ]
  | Record fl ->
      `Object
        [
          ("kind", `String "Record");
          ( "fields",
            `Array
              (List.map
                 (fun {
                        Odoc_model.Lang.TypeDecl.Field.id;
                        mutable_;
                        type_;
                        doc = _;
                      } ->
                   `Object
                     [
                       ("name", `String (Odoc_model.Paths.Identifier.name id));
                       ("mutable", `Bool mutable_);
                       ("type", `String (Render.text_of_type type_));
                     ])
                 fl) );
        ]

let rec of_id x =
  let open Odoc_model.Names in
  let open Odoc_model.Paths.Identifier in
  let ret kind name =
    `Object [ ("kind", `String kind); ("name", `String name) ]
  in
  match x.iv with
  | `Root (_, name) -> [ ret "Root" (ModuleName.to_string name) ]
  | `Page (_, name) -> [ ret "Page" (PageName.to_string name) ]
  | `LeafPage (_, name) -> [ ret "Page" (PageName.to_string name) ]
  | `Module (parent, name) ->
      ret "Module" (ModuleName.to_string name) :: of_id (parent :> t)
  | `Parameter (parent, name) ->
      ret "Parameter" (ModuleName.to_string name) :: of_id (parent :> t)
  | `Result x -> of_id (x :> t)
  | `ModuleType (parent, name) ->
      ret "ModuleType" (ModuleTypeName.to_string name) :: of_id (parent :> t)
  | `Type (parent, name) ->
      ret "Type" (TypeName.to_string name) :: of_id (parent :> t)
  | `CoreType name -> [ ret "CoreType" (TypeName.to_string name) ]
  | `Constructor (parent, name) ->
      ret "Constructor" (ConstructorName.to_string name) :: of_id (parent :> t)
  | `Field (parent, name) ->
      ret "Field" (FieldName.to_string name) :: of_id (parent :> t)
  | `Extension (parent, name) ->
      ret "Extension" (ExtensionName.to_string name) :: of_id (parent :> t)
  | `Exception (parent, name) ->
      ret "Exception" (ExceptionName.to_string name) :: of_id (parent :> t)
  | `CoreException name ->
      [ ret "CoreException" (ExceptionName.to_string name) ]
  | `Value (parent, name) ->
      ret "Value" (ValueName.to_string name) :: of_id (parent :> t)
  | `Class (parent, name) ->
      ret "Class" (ClassName.to_string name) :: of_id (parent :> t)
  | `ClassType (parent, name) ->
      ret "ClassType" (ClassTypeName.to_string name) :: of_id (parent :> t)
  | `Method (parent, name) ->
      ret "Method" (MethodName.to_string name) :: of_id (parent :> t)
  | `InstanceVariable (parent, name) ->
      ret "InstanceVariable" (InstanceVariableName.to_string name)
      :: of_id (parent :> t)
  | `Label (parent, name) ->
      ret "Label" (LabelName.to_string name) :: of_id (parent :> t)
  | `SourceDir _ | `SourceLocationMod _ | `SourceLocation _ | `SourcePage _
  | `SourceLocationInternal _ | `AssetFile _ ->
      [ `Null ]
(* TODO *)

let of_id n = `Array (List.rev @@ of_id (n :> Odoc_model.Paths.Identifier.t))

let of_doc (doc : Odoc_model.Comment.docs) =
  let txt = Render.text_of_doc doc in
  `String txt

let of_entry
    ({ entry = { id; doc; kind }; html = _ } as entry : Entry.with_html) :
    Odoc_html.Json.json =
  let j_id = of_id id in
  let doc = of_doc doc in
  let display = Json_display.of_entry entry in
  let kind =
    let return kind arr = `Object (("kind", `String kind) :: arr) in
    match kind with
    | TypeDecl { canonical = _; equation; representation = _; txt = _ } ->
        let {
          Odoc_model.Lang.TypeDecl.Equation.params = _;
          private_;
          manifest;
          constraints;
        } =
          equation
        in
        let private_ = `Bool private_ in
        let manifest =
          match manifest with
          | None -> `Null
          | Some te -> `String (Render.text_of_type te)
        in
        let constraints =
          `Array
            (List.map
               (fun (lhs, rhs) ->
                 `Object
                   [
                     ("lhs", `String (Render.text_of_type lhs));
                     ("rhs", `String (Render.text_of_type rhs));
                   ])
               constraints)
        in
        return "TypeDecl"
          [
            ("private", private_);
            ("manifest", manifest);
            ("constraints", constraints);
          ]
    | Module -> return "Module" []
    | Value { value = _; type_ } ->
        return "Value" [ ("type", `String (Render.text_of_type type_)) ]
    | Doc Paragraph -> return "Doc" [ ("subkind", `String "Paragraph") ]
    | Doc Heading -> return "Doc" [ ("subkind", `String "Heading") ]
    | Doc CodeBlock -> return "Doc" [ ("subkind", `String "CodeBlock") ]
    | Doc MathBlock -> return "Doc" [ ("subkind", `String "MathBlock") ]
    | Doc Verbatim -> return "Doc" [ ("subkind", `String "Verbatim") ]
    | Exception { args; res } ->
        let args = json_of_args args in
        let res = `String (Render.text_of_type res) in
        return "Exception" [ ("args", args); ("res", res) ]
    | Class_type { virtual_; params = _ } ->
        return "ClassType" [ ("virtual", `Bool virtual_) ]
    | Method { private_; virtual_; type_ } ->
        return "Method"
          [
            ("virtual", `Bool virtual_);
            ("private", `Bool private_);
            ("type", `String (Render.text_of_type type_));
          ]
    | Class { virtual_; params = _ } ->
        return "Class" [ ("virtual", `Bool virtual_) ]
    | TypeExtension { type_path = _; type_params = _; private_ } ->
        (* TODO: include type_path and type_params *)
        return "TypeExtension" [ ("private", `Bool private_) ]
    | ExtensionConstructor { args; res } ->
        let args = json_of_args args in
        let res = `String (Render.text_of_type res) in
        return "ExtensionConstructor" [ ("args", args); ("res", res) ]
    | ModuleType -> return "ModuleType" []
    | Constructor { args; res } ->
        let args = json_of_args args in
        let res = `String (Render.text_of_type res) in
        return "Constructor" [ ("args", args); ("res", res) ]
    | Field { mutable_; type_; parent_type } ->
        return "Field"
          [
            ("mutable", `Bool mutable_);
            ("type", `String (Render.text_of_type type_));
            ("parent_type", `String (Render.text_of_type parent_type));
          ]
  in
  `Object [ ("id", j_id); ("doc", doc); ("kind", kind); ("display", display) ]

let output_json ppf first entries =
  let output_json json =
    let str = Odoc_html.Json.to_string json in
    Format.fprintf ppf "%s\n" str
  in
  List.fold_left
    (fun first entry ->
      let json = of_entry entry in
      if not first then Format.fprintf ppf ",";
      output_json json;
      false)
    first entries

let unit ppf u =
  let f (first, id) i =
    let entries = Entry.entries_of_item id i in
    let entries = List.map Generator.with_html entries in
    let id =
      match i with
      | CompilationUnit u -> (u.id :> Odoc_model.Paths.Identifier.t)
      | TypeDecl _ -> id
      | Module m -> (m.id :> Odoc_model.Paths.Identifier.t)
      | Value _ -> id
      | Exception _ -> id
      | ClassType ct -> (ct.id :> Odoc_model.Paths.Identifier.t)
      | Method _ -> id
      | Class c -> (c.id :> Odoc_model.Paths.Identifier.t)
      | Extension _ -> id
      | ModuleType mt -> (mt.id :> Odoc_model.Paths.Identifier.t)
      | Doc _ -> id
    in
    let first = output_json ppf first entries in
    (first, id)
  in
  let _first =
    Odoc_model.Fold.unit ~f
      ( true,
        (u.Odoc_model.Lang.Compilation_unit.id :> Odoc_model.Paths.Identifier.t)
      )
      u
  in
  ()

let page ppf (page : Odoc_model.Lang.Page.t) =
  let f first i =
    let entries =
      Entry.entries_of_item (page.name :> Odoc_model.Paths.Identifier.t) i
    in
    let entries = List.map Generator.with_html entries in
    output_json ppf first entries
  in
  let _first = Odoc_model.Fold.page ~f true page in
  ()
