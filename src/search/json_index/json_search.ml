open Odoc_search
open Odoc_index

let json_of_args (args : Odoc_model.Lang.TypeDecl.Constructor.argument) =
  match args with
  | Tuple tel ->
      `Object
        [
          ("kind", `String "Tuple");
          ("vals", `Array (List.map (fun te -> `String (Text.of_type te)) tel));
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
                       ("type", `String (Text.of_type type_));
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
  | `AssetFile (_, name) -> [ ret "Asset" (AssetName.to_string name) ]
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
  | `Constructor (parent, name) ->
      ret "Constructor" (ConstructorName.to_string name) :: of_id (parent :> t)
  | `Field (parent, name) ->
      ret "Field" (FieldName.to_string name) :: of_id (parent :> t)
  | `Extension (parent, name) ->
      ret "Extension" (ExtensionName.to_string name) :: of_id (parent :> t)
  | `ExtensionDecl (parent, _, name) ->
      ret "ExtensionDecl" (ExtensionName.to_string name) :: of_id (parent :> t)
  | `Exception (parent, name) ->
      ret "Exception" (ExceptionName.to_string name) :: of_id (parent :> t)
  | `Value (parent, name) ->
      ret "Value" (ValueName.to_string name) :: of_id (parent :> t)
  | `Class (parent, name) ->
      ret "Class" (TypeName.to_string name) :: of_id (parent :> t)
  | `ClassType (parent, name) ->
      ret "ClassType" (TypeName.to_string name) :: of_id (parent :> t)
  | `Method (parent, name) ->
      ret "Method" (MethodName.to_string name) :: of_id (parent :> t)
  | `InstanceVariable (parent, name) ->
      ret "InstanceVariable" (InstanceVariableName.to_string name)
      :: of_id (parent :> t)
  | `Label (parent, name) ->
      ret "Label" (LabelName.to_string name) :: of_id (parent :> t)
  | `SourceLocationMod _ | `SourceLocation _ | `SourcePage _
  | `SourceLocationInternal _ ->
      [ `Null ]
(* TODO *)

let of_id n = `Array (List.rev @@ of_id (n :> Odoc_model.Paths.Identifier.t))

let of_doc (doc : Odoc_model.Comment.elements) =
  let txt = Text.of_doc doc in
  `String txt

let of_entry ({ Entry.id; doc; kind } as entry) html occurrences =
  let j_id = of_id id in
  let doc = of_doc doc in
  let kind =
    let return kind arr = `Object (("kind", `String kind) :: arr) in
    match kind with
    | TypeDecl { canonical = _; equation; representation = _ } ->
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
          | Some te -> `String (Text.of_type te)
        in
        let constraints =
          `Array
            (List.map
               (fun (lhs, rhs) ->
                 `Object
                   [
                     ("lhs", `String (Text.of_type lhs));
                     ("rhs", `String (Text.of_type rhs));
                   ])
               constraints)
        in
        return "TypeDecl"
          [
            ("private", private_);
            ("manifest", manifest);
            ("constraints", constraints);
          ]
    | Module _ -> return "Module" []
    | Value { value = _; type_ } ->
        return "Value" [ ("type", `String (Text.of_type type_)) ]
    | Doc -> return "Doc" []
    | Exception { args; res } ->
        let args = json_of_args args in
        let res = `String (Text.of_type res) in
        return "Exception" [ ("args", args); ("res", res) ]
    | Class_type { virtual_; params = _ } ->
        return "ClassType" [ ("virtual", `Bool virtual_) ]
    | Method { private_; virtual_; type_ } ->
        return "Method"
          [
            ("virtual", `Bool virtual_);
            ("private", `Bool private_);
            ("type", `String (Text.of_type type_));
          ]
    | Class { virtual_; params = _ } ->
        return "Class" [ ("virtual", `Bool virtual_) ]
    | TypeExtension { type_path = _; type_params = _; private_ } ->
        (* TODO: include type_path and type_params *)
        return "TypeExtension" [ ("private", `Bool private_) ]
    | ExtensionConstructor { args; res } ->
        let args = json_of_args args in
        let res = `String (Text.of_type res) in
        return "ExtensionConstructor" [ ("args", args); ("res", res) ]
    | ModuleType _ -> return "ModuleType" []
    | Constructor { args; res } ->
        let args = json_of_args args in
        let res = `String (Text.of_type res) in
        return "Constructor" [ ("args", args); ("res", res) ]
    | Field { mutable_; type_; parent_type } ->
        return "Field"
          [
            ("mutable", `Bool mutable_);
            ("type", `String (Text.of_type type_));
            ("parent_type", `String (Text.of_type parent_type));
          ]
    | Page _ -> return "Page" []
    | Impl -> return "Impl" []
    | Dir -> return "Dir" []
  in
  let occurrences =
    match occurrences with
    | Some { Odoc_occurrences.Table.direct; indirect } ->
        [
          ( "occurrences",
            `Object
              [
                ("direct", `Float (float_of_int direct));
                ("indirect", `Float (float_of_int indirect));
              ] );
        ]
    | None -> []
  in
  let display = Json_display.of_entry entry html in
  `Object
    ([ ("id", j_id); ("doc", doc); ("kind", kind); ("display", display) ]
    @ occurrences)

let of_entry ?occurrences entry =
  let get_occ id =
    match occurrences with
    | None -> None
    | Some occurrences -> (
        match Odoc_occurrences.Table.get occurrences id with
        | Some x -> Some x
        | None -> Some { direct = 0; indirect = 0 })
  in
  let entry, html, occurrences =
    let occ = get_occ entry.Entry.id in
    (entry, Html.of_entry entry, occ)
  in
  of_entry entry html occurrences
