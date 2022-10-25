open Odoc_model.Paths
open Odoc_model.Names

let lookup_root_module = ref (fun _ -> assert false)

module Reducer = Shape.Make_reduce (struct
  type env = Env.t

  let fuel = 10

  let read_unit_shape ~unit_name =
    match !lookup_root_module unit_name with
    | Some (Env.Resolved unit) -> unit.shape
    | Some Forward | None -> None

  (* TODO: What's in the env that's worth looking for ? *)
  let find_shape _env _ident = raise Not_found
end)

let shape_of_id id =
  let open Shape in
  let open Utils.OptionSyntax in
  let rec loop ~current_unit id =
    let proj parent item =
      let* parent = loop ~current_unit (parent :> Identifier.t) in
      (* let uid = Some (Uid.mk ~current_unit) *)
      let uid = None and desc = Proj (parent, item) in
      Some { uid; desc }
    in
    match id.Identifier.iv with
    | `Root (_, name) ->
        let name = ModuleName.to_string name in
        (* let ident = Ocaml_ident.create_persistent name in *)
        (* let uid = Some (Uid.of_compilation_unit_id ident) in *)
        let uid = None in
        Some { uid; desc = Comp_unit name }
    | `Module (parent, name) ->
        let ident = Ocaml_ident.create_local (ModuleName.to_string name) in
        proj parent (Item.module_ ident)
    | `CoreType _ | `CoreException _ | `Page _ | `LeafPage _ -> None
    (* | `Parameter (_, name) -> ModuleName.to_string name *)
    (* | `Result x -> name_aux (x :> t) *)
    (* | `ModuleType (_, name) -> ModuleTypeName.to_string name *)
    (* | `Type (_, name) -> TypeName.to_string name *)
    (* | `Constructor (_, name) -> ConstructorName.to_string name *)
    (* | `Field (_, name) -> FieldName.to_string name *)
    (* | `Extension (_, name) -> ExtensionName.to_string name *)
    (* | `Exception (_, name) -> ExceptionName.to_string name *)
    (* | `Value (_, name) -> ValueName.to_string name *)
    (* | `Class (_, name) -> ClassName.to_string name *)
    (* | `ClassType (_, name) -> ClassTypeName.to_string name *)
    (* | `Method (_, name) -> MethodName.to_string name *)
    (* | `InstanceVariable (_, name) -> InstanceVariableName.to_string name *)
    (* | `Label (_, name) -> LabelName.to_string name *)
    | _ -> None
  in
  let* (`Root (_, name)) = Identifier.root id in
  let current_unit = ModuleName.to_string name in
  loop ~current_unit (id :> Identifier.t)

let lookup_def env id =
  let open Utils.OptionSyntax in
  (lookup_root_module := fun n -> Env.lookup_root_module n env);
  let* query = shape_of_id id in
  Some (Reducer.reduce env query)
