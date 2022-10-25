open Odoc_model.Paths

let lookup_root_module = ref (fun _ -> assert false)

module Reducer = Shape.Make_reduce (struct
  type env = Ocaml_env.t

  let fuel = 10

  let read_unit_shape ~unit_name =
    match !lookup_root_module unit_name with
    | Some (Env.Resolved unit) -> unit.shape
    | Some Forward | None -> None

  let find_shape env id =
    Ocaml_env.shape_of_path ~namespace:Shape.Sig_component_kind.Module env
      (Ocaml_path.Pident id)
end)

let uid_of_path env path ns =
  let shape = Ocaml_env.shape_of_path ~namespace:ns env path in
  let r = Reducer.reduce env shape in
  r.uid

let rec aux : Identifier.t -> Ocaml_path.t option =
  let open Utils.OptionSyntax in
  let open Odoc_model.Names in
  fun x ->
    match x.iv with
    | `Module (parent, name) ->
        let* parent = aux (parent :> Identifier.t) in
        Some (Ocaml_path.Pdot (parent, ModuleName.to_string name))
    | `Root (_, name) ->
        Some
          (Pident (Ocaml_ident.create_persistent (ModuleName.to_string name)))
    | _ -> None
(*
   | `Page (_, name) -> PageName.to_string name
   | `LeafPage (_, name) -> PageName.to_string name
   | `Parameter (_, name) -> ModuleName.to_string name
   | `Result x -> name_aux (x :> t)
   | `ModuleType (_, name) -> ModuleTypeName.to_string name
   | `Type (_, name) -> TypeName.to_string name
   | `CoreType name -> TypeName.to_string name
   | `Constructor (_, name) -> ConstructorName.to_string name
   | `Field (_, name) -> FieldName.to_string name
   | `Extension (_, name) -> ExtensionName.to_string name
   | `Exception (_, name) -> ExceptionName.to_string name
   | `CoreException name -> ExceptionName.to_string name
   | `Value (_, name) -> ValueName.to_string name
   | `Class (_, name) -> ClassName.to_string name
   | `ClassType (_, name) -> ClassTypeName.to_string name
   | `Method (_, name) -> MethodName.to_string name
   | `InstanceVariable (_, name) -> InstanceVariableName.to_string name
   | `Label (_, name) -> LabelName.to_string name
*)

let path_of_id : [< Identifier.t_pv ] Identifier.id -> Ocaml_path.t option =
 fun n -> aux (n :> Identifier.t)

let lookup_def env id ns =
  let open Utils.OptionSyntax in
  let* oenv = Env.get_ocaml_env env in
  (lookup_root_module := fun n -> Env.lookup_root_module n env);
  let* path = path_of_id id in
  let* uid = uid_of_path oenv path ns in
  let tbl = Ocaml_env.get_uid_to_loc_tbl () in
  Shape.Uid.Tbl.find_opt tbl uid
