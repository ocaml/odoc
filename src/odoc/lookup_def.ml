open Odoc_model.Paths
open Odoc_model.Names
open Odoc_xref2.Utils.OptionSyntax
module Kind = Shape.Sig_component_kind

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

(** Project an identifier into a shape. *)
let rec project_id :
    Shape.t -> [< Identifier.t_pv ] Identifier.id -> Shape.t option =
  let proj shape parent kind name =
    let* shape = project_id shape parent in
    let item = Shape.Item.make name kind in
    Some (Shape.proj shape item)
  in
  fun shape id ->
    match id.iv with
    | `Module (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Module
          (ModuleName.to_string name)
    | `Root _ ->
        (* TODO: Assert that's the right root *)
        Some shape
    | _ -> None

let lookup_def (typing_env : Odoc_loader.typing_env) id =
  let* query = project_id typing_env.impl_shape id in
  let result = Shape.local_reduce query in
  let* uid = result.uid in
  let* loc = Shape.Uid.Tbl.find_opt typing_env.uid_to_loc uid in
  Some (Odoc_loader.read_location loc)
