open Odoc_model.Paths
open Odoc_model.Names
open Odoc_xref2.Utils.OptionSyntax
module Kind = Shape.Sig_component_kind

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
    | `Root _ ->
        (* TODO: Assert that's the right root *)
        Some shape
    | `Module (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Module
          (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Module_type
          (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        proj shape (parent :> Identifier.t) Kind.Type (TypeName.to_string name)
    | `Value (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Value (ValueName.to_string name)
    | `Extension (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Extension_constructor
          (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Extension_constructor
          (ExceptionName.to_string name)
    | `Class (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Class (ClassName.to_string name)
    | `ClassType (parent, name) ->
        proj shape
          (parent :> Identifier.t)
          Kind.Class_type
          (ClassTypeName.to_string name)
    | `Page _ | `LeafPage _ | `Label _ | `CoreType _ | `CoreException _
    | `Constructor _ | `Field _ | `Method _ | `InstanceVariable _ | `Parameter _
    | `Result _ ->
        (* Not represented in shapes. *)
        None

let lookup_def (typing_env : Odoc_loader.typing_env) id =
  let* query = project_id typing_env.impl_shape id in
  let result = Shape.local_reduce query in
  let* uid = result.uid in
  let* loc = Shape.Uid.Tbl.find_opt typing_env.uid_to_loc uid in
  Some (Odoc_loader.read_location loc)
