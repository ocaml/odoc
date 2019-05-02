(*open Odoc_model.Paths*)
open Odoc_model.Names

type resolved_signature = [
  | `Root
  | `Subst of Cpath.resolved_module_type * resolved_module
  | `SubstAlias of Cpath.resolved_module * resolved_module
  | `Module of resolved_signature * ModuleName.t
]

and resolved_module = [
  | `Subst of Cpath.resolved_module_type * resolved_module
  | `SubstAlias of Cpath.resolved_module * resolved_module
  | `Module of resolved_signature * ModuleName.t
]

and resolved_type = [
  | `Type of resolved_signature * TypeName.t
  | `Class of resolved_signature * ClassName.t
  | `ClassType of resolved_signature * ClassTypeName.t
]

and signature = [
  `Resolved of resolved_signature
]