open Odoc_model.Paths

val root : Odoc_model.Root.t Type_desc.t

val modulename : Odoc_model.Names.ModuleName.t Type_desc.t

val identifier : [< Identifier.t_pv ] Odoc_model.Paths.Identifier.id Type_desc.t

val resolved_path : [< Path.Resolved.t ] Type_desc.t

val path : [< Path.t ] Type_desc.t

val resolved_fragment : [< Fragment.Resolved.t ] Type_desc.t

val fragment : [< Fragment.t ] Type_desc.t

val reference : [< Reference.t ] Type_desc.t
