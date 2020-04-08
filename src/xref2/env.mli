(* Env.mli *)

type lookup_result_found = { root : Odoc_model.Root.t; hidden : bool }

type lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

type root =
  | Resolved of (Odoc_model.Paths.Identifier.Module.t * Component.Module.t)
  | Forward

type resolver = {
  open_units : string list;
  lookup_unit : string -> lookup_unit_result;
  resolve_unit : Odoc_model.Root.t -> Odoc_model.Lang.Compilation_unit.t;
  lookup_page : string -> Odoc_model.Root.t option;
  resolve_page : Odoc_model.Root.t -> Odoc_model.Lang.Page.t;
}

type lookup_type =
  | Module of Odoc_model.Paths_types.Identifier.reference_module * bool
  | ModuleType of Odoc_model.Paths_types.Identifier.module_type * bool
  | RootModule of
      string
      * [ `Forward | `Resolved of Odoc_model.Paths.Identifier.Module.t ] option
  | ModuleByName of
      string * Odoc_model.Paths_types.Identifier.reference_module option
  | FragmentRoot of int

val pp_lookup_type_list : Format.formatter -> lookup_type list -> unit

type t

(*val pp_modules :
    Format.formatter ->
      (Odoc_model.Paths.Identifier.Module.t *
       Component.Module.t) list -> unit
*)
val pp_module_types :
  Format.formatter ->
  (Odoc_model.Paths.Identifier.ModuleType.t * Component.ModuleType.t) list ->
  unit

val pp_types :
  Format.formatter ->
  (Odoc_model.Paths.Identifier.Type.t * Component.TypeDecl.t) list ->
  unit

val pp_values :
  Format.formatter ->
  (Odoc_model.Paths.Identifier.Value.t * Component.Value.t) list ->
  unit

val pp_externals :
  Format.formatter ->
  (Odoc_model.Paths.Identifier.Value.t * Component.External.t) list ->
  unit

val with_recorded_lookups : t -> (t -> 'a) -> lookup_type list * 'a

val pp : Format.formatter -> t -> unit

val set_resolver : t -> resolver -> t

val has_resolver : t -> bool

val id : t -> int

exception MyFailure of Odoc_model.Paths.Identifier.t * t

val empty : t

val add_fragment_root : Component.Signature.t -> t -> t

val add_module :
  Odoc_model.Paths_types.Identifier.reference_module ->
  Component.Module.t ->
  t ->
  t

val add_type :
  Odoc_model.Paths_types.Identifier.type_ -> Component.TypeDecl.t -> t -> t

val add_module_type :
  Odoc_model.Paths_types.Identifier.reference_module_type ->
  Component.ModuleType.t ->
  t ->
  t

val add_value :
  Odoc_model.Paths_types.Identifier.value -> Component.Value.t -> t -> t

val add_external :
  Odoc_model.Paths_types.Identifier.value -> Component.External.t -> t -> t

val add_label : Odoc_model.Paths_types.Identifier.reference_label -> t -> t

val add_label_title :
  Odoc_model.Paths_types.Identifier.reference_label ->
  Odoc_model.Comment.link_content ->
  t ->
  t

val add_class :
  Odoc_model.Paths_types.Identifier.reference_class ->
  Component.Class.t ->
  t ->
  t

val add_class_type :
  Odoc_model.Paths_types.Identifier.class_type ->
  Component.ClassType.t ->
  t ->
  t

val add_docs : Odoc_model.Comment.docs -> t -> t

val add_comment : Odoc_model.Comment.docs_or_stop -> t -> t

val add_method :
  Odoc_model.Paths_types.Identifier.reference_method ->
  Component.Method.t ->
  t ->
  t

val add_root : string -> root -> t -> t

val lookup_fragment_root : t -> int * Component.Signature.t

val lookup_module :
  Odoc_model.Paths_types.Identifier.reference_module -> t -> Component.Module.t

val lookup_type :
  Odoc_model.Paths_types.Identifier.type_ -> t -> Component.TypeDecl.t

val lookup_module_type :
  Odoc_model.Paths_types.Identifier.reference_module_type ->
  t ->
  Component.ModuleType.t

val lookup_value :
  Odoc_model.Paths_types.Identifier.value -> t -> Component.Value.t

val lookup_section_title :
  Odoc_model.Paths_types.Identifier.reference_label ->
  t ->
  Odoc_model.Comment.link_content option

val lookup_class :
  Odoc_model.Paths_types.Identifier.reference_class -> t -> Component.Class.t

val lookup_class_type :
  Odoc_model.Paths_types.Identifier.class_type -> t -> Component.ClassType.t

val lookup_page : string -> t -> Odoc_model.Lang.Page.t option

val module_of_unit : Odoc_model.Lang.Compilation_unit.t -> Component.Module.t

val lookup_root_module : string -> t -> root option

val find_map : ('a -> 'b option) -> 'a list -> 'b option

val lookup_any_by_name : string -> t -> Component.Element.any option

val lookup_signature_by_name : string -> t -> Component.Element.signature option

val lookup_module_by_name : string -> t -> root option

val lookup_module_type_by_name :
  string -> t -> Component.Element.module_type option

val lookup_datatype_by_name : string -> t -> Component.Element.datatype option

val lookup_value_by_name :
  string ->
  t ->
  [ `External of Odoc_model.Paths_types.Identifier.value * Component.External.t
  | `Value of Odoc_model.Paths_types.Identifier.value * Component.Value.t ]
  option

val add_functor_args : Odoc_model.Paths_types.Identifier.signature -> t -> t

(* val open_component_signature :
  Odoc_model.Paths_types.Identifier.signature -> Component.Signature.t -> t -> t *)

val open_class_signature : Odoc_model.Lang.ClassSignature.t -> t -> t

val open_signature : Odoc_model.Lang.Signature.t -> t -> t

val open_unit : Odoc_model.Lang.Compilation_unit.t -> t -> t

val initial_env :
  Odoc_model.Lang.Compilation_unit.t ->
  resolver ->
  Odoc_model.Lang.Compilation_unit.Import.t list * t

val modules_of :
  t -> (Odoc_model.Paths.Identifier.Module.t * Component.Module.t) list

val len : int ref

val n : int ref

val verify_lookups : t -> lookup_type list -> bool
