(* Env.mli *)

open Odoc_model
open Odoc_model.Paths

type lookup_unit_result =
  | Forward_reference
  | Found of Lang.Compilation_unit.t
  | Not_found

type lookup_page_result = Lang.Page.t option

type root =
  | Resolved of
      (Root.t * Odoc_model.Paths.Identifier.Module.t * Component.Module.t)
  | Forward

type resolver = {
  open_units : string list;
  lookup_unit : string -> lookup_unit_result;
  lookup_page : string -> lookup_page_result;
}

type lookup_type =
  | Module of Identifier.Path.Module.t
  | ModuleType of Identifier.ModuleType.t
  | RootModule of string * [ `Forward | `Resolved of Digest.t ] option
  | ModuleByName of string * Identifier.Path.Module.t
  | FragmentRoot of int

module LookupTypeSet : Set.S with type elt = lookup_type

val pp_lookup_type_list : Format.formatter -> lookup_type list -> unit

type t

val is_linking : t -> bool

val with_recorded_lookups : t -> (t -> 'a) -> LookupTypeSet.t * 'a

val set_resolver : t -> resolver -> t

val has_resolver : t -> bool

val id : t -> int

val empty : t

val add_fragment_root : Component.Signature.t -> t -> t

val add_module :
  Identifier.Path.Module.t ->
  Component.Module.t Component.Delayed.t ->
  Component.CComment.docs ->
  t ->
  t

val add_type : Identifier.Type.t -> Component.TypeDecl.t -> t -> t

val add_module_type :
  Identifier.Path.ModuleType.t -> Component.ModuleType.t -> t -> t

val add_value : Identifier.Value.t -> Component.Value.t -> t -> t

val add_label : Identifier.Label.t -> Component.Label.t -> t -> t

val add_class : Identifier.Class.t -> Component.Class.t -> t -> t

val add_class_type : Identifier.ClassType.t -> Component.ClassType.t -> t -> t

val add_exception : Identifier.Exception.t -> Component.Exception.t -> t -> t

val add_extension_constructor :
  Identifier.Extension.t ->
  Component.Extension.Constructor.t ->
  Component.Extension.t ->
  t ->
  t

val add_docs : Comment.docs -> t -> t

val add_comment : Comment.docs_or_stop -> t -> t

val add_method : Identifier.Method.t -> Component.Method.t -> t -> t

val add_module_functor_args :
  Component.Module.t -> Identifier.Path.Module.t -> t -> t

val add_module_type_functor_args :
  Component.ModuleType.t -> Identifier.ModuleType.t -> t -> t

val lookup_fragment_root : t -> (int * Component.Signature.t) option

val lookup_page : string -> t -> Lang.Page.t option

val lookup_unit : string -> t -> lookup_unit_result option

val module_of_unit : Lang.Compilation_unit.t -> Component.Module.t

val lookup_root_module : string -> t -> root option

type 'a scope constraint 'a = [< Component.Element.any ]
(** Target of a lookup *)

type 'a maybe_ambiguous =
  ('a, [ `Ambiguous of 'a * 'a list | `Not_found ]) Result.result

val lookup_by_name : 'a scope -> string -> t -> 'a maybe_ambiguous
(** Lookup an element in Env depending on the given [scope]. Return
    [Error (`Ambiguous _)] when two or more elements match the given scope and
    name. *)

val lookup_by_id :
  'a scope -> [< Identifier.t_pv ] Paths.Identifier.id -> t -> 'a option
(** Like [lookup_by_name] but use an identifier as key. *)

val s_any : Component.Element.any scope

val s_signature : Component.Element.signature scope

val s_module : Component.Element.module_ scope

val s_module_type : Component.Element.module_type scope

val s_type : Component.Element.type_ scope

val s_datatype : Component.Element.datatype scope

val s_class : Component.Element.class_ scope

val s_class_type : Component.Element.class_type scope

val s_value : Component.Element.value scope

val s_label : Component.Element.label scope

val s_constructor : Component.Element.constructor scope

val s_exception : Component.Element.exception_ scope

val s_extension : Component.Element.extension scope

val s_field : Component.Element.field scope

val s_label_parent : Component.Element.label_parent scope

val s_fragment_type_parent : Component.Element.fragment_type_parent scope

(* val open_component_signature :
   Paths_types.Identifier.signature -> Component.Signature.t -> t -> t *)

val add_functor_parameter : Lang.FunctorParameter.t -> t -> t

val open_class_signature : Lang.ClassSignature.t -> t -> t

val open_signature : Lang.Signature.t -> t -> t

val open_type_substitution : Lang.TypeDecl.t -> t -> t

val open_module_substitution : Lang.ModuleSubstitution.t -> t -> t

val open_module_type_substitution : Lang.ModuleTypeSubstitution.t -> t -> t

val open_page : Lang.Page.t -> t -> t
(** Add a page content to the env. *)

val env_of_unit : Lang.Compilation_unit.t -> linking:bool -> resolver -> t
(** Create a new env with a module initially opened. *)

val env_of_page : Lang.Page.t -> resolver -> t
(** Create a new env for a page. *)

val env_for_reference : resolver -> t
(** Create a new env for a reference. *)

val env_for_testing : linking:bool -> t
(** Create a new env for testing purposes *)

val inherit_resolver : t -> t
(** Create an empty environment reusing the same resolver. *)

val len : int ref

val n : int ref

val verify_lookups : t -> LookupTypeSet.t -> bool
