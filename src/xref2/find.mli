(* Find *)
open Odoc_model.Names
open Component

(** Context in which to perform search. Every search operation needs a context.
    The context can be expensive to compute. *)

type sig_ctx
type type_ctx
type typext_ctx
type class_sig_ctx

val context_of_sig : Signature.t -> sig_ctx
val context_of_type : TypeDecl.t -> type_ctx
val context_of_typext : Extension.t -> typext_ctx
val context_of_class_sig : ClassSignature.t -> class_sig_ctx

type module_ = [ `FModule of ModuleName.t * Module.t ]

type module_type = [ `FModuleType of ModuleTypeName.t * ModuleType.t ]

type datatype = [ `FType of TypeName.t * TypeDecl.t ]

type class_ =
  [ `FClass of ClassName.t * Class.t
  | `FClassType of ClassTypeName.t * ClassType.t ]

type value = [ `FValue of ValueName.t * Value.t ]

type label = [ `FLabel of Label.t ]

type exception_ = [ `FExn of ExceptionName.t * Exception.t ]

type extension = [ `FExt of Extension.t * Extension.Constructor.t ]

type substitution =
  [ `FModule_subst of ModuleSubstitution.t
  | `FType_subst of TypeDecl.t
  | `FModuleType_subst of ModuleTypeSubstitution.t ]

type signature = [ module_ | module_type ]

type type_ = [ datatype | class_ ]

type label_parent = [ signature | type_ ]

type constructor = [ `FConstructor of TypeDecl.Constructor.t ]

type field = [ `FField of TypeDecl.Field.t ]

type any_in_type = [ constructor | field ]

type any_in_type_in_sig = [ `In_type of TypeName.t * TypeDecl.t * any_in_type ]

type any_in_sig =
  [ label_parent
  | value
  | label
  | exception_
  | extension
  | substitution
  | any_in_type_in_sig ]

type instance_variable =
  [ `FInstance_variable of InstanceVariableName.t * InstanceVariable.t ]

type method_ = [ `FMethod of MethodName.t * Method.t ]

type any_in_class_sig = [ instance_variable | method_ ]

(** Lookup by name, unambiguous *)

val module_in_sig : sig_ctx -> string -> module_ option

val type_in_sig : sig_ctx -> string -> type_ option

val datatype_in_sig : sig_ctx -> string -> datatype option

val module_type_in_sig : sig_ctx -> string -> module_type option

val exception_in_sig : sig_ctx -> string -> exception_ option

val extension_in_sig : sig_ctx -> string -> extension option

val any_in_type : type_ctx -> string -> any_in_type option

val constructor_in_type : type_ctx -> string -> constructor option

val any_in_typext : typext_ctx -> string -> extension option

val method_in_class_signature : class_sig_ctx -> string -> method_ option

val instance_variable_in_class_signature :
  class_sig_ctx -> string -> instance_variable option

(** Maybe ambiguous *)

val class_in_sig : sig_ctx -> string -> class_ list

val signature_in_sig : sig_ctx -> string -> signature list

val value_in_sig : sig_ctx -> string -> value list

val label_in_sig : sig_ctx -> string -> label list

val label_parent_in_sig : sig_ctx -> string -> label_parent list

val any_in_sig : sig_ctx -> string -> any_in_sig list

val any_in_type_in_sig : sig_ctx -> string -> any_in_type_in_sig list

val any_in_class_signature : class_sig_ctx -> string -> any_in_class_sig list

(** Disambiguated lookups, returns the last match. *)

val class_in_sig_unambiguous : sig_ctx -> string -> class_ option

val value_in_sig_unambiguous : sig_ctx -> string -> value option

(** Lookup removed items *)

type removed_type =
  [ `FType_removed of TypeName.t * TypeExpr.t * TypeDecl.Equation.t ]

type careful_module = [ module_ | `FModule_removed of Cpath.Resolved.module_ ]

type careful_module_type =
  [ module_type | `FModuleType_removed of ModuleType.expr ]

type careful_type = [ type_ | removed_type ]

type careful_datatype = [ datatype | removed_type ]

type careful_class = [ class_ | removed_type ]

val careful_module_in_sig : sig_ctx -> string -> careful_module option

val careful_module_type_in_sig : sig_ctx -> string -> careful_module_type option

val careful_type_in_sig : sig_ctx -> string -> careful_type option

val careful_datatype_in_sig : sig_ctx -> string -> careful_datatype option

val careful_class_in_sig : sig_ctx -> string -> careful_class option
