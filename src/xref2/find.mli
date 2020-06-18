(* Find *)
open Odoc_model.Names

open Component

type module_ = [ `M of ModuleName.t * Module.t ]

type module_type = [ `MT of ModuleTypeName.t * ModuleType.t ]

type datatype = [ `T of TypeName.t * TypeDecl.t ]

type class_ = [ `C of ClassName.t * Class.t | `CT of ClassTypeName.t * ClassType.t ]

type value = [ `E of External.t | `V of Value.t ]

type label = [ `L of Ident.label ]

type exception_ = [ `Exn of Exception.t ]

type extension = [ `Ext of Extension.t * Extension.Constructor.t ]

type substitution = [ `Msub of ModuleSubstitution.t | `Tsub of TypeDecl.t ]

type signature = [ module_ | module_type ]

type type_ = [ datatype | class_ ]

type label_parent = [ signature | type_ ]

type constructor = [ `Cs of TypeDecl.Constructor.t ]

type field = [ `Fd of TypeDecl.Field.t ]

type any_in_type = [ constructor | field ]

type any_in_type_in_sig =
  [ `In_type of Odoc_model.Names.TypeName.t * TypeDecl.t * any_in_type ]

type any_in_sig =
  [ label_parent
  | value
  | label
  | exception_
  | extension
  | substitution
  | any_in_type_in_sig ]

type instance_variable = [ `Mv of InstanceVariable.t ]

type method_ = [ `Mm of Method.t ]

type any_in_class_sig = [ instance_variable | method_ ]

(** Lookup by name, unambiguous *)

val module_in_sig : Signature.t -> ModuleName.t -> module_ option

val type_in_sig : Signature.t -> string -> type_ option

val datatype_in_sig : Signature.t -> string -> datatype option

val module_type_in_sig : Signature.t -> ModuleTypeName.t -> module_type option

val exception_in_sig : Signature.t -> string -> exception_ option

val extension_in_sig : Signature.t -> string -> extension option

val any_in_type : TypeDecl.t -> string -> any_in_type option

val any_in_typext : Extension.t -> string -> extension option

val method_in_class_signature : ClassSignature.t -> string -> method_ option

val instance_variable_in_class_signature :
  ClassSignature.t -> string -> instance_variable option

(** Maybe ambiguous *)

val class_in_sig : Signature.t -> string -> class_ option

val signature_in_sig : Signature.t -> string -> signature option

val value_in_sig : Signature.t -> string -> value option

val label_in_sig : Signature.t -> string -> label option

val label_parent_in_sig : Signature.t -> string -> label_parent option

val any_in_sig : Signature.t -> string -> any_in_sig option

val any_in_type_in_sig :
  Signature.t -> string -> (Odoc_model.Names.TypeName.t * any_in_type) option

val any_in_class_signature :
  ClassSignature.t -> string -> any_in_class_sig option

(** Lookup removed items *)

type removed_type = [ `T_removed of TypeName.t * TypeExpr.t ]

type careful_module = [ module_ | `M_removed of Cpath.Resolved.module_ ]

type careful_type = [ type_ | removed_type ]

type careful_class = [ class_ | removed_type ]

val careful_module_in_sig : Signature.t -> ModuleName.t -> careful_module option

val careful_type_in_sig : Signature.t -> string -> careful_type option

val careful_class_in_sig : Signature.t -> string -> careful_class option
