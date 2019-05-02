(* Component reference *)

(* The only difference between this and normal references is that resolved references in
   these types can point at local identifiers *)

open Odoc_model.Paths_types
open Reference
open Odoc_model.Names

module rec Resolved : sig
  type datatype =
    [ `Identifier of Identifier.datatype
    | `Local of Ident.type_
    | `Type of signature * TypeName.t ]

  and module_ =
    [ `Identifier of Identifier.module_
    | `Local of Ident.module_
    | `SubstAlias of Cpath.resolved_module * module_
    | `Module of signature * ModuleName.t
    | `Canonical of module_ * Reference.module_ ]

  (* Signature is [ module | moduletype ] *)
  and signature =
    [ `Identifier of Identifier.signature
    | `Local of Ident.signature
    | `SubstAlias of Cpath.resolved_module * module_
    | `Module of signature * ModuleName.t
    | `Canonical of module_ * Reference.module_
    | `ModuleType of signature * ModuleTypeName.t ]

  and class_signature =
    [ `Identifier of Identifier.class_signature
    | `Local of Ident.class_signature
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t ]

  type parent_no_id =
    [ `SubstAlias of Cpath.resolved_module * module_
    | `Module of signature * ModuleName.t
    | `Canonical of module_ * Reference.module_
    | `ModuleType of signature * ModuleTypeName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Type of signature * TypeName.t ]

  (* parent is [ signature | class_signature ] *)
  type parent =
    [ `Identifier of Identifier.parent
    | `Local of Ident.parent
    | parent_no_id ]

  (* The only difference between parent and label_parent
     is that the Identifier allows more types *)
  and label_parent =
    [ `Identifier of Identifier.label_parent
    | `Local of Ident.label_parent
    | parent_no_id ]
  
  type s_substalias = [ `SubstAlias of Cpath.resolved_module * module_ ]

  type s_module = [ `Module of signature * ModuleName.t ]

  type s_canonical = [ `Canonical of module_ * Reference.module_ ]

  type s_module_type = [ `ModuleType of signature * ModuleTypeName.t ]

  type s_type = [ `Type of signature * TypeName.t ]

  type s_constructor = [ `Constructor of datatype * ConstructorName.t ]

  type s_field = [ `Field of parent * FieldName.t ]

  type s_extension = [ `Extension of signature * ExtensionName.t ]

  type s_exception = [ `Exception of signature * ExceptionName.t ]

  type s_value = [ `Value of signature * ValueName.t ]

  type s_class = [ `Class of signature * ClassName.t ]

  type s_class_type = [ `ClassType of signature * ClassTypeName.t ]

  type s_method = [ `Method of class_signature * MethodName.t ]

  type s_instance_variable =
    [ `InstanceVariable of class_signature * InstanceVariableName.t ]

  type s_label = [ `Label of label_parent * LabelName.t ]

  type module_no_id = [ s_substalias | s_module | s_canonical ]

  type signature_no_id = [ module_no_id | s_module_type ]

  type class_signature_no_id = [ s_class | s_class_type ]

  type datatype_no_id = [ | s_type ]

  type module_type =
    [ `Identifier of Identifier.reference_module_type
    | `Local of Ident.module_type
    | s_module_type ]

  type type_ =
    [ `Identifier of Identifier.reference_type
    | `Local of Ident.path_type
    | s_type
    | s_class
    | s_class_type ]

  type constructor =
    [ `Identifier of Identifier.reference_constructor
    | `Local of constructor
    | s_constructor
    | s_extension
    | s_exception ]

  type constructor_no_id = [ s_constructor | s_extension | s_exception ]

  type field =
    [ `Identifier of Identifier.reference_field
    | `Local of Ident.field
    | s_field ]

  type extension =
    [ `Identifier of Identifier.reference_extension
    | `Local of Ident.extension
    | s_exception
    | s_extension ]

  type extension_no_id = [ s_exception | s_extension ]

  type exception_ =
    [ `Identifier of Identifier.reference_exception
    | `Local of Ident.exception_
    | s_exception ]

  type value =
    [ `Identifier of Identifier.reference_value
    | `Local of Ident.value
    | s_value ]

  type class_ =
    [ `Identifier of Identifier.reference_class
    | `Local of Ident.class_
    | s_class ]

  type class_type =
    [ `Identifier of Identifier.reference_class_type
    | `Local of Ident.path_class_type
    | s_class
    | s_class_type ]

  type class_type_no_id = [ s_class | s_class_type ]

  type method_ =
    [ `Identifier of Identifier.reference_method
    | `Local of Ident.method_
    | s_method ]

  type instance_variable =
    [ `Identifier of Identifier.reference_instance_variable
    | `Local of Ident.instance_variable
    | s_instance_variable ]

  type label =
    [ `Identifier of Identifier.reference_label
    | `Local of Ident.label
    | s_label ]

  type page = [ `Identifier of Identifier.reference_page | `Local of Ident.page ]

  type any =
    [ `Identifier of Identifier.any
    | `Local of Ident.any
    | s_substalias
    | s_module
    | s_canonical
    | s_module_type
    | s_type
    | s_constructor
    | s_field
    | s_extension
    | s_exception
    | s_value
    | s_class
    | s_class_type
    | s_method
    | s_instance_variable
    | s_label ]
end =
  Resolved

and Reference : sig
  type signature =
    [ `Resolved of Resolved.signature
    | `Root of UnitName.t * tag_signature
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t ]

  and class_signature =
    [ `Resolved of Resolved.class_signature
    | `Root of UnitName.t * tag_class_signature
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t ]

  and datatype =
    [ `Resolved of Resolved.datatype
    | `Root of UnitName.t * tag_datatype
    | `Dot of label_parent * string
    | `Type of signature * TypeName.t ]

  and parent =
    [ `Resolved of Resolved.parent
    | `Root of UnitName.t * tag_parent
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Type of signature * TypeName.t ]

  and label_parent =
    [ `Resolved of Resolved.label_parent
    | `Root of UnitName.t * tag_label_parent
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Type of signature * TypeName.t ]

  type module_ =
    [ `Resolved of Resolved.module_
    | `Root of UnitName.t * tag_module
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t ]

  type module_type =
    [ `Resolved of Resolved.module_type
    | `Root of UnitName.t * tag_module_type
    | `Dot of label_parent * string
    | `ModuleType of signature * ModuleTypeName.t ]

  type type_ =
    [ `Resolved of Resolved.type_
    | `Root of UnitName.t * tag_type
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Type of signature * TypeName.t ]

  type constructor =
    [ `Resolved of Resolved.constructor
    | `Root of UnitName.t * tag_constructor
    | `Dot of label_parent * string
    | `Constructor of datatype * ConstructorName.t
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t ]

  type field =
    [ `Resolved of Resolved.field
    | `Root of UnitName.t * tag_field
    | `Dot of label_parent * string
    | `Field of parent * FieldName.t ]

  type extension =
    [ `Resolved of Resolved.extension
    | `Root of UnitName.t * tag_extension
    | `Dot of label_parent * string
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t ]

  type exception_ =
    [ `Resolved of Resolved.exception_
    | `Root of UnitName.t * tag_exception
    | `Dot of label_parent * string
    | `Exception of signature * ExceptionName.t ]

  type value =
    [ `Resolved of Resolved.value
    | `Root of UnitName.t * tag_value
    | `Dot of label_parent * string
    | `Value of signature * ValueName.t ]

  type class_ =
    [ `Resolved of Resolved.class_
    | `Root of UnitName.t * tag_class
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t ]

  type class_type =
    [ `Resolved of Resolved.class_type
    | `Root of UnitName.t * tag_class_type
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t ]

  type method_ =
    [ `Resolved of Resolved.method_
    | `Root of UnitName.t * tag_method
    | `Dot of label_parent * string
    | `Method of class_signature * MethodName.t ]

  type instance_variable =
    [ `Resolved of Resolved.instance_variable
    | `Root of UnitName.t * tag_instance_variable
    | `Dot of label_parent * string
    | `InstanceVariable of class_signature * InstanceVariableName.t ]

  type label =
    [ `Resolved of Resolved.label
    | `Root of UnitName.t * tag_label
    | `Dot of label_parent * string
    | `Label of label_parent * LabelName.t ]

  type page =
    [ `Resolved of Resolved.page
    | `Root of UnitName.t * tag_page
    | `Dot of label_parent * string ]

  type any =
    [ `Resolved of Resolved.any
    | `Root of UnitName.t * tag_any
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Type of signature * TypeName.t
    | `Constructor of datatype * ConstructorName.t
    | `Field of parent * FieldName.t
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t
    | `Value of signature * ValueName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Method of class_signature * MethodName.t
    | `InstanceVariable of class_signature * InstanceVariableName.t
    | `Label of label_parent * LabelName.t ]
end =
  Reference

include Reference

let rec signature_identifier_of_resolved_reference (r : Resolved.signature) =
  match r with
  | `Local _ -> failwith "broken"
  | `Identifier id -> id
  | `Module (parent, name) ->
      `Module (signature_identifier_of_resolved_reference parent, name)
  | `ModuleType (parent, name) ->
      `ModuleType (signature_identifier_of_resolved_reference parent, name)
  | `SubstAlias (_m1, _m2) -> failwith "unhandled"
  | `Canonical (_m1, _m2) -> failwith "unhandled"
