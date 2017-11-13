(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open DocOckPaths
open DocOckTypes

val option_map : ('a -> 'a) -> 'a option -> 'a option

val list_map : ('a -> 'a) -> 'a list -> 'a list

val pair_map : ('a -> 'a) -> ('b -> 'b) -> ('a * 'b) -> ('a * 'b)

class virtual ['a] identifier : object

  method virtual root : 'a -> 'a

  method identifier : 'k. ('a, 'k) Identifier.t -> ('a, 'k) Identifier.t

  method identifier_root_name : string -> string

  method identifier_page_name : string -> string

  method identifier_module_name : string -> string

  method identifier_argument_position : int -> int

  method identifier_argument_name : string -> string

  method identifier_module_type_name : string -> string

  method identifier_type_name : string -> string

  method identifier_core_type_name : string -> string

  method identifier_constructor_name : string -> string

  method identifier_field_name : string -> string

  method identifier_extension_name : string -> string

  method identifier_exception_name : string -> string

  method identifier_core_exception_name : string -> string

  method identifier_value_name : string -> string

  method identifier_class_name : string -> string

  method identifier_class_type_name : string -> string

  method identifier_method_name : string -> string

  method identifier_instance_variable_name : string -> string

  method identifier_label_name : string -> string

  method identifier_page : 'a Identifier.page -> 'a Identifier.page

  method identifier_signature : 'a Identifier.signature ->
    'a Identifier.signature

  method identifier_class_signature : 'a Identifier.class_signature ->
    'a Identifier.class_signature

  method identifier_datatype : 'a Identifier.datatype ->
    'a Identifier.datatype

  method identifier_module : 'a Identifier.module_ ->
    'a Identifier.module_

  method identifier_module_type : 'a Identifier.module_type ->
    'a Identifier.module_type

  method identifier_type : 'a Identifier.type_ -> 'a Identifier.type_

  method identifier_constructor : 'a Identifier.constructor ->
    'a Identifier.constructor

  method identifier_field : 'a Identifier.field -> 'a Identifier.field

  method identifier_extension : 'a Identifier.extension ->
    'a Identifier.extension

  method identifier_exception : 'a Identifier.exception_ ->
    'a Identifier.exception_

  method identifier_value : 'a Identifier.value -> 'a Identifier.value

  method identifier_class : 'a Identifier.class_ -> 'a Identifier.class_

  method identifier_class_type : 'a Identifier.class_type ->
    'a Identifier.class_type

  method identifier_method : 'a Identifier.method_ -> 'a Identifier.method_

  method identifier_instance_variable : 'a Identifier.instance_variable ->
    'a Identifier.instance_variable

  method identifier_label : 'a Identifier.label -> 'a Identifier.label

end

class virtual ['a] path : object

  method virtual identifier : 'k. ('a, 'k) Identifier.t ->
    ('a, 'k) Identifier.t

  method path_resolved : 'k. ('a, 'k) Path.Resolved.t ->
    ('a, 'k) Path.Resolved.t

  method path_resolved_module_name : string -> string

  method path_resolved_module_type_name : string -> string

  method path_resolved_type_name : string -> string

  method path_resolved_class_name : string -> string

  method path_resolved_class_type_name : string -> string

  method path_resolved_module : 'a Path.Resolved.module_ ->
    'a Path.Resolved.module_

  method path_resolved_module_type : 'a Path.Resolved.module_type ->
    'a Path.Resolved.module_type

  method path_resolved_type : 'a Path.Resolved.type_ ->
    'a Path.Resolved.type_

  method path_resolved_class_type : 'a Path.Resolved.class_type ->
    'a Path.Resolved.class_type

  method path : 'k . ('a, 'k) Path.t -> ('a, 'k) Path.t

  method path_root_name : string -> string

  method path_dot_name : string -> string

  method path_module : 'a Path.module_ -> 'a Path.module_

  method path_module_type : 'a Path.module_type -> 'a Path.module_type

  method path_type : 'a Path.type_ -> 'a Path.type_

  method path_class_type : 'a Path.class_type -> 'a Path.class_type

end

class virtual ['a] fragment : object

  method virtual path_resolved : 'k. ('a, 'k) Path.Resolved.t ->
    ('a, 'k) Path.Resolved.t

  method fragment_resolved : 'k 's. ('a, 'k, 's) Fragment.Resolved.raw ->
    ('a, 'k, 's) Fragment.Resolved.raw

  method fragment_resolved_module_name : string -> string

  method fragment_resolved_type_name : string -> string

  method fragment_resolved_class_name : string -> string

  method fragment_resolved_class_type_name : string -> string

  method fragment_resolved_module : 'a Fragment.Resolved.module_ ->
    'a Fragment.Resolved.module_

  method fragment_resolved_type : 'a Fragment.Resolved.type_ ->
    'a Fragment.Resolved.type_

  method fragment : 'k 's. ('a, 'k, 's) Fragment.raw ->
    ('a, 'k, 's) Fragment.raw

  method fragment_name : string -> string

  method fragment_module : 'a Fragment.module_ -> 'a Fragment.module_

  method fragment_type : 'a Fragment.type_ -> 'a Fragment.type_

end

class virtual ['a] reference : object

  method virtual identifier : 'k. ('a, 'k) Identifier.t ->
    ('a, 'k) Identifier.t

  method virtual path_resolved : 'k. ('a, 'k) Path.Resolved.t ->
    ('a, 'k) Path.Resolved.t

  method reference_resolved : 'k. ('a, 'k) Reference.Resolved.t ->
    ('a, 'k) Reference.Resolved.t

  method reference_resolved_module_name : string -> string

  method reference_resolved_module_type_name : string -> string

  method reference_resolved_type_name : string -> string

  method reference_resolved_class_name : string -> string

  method reference_resolved_class_type_name : string -> string

  method reference_resolved_constructor_name : string -> string

  method reference_resolved_extension_name : string -> string

  method reference_resolved_exception_name : string -> string

  method reference_resolved_field_name : string -> string

  method reference_resolved_value_name : string -> string

  method reference_resolved_method_name : string -> string

  method reference_resolved_instance_variable_name : string -> string

  method reference_resolved_label_name : string -> string

  method reference_resolved_module : 'a Reference.Resolved.module_ ->
    'a Reference.Resolved.module_

  method reference_resolved_module_type : 'a Reference.Resolved.module_type ->
    'a Reference.Resolved.module_type

  method reference_resolved_type : 'a Reference.Resolved.type_ ->
    'a Reference.Resolved.type_

  method reference_resolved_constructor : 'a Reference.Resolved.constructor ->
    'a Reference.Resolved.constructor

  method reference_resolved_field : 'a Reference.Resolved.field ->
    'a Reference.Resolved.field

  method reference_resolved_extension : 'a Reference.Resolved.extension ->
    'a Reference.Resolved.extension

  method reference_resolved_exception : 'a Reference.Resolved.exception_ ->
    'a Reference.Resolved.exception_

  method reference_resolved_value : 'a Reference.Resolved.value ->
    'a Reference.Resolved.value

  method reference_resolved_class : 'a Reference.Resolved.class_ ->
    'a Reference.Resolved.class_

  method reference_resolved_class_type : 'a Reference.Resolved.class_type ->
    'a Reference.Resolved.class_type

  method reference_resolved_method : 'a Reference.Resolved.method_ ->
    'a Reference.Resolved.method_

  method reference_resolved_instance_variable :
    'a Reference.Resolved.instance_variable ->
    'a Reference.Resolved.instance_variable

  method reference_resolved_label : 'a Reference.Resolved.label ->
    'a Reference.Resolved.label

  method reference_resolved_any : 'a Reference.Resolved.any ->
    'a Reference.Resolved.any

  method reference : 'k. ('a, 'k) Reference.t -> ('a, 'k) Reference.t

  method reference_root_name : string -> string

  method reference_dot_name : string -> string

  method reference_module_name : string -> string

  method reference_module_type_name : string -> string

  method reference_type_name : string -> string

  method reference_constructor_name : string -> string

  method reference_field_name : string -> string

  method reference_extension_name : string -> string

  method reference_exception_name : string -> string

  method reference_value_name : string -> string

  method reference_class_name : string -> string

  method reference_class_type_name : string -> string

  method reference_method_name : string -> string

  method reference_instance_variable_name : string -> string

  method reference_label_name : string -> string

  method reference_module : 'a Reference.module_ -> 'a Reference.module_

  method reference_module_type : 'a Reference.module_type ->
    'a Reference.module_type

  method reference_type : 'a Reference.type_ -> 'a Reference.type_

  method reference_constructor : 'a Reference.constructor ->
    'a Reference.constructor

  method reference_field : 'a Reference.field -> 'a Reference.field

  method reference_extension : 'a Reference.extension ->
    'a Reference.extension

  method reference_exception : 'a Reference.exception_ ->
    'a Reference.exception_

  method reference_value : 'a Reference.value -> 'a Reference.value

  method reference_class : 'a Reference.class_ -> 'a Reference.class_

  method reference_class_type : 'a Reference.class_type ->
    'a Reference.class_type

  method reference_method : 'a Reference.method_ -> 'a Reference.method_

  method reference_instance_variable : 'a Reference.instance_variable ->
    'a Reference.instance_variable

  method reference_label : 'a Reference.label -> 'a Reference.label

  method reference_any : 'a Reference.any -> 'a Reference.any

end

class virtual ['a] paths : object
  inherit ['a] identifier
  inherit ['a] path
  inherit ['a] fragment
  inherit ['a] reference
end

class virtual ['a] documentation : object

  method virtual identifier_label : 'a Identifier.label -> 'a Identifier.label

  method virtual identifier :
    'k. ('a, 'k) Identifier.t -> ('a, 'k) Identifier.t

  method virtual path_module : 'a Path.module_ -> 'a Path.module_

  method virtual reference_module : 'a Reference.module_ ->
    'a Reference.module_

  method virtual reference_module_type : 'a Reference.module_type ->
    'a Reference.module_type

  method virtual reference_type : 'a Reference.type_ -> 'a Reference.type_

  method virtual reference_constructor : 'a Reference.constructor ->
    'a Reference.constructor

  method virtual reference_field : 'a Reference.field -> 'a Reference.field

  method virtual reference_extension : 'a Reference.extension ->
    'a Reference.extension

  method virtual reference_exception : 'a Reference.exception_ ->
    'a Reference.exception_

  method virtual reference_value : 'a Reference.value -> 'a Reference.value

  method virtual reference_class : 'a Reference.class_ -> 'a Reference.class_

  method virtual reference_class_type : 'a Reference.class_type ->
    'a Reference.class_type

  method virtual reference_method : 'a Reference.method_ ->
    'a Reference.method_

  method virtual reference_instance_variable : 'a Reference.instance_variable ->
    'a Reference.instance_variable

  method virtual reference_label : 'a Reference.label -> 'a Reference.label

  method virtual reference_any : 'a Reference.any -> 'a Reference.any

  method documentation_style : Documentation.style -> Documentation.style

  method documentation_style_custom : string -> string

  method documentation_reference : 'a Documentation.reference ->
    'a Documentation.reference

  method documentation_reference_link : string -> string

  method documentation_reference_custom : string -> string

  method documentation_reference_custom_body : string -> string

  method documentation_special : 'a Documentation.special ->
    'a Documentation.special

  method documentation_special_modules :
    'a Reference.module_ * 'a Documentation.text ->
    'a Reference.module_ * 'a Documentation.text

  method documentation_see : Documentation.see -> Documentation.see

  method documentation_see_url : string -> string

  method documentation_see_file : string -> string

  method documentation_see_doc : string -> string

  method documentation_text_element : 'a Documentation.text_element ->
    'a Documentation.text_element

  method documentation_text_raw : string -> string

  method documentation_text_code : string -> string

  method documentation_text_precode : string -> string

  method documentation_text_verbatim : string -> string

  method documentation_text_title_level : int -> int

  method documentation_text_target : string option -> string option

  method documentation_text_target_body : string -> string

  method documentation_text : 'a Documentation.text -> 'a Documentation.text

  method documentation_tag : 'a Documentation.tag -> 'a Documentation.tag

  method documentation_tag_author : string -> string

  method documentation_tag_version : string -> string

  method documentation_tag_since : string -> string

  method documentation_tag_before : string -> string

  method documentation_tag_param : string -> string

  method documentation_tag_raise : string -> string

  method documentation_tag_name : string -> string

  method documentation_tags : 'a Documentation.tag list ->
    'a Documentation.tag list

  method documentation_error_position : Documentation.Error.Position.t ->
    Documentation.Error.Position.t

  method documentation_error_position_column : int -> int

  method documentation_error_position_line : int -> int

  method documentation_error_offset : Documentation.Error.Offset.t ->
    Documentation.Error.Offset.t

  method documentation_error_location : Documentation.Error.Location.t ->
    Documentation.Error.Location.t

  method documentation_error_location_filename : string -> string

  method documentation_error : 'a Documentation.Error.t ->
    'a Documentation.Error.t

  method documentation_error_message : string -> string

  method documentation_body : 'a Documentation.body -> 'a Documentation.body

  method documentation : 'a Documentation.t -> 'a Documentation.t

  method documentation_comment : 'a Documentation.comment ->
    'a Documentation.comment

end

class virtual ['a] module_ : object

  method virtual identifier_module : 'a Identifier.module_ ->
    'a Identifier.module_

  method virtual path_module : 'a Path.module_ -> 'a Path.module_

  method virtual reference_module : 'a Reference.module_ -> 'a Reference.module_

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual module_type_expr : 'a ModuleType.expr -> 'a ModuleType.expr

  method virtual signature : 'a Signature.t -> 'a Signature.t

  method virtual module_type_functor_arg :
    'a FunctorArgument.t option -> 'a FunctorArgument.t option

  method module_expansion : 'a Module.expansion -> 'a Module.expansion

  method module_decl : 'a Module.decl -> 'a Module.decl

  method module_ : 'a Module.t -> 'a Module.t

  method module_equation : 'a Module.decl -> 'a Module.decl

  method module_hidden : bool -> bool
end

class virtual ['a] module_type : object

  method virtual identifier_module : 'a Identifier.module_ ->
    'a Identifier.module_

  method virtual identifier_module_type : 'a Identifier.module_type ->
    'a Identifier.module_type

  method virtual path_module : 'a Path.module_ -> 'a Path.module_

  method virtual path_module_type : 'a Path.module_type -> 'a Path.module_type

  method virtual path_type : 'a Path.type_ -> 'a Path.type_

  method virtual fragment_module : 'a Fragment.module_ -> 'a Fragment.module_

  method virtual fragment_type : 'a Fragment.type_ -> 'a Fragment.type_

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual module_decl : 'a Module.decl -> 'a Module.decl

  method virtual module_equation : 'a Module.Equation.t -> 'a Module.Equation.t

  method virtual signature : 'a Signature.t -> 'a Signature.t

  method virtual type_decl_equation : 'a TypeDecl.Equation.t ->
    'a TypeDecl.Equation.t

  method virtual type_decl_param_name : string -> string

  method virtual module_expansion : 'a Module.expansion -> 'a Module.expansion

  method module_type_substitution : 'a ModuleType.substitution ->
    'a ModuleType.substitution

  method module_type_expr : 'a ModuleType.expr -> 'a ModuleType.expr

  method module_type_functor_arg :
    'a FunctorArgument.t option -> 'a FunctorArgument.t option

  method module_type : 'a ModuleType.t -> 'a ModuleType.t

end

class virtual ['a] signature : object

  method virtual documentation_comment : 'a Documentation.comment ->
    'a Documentation.comment

  method virtual module_ : 'a Module.t -> 'a Module.t

  method virtual module_type : 'a ModuleType.t -> 'a ModuleType.t

  method virtual type_decl : 'a TypeDecl.t -> 'a TypeDecl.t

  method virtual extension : 'a Extension.t -> 'a Extension.t

  method virtual exception_ : 'a Exception.t -> 'a Exception.t

  method virtual value : 'a Value.t -> 'a Value.t

  method virtual external_ : 'a External.t -> 'a External.t

  method virtual class_ : 'a Class.t -> 'a Class.t

  method virtual class_type : 'a ClassType.t -> 'a ClassType.t

  method virtual include_ : 'a Include.t -> 'a Include.t

  method signature_item : 'a Signature.item -> 'a Signature.item

  method signature : 'a Signature.t -> 'a Signature.t

end

class virtual ['a] include_ : object

  method virtual module_decl : 'a Module.decl -> 'a Module.decl

  method virtual identifier_signature : 'a Identifier.signature ->
                                        'a Identifier.signature

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual signature : 'a Signature.t -> 'a Signature.t

  method include_expansion_resolved : bool -> bool

  method include_expansion : 'a Include.expansion -> 'a Include.expansion

  method include_ : 'a Include.t -> 'a Include.t

end

class virtual ['a] type_decl : object

  method virtual identifier_type : 'a Identifier.type_ -> 'a Identifier.type_

  method virtual identifier_constructor : 'a Identifier.constructor ->
    'a Identifier.constructor

  method virtual identifier_field : 'a Identifier.field -> 'a Identifier.field

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method type_decl_constructor : 'a TypeDecl.Constructor.t ->
    'a TypeDecl.Constructor.t

  method type_decl_constructor_argument : 'a TypeDecl.Constructor.argument ->
    'a TypeDecl.Constructor.argument

  method type_decl_field : 'a TypeDecl.Field.t -> 'a TypeDecl.Field.t

  method type_decl_field_mutable : bool -> bool

  method type_decl_representation : 'a TypeDecl.Representation.t ->
    'a TypeDecl.Representation.t

  method type_decl_variance : TypeDecl.variance -> TypeDecl.variance

  method type_decl_param_desc : TypeDecl.param_desc -> TypeDecl.param_desc

  method type_decl_param_name : string -> string

  method type_decl_param : TypeDecl.param -> TypeDecl.param

  method type_decl_equation : 'a TypeDecl.Equation.t ->
    'a TypeDecl.Equation.t

  method type_decl_private : bool -> bool

  method type_decl_constraint : 'a TypeExpr.t * 'a TypeExpr.t ->
    'a TypeExpr.t * 'a TypeExpr.t

  method type_decl : 'a TypeDecl.t -> 'a TypeDecl.t

end

class virtual ['a] extension : object

  method virtual identifier_extension : 'a Identifier.extension ->
    'a Identifier.extension

  method virtual path_type : 'a Path.type_ -> 'a Path.type_

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual type_decl_private : bool -> bool

  method virtual type_decl_constructor_argument :
    'a TypeDecl.Constructor.argument -> 'a TypeDecl.Constructor.argument

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method extension_constructor : 'a Extension.Constructor.t ->
    'a Extension.Constructor.t

  method extension : 'a Extension.t -> 'a Extension.t

end

class virtual ['a] exception_ : object

  method virtual identifier_exception : 'a Identifier.exception_ ->
    'a Identifier.exception_

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method virtual type_decl_constructor_argument :
    'a TypeDecl.Constructor.argument -> 'a TypeDecl.Constructor.argument

  method exception_ : 'a Exception.t -> 'a Exception.t

end

class virtual ['a] value : object

  method virtual identifier_value : 'a Identifier.value -> 'a Identifier.value

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method value : 'a Value.t -> 'a Value.t
end

class virtual ['a] external_ : object

  method virtual identifier_value : 'a Identifier.value -> 'a Identifier.value

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method external_ : 'a External.t -> 'a External.t

  method external_primitive : string -> string

end

class virtual ['a] class_ : object

  method virtual identifier_class : 'a Identifier.class_ -> 'a Identifier.class_

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual class_type_expr : 'a ClassType.expr -> 'a ClassType.expr

  method virtual type_expr_label : TypeExpr.label -> TypeExpr.label

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method virtual class_signature : 'a ClassSignature.t -> 'a ClassSignature.t

  method class_decl : 'a Class.decl -> 'a Class.decl

  method class_ : 'a Class.t -> 'a Class.t

  method class_virtual : bool -> bool

end

class virtual ['a] class_type : object

  method virtual identifier_class_type : 'a Identifier.class_type ->
    'a Identifier.class_type

  method virtual path_class_type : 'a Path.class_type -> 'a Path.class_type

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual class_signature : 'a ClassSignature.t -> 'a ClassSignature.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method class_type_expr : 'a ClassType.expr -> 'a ClassType.expr

  method class_type : 'a ClassType.t -> 'a ClassType.t

  method class_type_virtual : bool -> bool

end

class virtual ['a] class_signature : object

  method virtual documentation_comment : 'a Documentation.comment ->
    'a Documentation.comment

  method virtual class_type_expr : 'a ClassType.expr -> 'a ClassType.expr

  method virtual method_ : 'a Method.t -> 'a Method.t

  method virtual instance_variable : 'a InstanceVariable.t ->
    'a InstanceVariable.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method class_signature_item : 'a ClassSignature.item ->
    'a ClassSignature.item

  method class_signature : 'a ClassSignature.t -> 'a ClassSignature.t

end

class virtual ['a] method_ : object

  method virtual identifier_method : 'a Identifier.method_ ->
    'a Identifier.method_

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method method_ : 'a Method.t -> 'a Method.t

  method method_private : bool -> bool

  method method_virtual : bool -> bool

end

class virtual ['a] instance_variable : object

  method virtual identifier_instance_variable :
    'a Identifier.instance_variable -> 'a Identifier.instance_variable

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method instance_variable : 'a InstanceVariable.t -> 'a InstanceVariable.t

  method instance_variable_mutable : bool -> bool

  method instance_variable_virtual : bool -> bool

end

class virtual ['a] type_expr : object

  method virtual path_module_type : 'a Path.module_type -> 'a Path.module_type

  method virtual path_type : 'a Path.type_ -> 'a Path.type_

  method virtual path_class_type : 'a Path.class_type -> 'a Path.class_type

  method virtual fragment_type : 'a Fragment.type_ -> 'a Fragment.type_

  method type_expr_variant_kind : TypeExpr.Variant.kind ->
    TypeExpr.Variant.kind

  method type_expr_variant_element : 'a TypeExpr.Variant.element ->
    'a TypeExpr.Variant.element

  method type_expr_variant_constructor_name : string -> string

  method type_expr_variant_constructor_const : bool -> bool

  method type_expr_variant : 'a TypeExpr.Variant.t -> 'a TypeExpr.Variant.t

  method type_expr_object_method : 'a TypeExpr.Object.method_ ->
    'a TypeExpr.Object.method_

  method type_expr_object_method_name : string -> string

  method type_expr_object : 'a TypeExpr.Object.t -> 'a TypeExpr.Object.t

  method type_expr_object_open : bool -> bool

  method type_expr_package_substitution : 'a TypeExpr.Package.substitution ->
    'a TypeExpr.Package.substitution

  method type_expr_package : 'a TypeExpr.Package.t -> 'a TypeExpr.Package.t

  method type_expr_label : TypeExpr.label -> TypeExpr.label

  method type_expr_label_name : string -> string

  method type_expr : 'a TypeExpr.t -> 'a TypeExpr.t

  method type_expr_var_name : string -> string

end

class virtual ['a] unit : object

  method virtual root : 'a -> 'a

  method virtual identifier_module : 'a Identifier.module_ ->
    'a Identifier.module_

  method virtual path_module :
    'a Path.module_ -> 'a Path.module_

  method virtual documentation : 'a Documentation.t -> 'a Documentation.t

  method virtual signature : 'a Signature.t -> 'a Signature.t

  method unit_import : 'a Unit.Import.t -> 'a Unit.Import.t

  method unit_import_name : string -> string

  method unit_import_digest : Digest.t -> Digest.t

  method unit_source : 'a Unit.Source.t -> 'a Unit.Source.t

  method unit_source_file : string -> string

  method unit_source_build_dir : string -> string

  method unit_source_digest : Digest.t -> Digest.t

  method unit_packed_item : 'a Unit.Packed.item -> 'a Unit.Packed.item

  method unit_packed : 'a Unit.Packed.t -> 'a Unit.Packed.t

  method unit_content : 'a Unit.content -> 'a Unit.content

  method unit : 'a Unit.t -> 'a Unit.t

  method unit_digest : Digest.t -> Digest.t

  method unit_interface : bool -> bool

  method unit_hidden : bool -> bool

end

class virtual ['a] page : object

  method virtual identifier_page :
    'a Identifier.page -> 'a Identifier.page

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method page : 'a Page.t -> 'a Page.t

  method page_digest : Digest.t -> Digest.t

end

class virtual ['a] types : object
  inherit ['a] documentation
  inherit ['a] module_
  inherit ['a] module_type
  inherit ['a] signature
  inherit ['a] include_
  inherit ['a] type_decl
  inherit ['a] extension
  inherit ['a] exception_
  inherit ['a] value
  inherit ['a] external_
  inherit ['a] class_
  inherit ['a] class_type
  inherit ['a] class_signature
  inherit ['a] method_
  inherit ['a] instance_variable
  inherit ['a] type_expr
  inherit ['a] unit
  inherit ['a] page
end
