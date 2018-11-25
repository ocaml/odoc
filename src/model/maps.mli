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

open Paths
open Lang

val option_map : ('a -> 'a) -> 'a option -> 'a option

val list_map : ('a -> 'a) -> 'a list -> 'a list

val pair_map : ('a -> 'a) -> ('b -> 'b) -> ('a * 'b) -> ('a * 'b)

class virtual identifier : object

  method virtual root : Root.t -> Root.t

  method identifier : 'k. 'k Identifier.t -> 'k Identifier.t

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

  method identifier_page : Identifier.page -> Identifier.page

  method identifier_signature : Identifier.signature -> Identifier.signature

  method identifier_class_signature : Identifier.class_signature ->
    Identifier.class_signature

  method identifier_datatype : Identifier.datatype -> Identifier.datatype

  method identifier_module : Identifier.module_ -> Identifier.module_

  method identifier_module_type : Identifier.module_type ->
    Identifier.module_type

  method identifier_type : Identifier.type_ -> Identifier.type_

  method identifier_constructor : Identifier.constructor ->
    Identifier.constructor

  method identifier_field : Identifier.field -> Identifier.field

  method identifier_extension : Identifier.extension -> Identifier.extension

  method identifier_exception : Identifier.exception_ -> Identifier.exception_

  method identifier_value : Identifier.value -> Identifier.value

  method identifier_class : Identifier.class_ -> Identifier.class_

  method identifier_class_type : Identifier.class_type -> Identifier.class_type

  method identifier_method : Identifier.method_ -> Identifier.method_

  method identifier_instance_variable : Identifier.instance_variable ->
    Identifier.instance_variable

  method identifier_label : Identifier.label -> Identifier.label

end

class virtual path : object

  method virtual identifier : 'k. 'k Identifier.t -> 'k Identifier.t

  method path_resolved : 'k. 'k Path.Resolved.t -> 'k Path.Resolved.t

  method path_resolved_module_name : string -> string

  method path_resolved_module_type_name : string -> string

  method path_resolved_type_name : string -> string

  method path_resolved_class_name : string -> string

  method path_resolved_class_type_name : string -> string

  method path_resolved_module : Path.Resolved.module_ -> Path.Resolved.module_

  method path_resolved_module_type : Path.Resolved.module_type ->
    Path.Resolved.module_type

  method path_resolved_type : Path.Resolved.type_ -> Path.Resolved.type_

  method path_resolved_class_type : Path.Resolved.class_type ->
    Path.Resolved.class_type

  method path : 'k . 'k Path.t -> 'k Path.t

  method path_root_name : string -> string

  method path_dot_name : string -> string

  method path_module : Path.module_ -> Path.module_

  method path_module_type : Path.module_type -> Path.module_type

  method path_type : Path.type_ -> Path.type_

  method path_class_type : Path.class_type -> Path.class_type

end

class virtual fragment : object

  method virtual path_resolved : 'k. 'k Path.Resolved.t -> 'k Path.Resolved.t

  method fragment_resolved : 'k 's. ('k, 's) Fragment.Resolved.raw ->
    ('k, 's) Fragment.Resolved.raw

  method fragment_resolved_module_name : string -> string

  method fragment_resolved_type_name : string -> string

  method fragment_resolved_class_name : string -> string

  method fragment_resolved_class_type_name : string -> string

  method fragment_resolved_module : Fragment.Resolved.module_ ->
    Fragment.Resolved.module_

  method fragment_resolved_type : Fragment.Resolved.type_ ->
    Fragment.Resolved.type_

  method fragment : 'k 's. ('k, 's) Fragment.raw -> ('k, 's) Fragment.raw

  method fragment_name : string -> string

  method fragment_module : Fragment.module_ -> Fragment.module_

  method fragment_type : Fragment.type_ -> Fragment.type_

end

class virtual reference : object

  method virtual identifier : 'k. 'k Identifier.t -> 'k Identifier.t

  method virtual path_resolved : 'k. 'k Path.Resolved.t -> 'k Path.Resolved.t

  method reference_resolved : 'k. 'k Reference.Resolved.t ->
    'k Reference.Resolved.t

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

  method reference_resolved_module : Reference.Resolved.module_ ->
    Reference.Resolved.module_

  method reference_resolved_module_type : Reference.Resolved.module_type ->
    Reference.Resolved.module_type

  method reference_resolved_type : Reference.Resolved.type_ ->
    Reference.Resolved.type_

  method reference_resolved_constructor : Reference.Resolved.constructor ->
    Reference.Resolved.constructor

  method reference_resolved_field : Reference.Resolved.field ->
    Reference.Resolved.field

  method reference_resolved_extension : Reference.Resolved.extension ->
    Reference.Resolved.extension

  method reference_resolved_exception : Reference.Resolved.exception_ ->
    Reference.Resolved.exception_

  method reference_resolved_value : Reference.Resolved.value ->
    Reference.Resolved.value

  method reference_resolved_class : Reference.Resolved.class_ ->
    Reference.Resolved.class_

  method reference_resolved_class_type : Reference.Resolved.class_type ->
    Reference.Resolved.class_type

  method reference_resolved_method : Reference.Resolved.method_ ->
    Reference.Resolved.method_

  method reference_resolved_instance_variable :
    Reference.Resolved.instance_variable ->
    Reference.Resolved.instance_variable

  method reference_resolved_label : Reference.Resolved.label ->
    Reference.Resolved.label

  method reference_resolved_any : Reference.Resolved.any ->
    Reference.Resolved.any

  method reference : 'k. 'k Reference.t -> 'k Reference.t

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

  method reference_module : Reference.module_ -> Reference.module_

  method reference_module_type : Reference.module_type -> Reference.module_type

  method reference_type : Reference.type_ -> Reference.type_

  method reference_constructor : Reference.constructor -> Reference.constructor

  method reference_field : Reference.field -> Reference.field

  method reference_extension : Reference.extension -> Reference.extension

  method reference_exception : Reference.exception_ -> Reference.exception_

  method reference_value : Reference.value -> Reference.value

  method reference_class : Reference.class_ -> Reference.class_

  method reference_class_type : Reference.class_type -> Reference.class_type

  method reference_method : Reference.method_ -> Reference.method_

  method reference_instance_variable : Reference.instance_variable ->
    Reference.instance_variable

  method reference_label : Reference.label -> Reference.label

  method reference_any : Reference.any -> Reference.any

end

class virtual paths : object
  inherit identifier
  inherit path
  inherit fragment
  inherit reference
end

class virtual documentation : object

  method virtual identifier_label : Identifier.label -> Identifier.label

  method virtual identifier : 'k. 'k Identifier.t -> 'k Identifier.t

  method virtual path_module : Path.module_ -> Path.module_

  method virtual reference_module : Reference.module_ -> Reference.module_

  method virtual reference_module_type : Reference.module_type ->
    Reference.module_type

  method virtual reference_type : Reference.type_ -> Reference.type_

  method virtual reference_constructor : Reference.constructor ->
    Reference.constructor

  method virtual reference_field : Reference.field -> Reference.field

  method virtual reference_extension : Reference.extension ->
    Reference.extension

  method virtual reference_exception : Reference.exception_ ->
    Reference.exception_

  method virtual reference_value : Reference.value -> Reference.value

  method virtual reference_class : Reference.class_ -> Reference.class_

  method virtual reference_class_type : Reference.class_type ->
    Reference.class_type

  method virtual reference_method : Reference.method_ -> Reference.method_

  method virtual reference_instance_variable : Reference.instance_variable ->
    Reference.instance_variable

  method virtual reference_label : Reference.label -> Reference.label

  method virtual reference_any : Reference.any -> Reference.any

  method documentation_reference :
    Paths.Reference.any * Comment.link_content ->
      Paths.Reference.any * Comment.link_content

  method documentation : Comment.docs -> Comment.docs

  method documentation_comment : Comment.docs_or_stop -> Comment.docs_or_stop

end

class virtual module_ : object

  method virtual identifier_module : Identifier.module_ -> Identifier.module_

  method virtual path_module : Path.module_ -> Path.module_

  method virtual reference_module : Reference.module_ -> Reference.module_

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual module_type_expr : ModuleType.expr -> ModuleType.expr

  method virtual signature : Signature.t -> Signature.t

  method virtual module_type_functor_arg :
    FunctorArgument.t option -> FunctorArgument.t option

  method module_expansion : Module.expansion -> Module.expansion

  method module_decl : Module.decl -> Module.decl

  method module_ : Module.t -> Module.t

  method module_equation : Module.decl -> Module.decl

  method module_hidden : bool -> bool
end

class virtual module_type : object

  method virtual identifier_module : Identifier.module_ -> Identifier.module_

  method virtual identifier_module_type : Identifier.module_type ->
    Identifier.module_type

  method virtual path_module : Path.module_ -> Path.module_

  method virtual path_module_type : Path.module_type -> Path.module_type

  method virtual path_type : Path.type_ -> Path.type_

  method virtual fragment_module : Fragment.module_ -> Fragment.module_

  method virtual fragment_type : Fragment.type_ -> Fragment.type_

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual module_decl : Module.decl -> Module.decl

  method virtual module_equation : Module.Equation.t -> Module.Equation.t

  method virtual signature : Signature.t -> Signature.t

  method virtual type_decl_equation : TypeDecl.Equation.t -> TypeDecl.Equation.t

  method virtual type_decl_param_name : string -> string

  method virtual module_expansion : Module.expansion -> Module.expansion

  method module_type_substitution : ModuleType.substitution ->
    ModuleType.substitution

  method module_type_expr : ModuleType.expr -> ModuleType.expr

  method module_type_functor_arg :
    FunctorArgument.t option -> FunctorArgument.t option

  method module_type : ModuleType.t -> ModuleType.t

end

class virtual signature : object

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual module_ : Module.t -> Module.t

  method virtual module_type : ModuleType.t -> ModuleType.t

  method virtual type_decl : TypeDecl.t -> TypeDecl.t

  method virtual extension : Extension.t -> Extension.t

  method virtual exception_ : Exception.t -> Exception.t

  method virtual value : Value.t -> Value.t

  method virtual external_ : External.t -> External.t

  method virtual class_ : Class.t -> Class.t

  method virtual class_type : ClassType.t -> ClassType.t

  method virtual include_ : Include.t -> Include.t

  method signature_item : Signature.item -> Signature.item

  method signature : Signature.t -> Signature.t

end

class virtual include_ : object

  method virtual module_decl : Module.decl -> Module.decl

  method virtual identifier_signature : Identifier.signature ->
                                        Identifier.signature

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual signature : Signature.t -> Signature.t

  method include_expansion_resolved : bool -> bool

  method include_expansion : Include.expansion -> Include.expansion

  method include_ : Include.t -> Include.t

end

class virtual type_decl : object

  method virtual identifier_type : Identifier.type_ -> Identifier.type_

  method virtual identifier_constructor : Identifier.constructor ->
    Identifier.constructor

  method virtual identifier_field : Identifier.field -> Identifier.field

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method type_decl_constructor : TypeDecl.Constructor.t ->
    TypeDecl.Constructor.t

  method type_decl_constructor_argument : TypeDecl.Constructor.argument ->
    TypeDecl.Constructor.argument

  method type_decl_field : TypeDecl.Field.t -> TypeDecl.Field.t

  method type_decl_field_mutable : bool -> bool

  method type_decl_representation : TypeDecl.Representation.t ->
    TypeDecl.Representation.t

  method type_decl_variance : TypeDecl.variance -> TypeDecl.variance

  method type_decl_param_desc : TypeDecl.param_desc -> TypeDecl.param_desc

  method type_decl_param_name : string -> string

  method type_decl_param : TypeDecl.param -> TypeDecl.param

  method type_decl_equation : TypeDecl.Equation.t -> TypeDecl.Equation.t

  method type_decl_private : bool -> bool

  method type_decl_constraint : TypeExpr.t * TypeExpr.t ->
    TypeExpr.t * TypeExpr.t

  method type_decl : TypeDecl.t -> TypeDecl.t

end

class virtual extension : object

  method virtual identifier_extension : Identifier.extension ->
    Identifier.extension

  method virtual path_type : Path.type_ -> Path.type_

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual type_decl_private : bool -> bool

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method extension_constructor : Extension.Constructor.t ->
    Extension.Constructor.t

  method extension : Extension.t -> Extension.t

end

class virtual exception_ : object

  method virtual identifier_exception : Identifier.exception_ ->
    Identifier.exception_

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method exception_ : Exception.t -> Exception.t

end

class virtual value : object

  method virtual identifier_value : Identifier.value -> Identifier.value

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method value : Value.t -> Value.t
end

class virtual external_ : object

  method virtual identifier_value : Identifier.value -> Identifier.value

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method external_ : External.t -> External.t

  method external_primitive : string -> string

end

class virtual class_ : object

  method virtual identifier_class : Identifier.class_ -> Identifier.class_

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual class_type_expr : ClassType.expr -> ClassType.expr

  method virtual type_expr_label : TypeExpr.label -> TypeExpr.label

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method virtual class_signature : ClassSignature.t -> ClassSignature.t

  method class_decl : Class.decl -> Class.decl

  method class_ : Class.t -> Class.t

  method class_virtual : bool -> bool

end

class virtual class_type : object

  method virtual identifier_class_type : Identifier.class_type ->
    Identifier.class_type

  method virtual path_class_type : Path.class_type -> Path.class_type

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual class_signature : ClassSignature.t -> ClassSignature.t

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method class_type_expr : ClassType.expr -> ClassType.expr

  method class_type : ClassType.t -> ClassType.t

  method class_type_virtual : bool -> bool

end

class virtual class_signature : object

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual class_type_expr : ClassType.expr -> ClassType.expr

  method virtual method_ : Method.t -> Method.t

  method virtual instance_variable : InstanceVariable.t ->
    InstanceVariable.t

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method class_signature_item : ClassSignature.item ->
    ClassSignature.item

  method class_signature : ClassSignature.t -> ClassSignature.t

end

class virtual method_ : object

  method virtual identifier_method : Identifier.method_ ->
    Identifier.method_

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method method_ : Method.t -> Method.t

  method method_private : bool -> bool

  method method_virtual : bool -> bool

end

class virtual instance_variable : object

  method virtual identifier_instance_variable :
    Identifier.instance_variable -> Identifier.instance_variable

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method instance_variable : InstanceVariable.t -> InstanceVariable.t

  method instance_variable_mutable : bool -> bool

  method instance_variable_virtual : bool -> bool

end

class virtual type_expr : object

  method virtual path_module_type : Path.module_type -> Path.module_type

  method virtual path_type : Path.type_ -> Path.type_

  method virtual path_class_type : Path.class_type -> Path.class_type

  method virtual fragment_type : Fragment.type_ -> Fragment.type_

  method virtual documentation : Comment.docs -> Comment.docs

  method type_expr_variant_kind : TypeExpr.Polymorphic_variant.kind ->
    TypeExpr.Polymorphic_variant.kind

  method type_expr_variant_element : TypeExpr.Polymorphic_variant.element ->
    TypeExpr.Polymorphic_variant.element

  method type_expr_variant_constructor_name : string -> string

  method type_expr_variant_constructor_const : bool -> bool

  method type_expr_variant :
    TypeExpr.Polymorphic_variant.t -> TypeExpr.Polymorphic_variant.t

  method type_expr_object_method : TypeExpr.Object.method_ ->
    TypeExpr.Object.method_

  method type_expr_object_method_name : string -> string

  method type_expr_object_field : TypeExpr.Object.field ->
    TypeExpr.Object.field

  method type_expr_object : TypeExpr.Object.t -> TypeExpr.Object.t

  method type_expr_object_open : bool -> bool

  method type_expr_package_substitution : TypeExpr.Package.substitution ->
    TypeExpr.Package.substitution

  method type_expr_package : TypeExpr.Package.t -> TypeExpr.Package.t

  method type_expr_label : TypeExpr.label -> TypeExpr.label

  method type_expr_label_name : string -> string

  method type_expr : TypeExpr.t -> TypeExpr.t

  method type_expr_var_name : string -> string

end

class virtual unit : object

  method virtual root : Root.t -> Root.t

  method virtual identifier_module : Identifier.module_ ->
    Identifier.module_

  method virtual path_module :
    Path.module_ -> Path.module_

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual signature : Signature.t -> Signature.t

  method unit_import :
    Compilation_unit.Import.t -> Compilation_unit.Import.t

  method unit_import_name : string -> string

  method unit_import_digest : Digest.t -> Digest.t

  method unit_source :
    Compilation_unit.Source.t -> Compilation_unit.Source.t

  method unit_source_file : string -> string

  method unit_source_build_dir : string -> string

  method unit_source_digest : Digest.t -> Digest.t

  method unit_packed_item :
    Compilation_unit.Packed.item -> Compilation_unit.Packed.item

  method unit_packed :
    Compilation_unit.Packed.t -> Compilation_unit.Packed.t

  method unit_content :
    Compilation_unit.content -> Compilation_unit.content

  method unit : Compilation_unit.t -> Compilation_unit.t

  method unit_digest : Digest.t -> Digest.t

  method unit_interface : bool -> bool

  method unit_hidden : bool -> bool

end

class virtual page : object

  method virtual identifier_page : Identifier.page -> Identifier.page

  method virtual documentation : Comment.docs -> Comment.docs

  method page : Page.t -> Page.t

  method page_digest : Digest.t -> Digest.t

end

class virtual types : object
  inherit documentation
  inherit module_
  inherit module_type
  inherit signature
  inherit include_
  inherit type_decl
  inherit extension
  inherit exception_
  inherit value
  inherit external_
  inherit class_
  inherit class_type
  inherit class_signature
  inherit method_
  inherit instance_variable
  inherit type_expr
  inherit unit
  inherit page
end
