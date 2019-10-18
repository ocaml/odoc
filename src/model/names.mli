module type Name = sig

    type t

    val to_string : t -> string

    val to_string_unsafe : t -> string

    val of_string : string -> t

    val of_ident : Ident.t -> t

    val internal_of_string : string -> t

    val internal_of_ident : Ident.t -> t

    val is_internal : t -> bool

    val equal : t -> t -> bool

    val is_hidden : t -> bool
end

module type SimpleName = sig

    type t

    val to_string : t -> string

    val of_string : string -> t

    val of_ident : Ident.t -> t

    val equal : t -> t -> bool

    val is_hidden : t -> bool

end

module ModuleName : Name

module ArgumentName : Name

module ModuleTypeName : Name

module TypeName : Name

module ConstructorName : SimpleName

module FieldName : SimpleName

module ExtensionName : SimpleName

module ExceptionName : SimpleName

module ValueName : SimpleName

module ClassName : Name

module ClassTypeName : Name

module MethodName : SimpleName

module InstanceVariableName : SimpleName

module UnitName : SimpleName

module LabelName : SimpleName

module PageName : SimpleName
