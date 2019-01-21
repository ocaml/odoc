module type Name = sig

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

module ConstructorName : Name

module FieldName : Name

module ExtensionName : Name

module ExceptionName : Name

module ValueName : Name

module ClassName : Name

module ClassTypeName : Name

module MethodName : Name

module InstanceVariableName : Name

module UnitName : Name

module LabelName : Name

module PageName : Name
