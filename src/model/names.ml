module type Name = sig

    type t

    val to_string : t -> string

    val of_string : string -> t

    val of_ident : Ident.t -> t

    val equal : t -> t -> bool

    val is_hidden : t -> bool

end

module Name : Name = struct

    type t = string

    let to_string s = s

    let of_string s = s
    
    let of_ident id = of_string (Ident.name id)

    let equal (x : t) (y : t) = x = y

    let is_hidden s =
        let len = String.length s in
        let rec aux i =
            if i > len - 2 then false else
            if s.[i] = '_' && s.[i + 1] = '_' then true
            else aux (i + 1)
        in aux 0
end

module ModuleName = Name

module ArgumentName = Name

module ModuleTypeName = Name

module TypeName = Name

module ConstructorName = Name

module FieldName = Name

module ExtensionName = Name

module ExceptionName = Name

module ValueName = Name

module ClassName = Name

module ClassTypeName = Name

module MethodName = Name

module InstanceVariableName = Name

module UnitName = Name

module LabelName = Name

module PageName = Name


