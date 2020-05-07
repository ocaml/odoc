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

    val compare : t -> t -> int

    val fmt : Format.formatter -> t -> unit

    val is_hidden : t -> bool
end

module Name : Name = struct

    type t =
        | Internal of string
        | Std of string

    let to_string = function
        | Std s -> s
        | Internal s -> Printf.sprintf "$%s" s

    let to_string_unsafe = function
        | Std s -> s
        | Internal s -> s

    let of_string s =
      if String.length s > 0 && s.[0] = '$' then
        Internal (String.sub s 1 (String.length s - 1))
      else Std s

    let of_ident id = of_string (Ident.name id)

    let internal_of_string id = Internal id

    let internal_of_ident id = internal_of_string (Ident.name id)

    let is_internal = function | Std _ -> false | Internal _ -> true

    let equal (x : t) (y : t) = x = y

    let compare x y =
        match x, y with
        | Internal x, Internal y -> String.compare x y
        | Std x, Std y -> String.compare x y
        | Internal _, Std _ -> -1
        | Std _, Internal _ -> 1

    let fmt ppf x = Format.fprintf ppf "%s" (to_string x)

    let is_hidden = function
        | Std s ->
            let len = String.length s in
            let rec aux i =
                if i > len - 2 then false else
                if s.[i] = '_' && s.[i + 1] = '_' then true
                else aux (i + 1)
            in aux 0
        | Internal _ -> true
end

module type SimpleName = sig

    type t

    val to_string : t -> string

    val of_string : string -> t

    val of_ident : Ident.t -> t

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val fmt : Format.formatter -> t -> unit

    val is_hidden : t -> bool

end

module SimpleName : SimpleName = struct

    type t = string

    let to_string s = s

    let of_string s = s

    let of_ident id = of_string (Ident.name id)

    let equal (x : t) (y : t) = x = y

    let compare x y = String.compare x y

    let fmt ppf t = Format.pp_print_string ppf (to_string t)

    let is_hidden s =
        let len = String.length s in
        let rec aux i =
            if i > len - 2 then false else
            if s.[i] = '_' && s.[i + 1] = '_' then true
            else aux (i + 1)
        in aux 0
end

module ModuleName = Name

module ParameterName = Name

module ModuleTypeName = Name

module TypeName = Name

module ConstructorName = SimpleName

module FieldName = SimpleName

module ExtensionName = SimpleName

module ExceptionName = SimpleName

module ValueName = SimpleName

module ClassName = Name

module ClassTypeName = Name

module MethodName = SimpleName

module InstanceVariableName = SimpleName

module UnitName = SimpleName

module LabelName = SimpleName

module PageName = SimpleName


