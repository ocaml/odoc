let parenthesise name =
  match name with
  | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" -> "(" ^ name ^ ")"
  | _ ->
      if String.length name > 0 then
        match name.[0] with
        | 'a' .. 'z'
        | '\223' .. '\246'
        | '\248' .. '\255'
        | '_'
        | 'A' .. 'Z'
        | '\192' .. '\214'
        | '\216' .. '\222' ->
            name
        | _ -> "(" ^ name ^ ")"
      else name

module type Name = sig
  type t

  val to_string : t -> string

  val to_string_unsafe : t -> string

  val make_std : string -> t

  val of_ident : Ident.t -> t

  val internal_of_string : string -> t

  val internal_of_ident : Ident.t -> t

  val is_internal : t -> bool

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val fmt : Format.formatter -> t -> unit

  val is_hidden : t -> bool
end

let internal_counter = ref 0

module Name : Name = struct
  type t = Internal of string * int | Std of string

  let to_string = function
    | Std s -> s
    | Internal (s, i) -> Printf.sprintf "{%s}%d" s i

  let to_string_unsafe = function Std s -> s | Internal (s, _i) -> s

  let make_std s =
    let s = parenthesise s in
    Std s

  let of_ident id = make_std (Ident.name id)

  let internal_of_string id =
    incr internal_counter;
    Internal (id, !internal_counter)

  let internal_of_ident id = internal_of_string (Ident.name id)

  let is_internal = function Std _ -> false | Internal _ -> true

  let equal (x : t) (y : t) = x = y

  let compare = compare

  let fmt ppf x = Format.fprintf ppf "%s" (to_string x)

  let is_hidden = function
    | Std s ->
        let len = String.length s in
        let rec aux i =
          if i > len - 2 then false
          else if s.[i] = '_' && s.[i + 1] = '_' then true
          else aux (i + 1)
        in
        aux 0
    | Internal _ -> true
end

module type SimpleName = sig
  type t

  val to_string : t -> string

  val make_std : string -> t

  val of_ident : Ident.t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val fmt : Format.formatter -> t -> unit

  val is_hidden : t -> bool
end

module SimpleName : SimpleName = struct
  type t = string

  let to_string s = s

  let make_std s = s

  let of_ident id = make_std (Ident.name id)

  let equal (x : t) (y : t) = x = y

  let compare x y = String.compare x y

  let fmt ppf t = Format.pp_print_string ppf (to_string t)

  let is_hidden s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then false
      else if s.[i] = '_' && s.[i + 1] = '_' then true
      else aux (i + 1)
    in
    aux 0
end

module ModuleName = Name
module ModuleTypeName = Name
module TypeName = Name
module ConstructorName = SimpleName
module FieldName = SimpleName
module ExtensionName = SimpleName
module ExceptionName = SimpleName
module ValueName = Name
module ClassName = Name
module ClassTypeName = Name
module MethodName = SimpleName
module InstanceVariableName = SimpleName
module LabelName = SimpleName
module PageName = SimpleName
module DefName = SimpleName
module LocalName = SimpleName
