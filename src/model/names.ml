open Odoc_utils

(** Returns [true] on chars that are part of operators. *)
let operator_char = function
  (* https://ocaml.org/manual/5.2/lex.html#core-operator-char *)
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' | '~' | '!'
  | '?' | '%' | '<' | ':' | '.'
  (* https://ocaml.org/manual/5.2/lex.html#infix-symbol *)
  | '#'
  (* https://ocaml.org/manual/5.2/indexops.html#s:index-operators *)
  | '(' | ')' | '[' | ']' | '{' | '}' ->
      true
  | _ -> false

let is_operator = function
  | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" | "or" -> true
  | name -> String.exists operator_char name

let parenthesise name = if is_operator name then "(" ^ name ^ ")" else name

let contains_double_underscore s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false
    else if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

module type Name = sig
  type t

  val to_string : t -> string

  val to_string_unsafe : t -> string

  val make_std : string -> t

  val of_ident : Ident.t -> t

  val hidden_of_string : string -> t

  val hidden_of_ident : Ident.t -> t

  val shadowed_of_string : string -> t

  val shadowed_of_ident : Ident.t -> t

  val equal_modulo_shadowing : t -> t -> bool

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val fmt : Format.formatter -> t -> unit

  val is_hidden : t -> bool
end

let internal_counter = ref 0

let unique_id = ref None

let set_unique_ident id =
  match !unique_id with
  | Some _ -> failwith "Unique id already set"
  | None -> unique_id := Some id

module Name : Name = struct
  type t =
    | Hidden of string
    | Shadowed of string * int * string
    | Std of string

  let to_string = function
    | Std s -> parenthesise s
    | Hidden s -> Printf.sprintf "%s" s
    | Shadowed (s, i, s2) -> Printf.sprintf "{%s}%d/shadowed/(%s)" s i s2

  let to_string_unsafe = function
    | Std s -> s
    | Hidden s -> s
    | Shadowed (s, _i, _s2) -> s

  let make_std s = Std s

  let of_ident id = make_std (Ident.name id)

  let hidden_of_string id = Hidden id

  let hidden_of_ident id = hidden_of_string (Ident.name id)

  let shadowed_of_string id =
    incr internal_counter;
    match !unique_id with
    | None -> failwith "Unset unique id"
    | Some s -> Shadowed (id, !internal_counter, s)

  let shadowed_of_ident id = shadowed_of_string (Ident.name id)

  let equal_modulo_shadowing (x : t) (y : t) =
    match (x, y) with
    | Std x, Std y -> x = y
    | Hidden x, Std y -> x = y
    | Std x, Hidden y -> x = y
    | Hidden x, Hidden y -> x = y
    | Shadowed (x, i, s), Shadowed (y, j, t) -> x = y && i = j && s = t
    | _, _ -> false

  let equal (x : t) (y : t) = x = y

  let compare = compare

  let fmt ppf x = Format.fprintf ppf "%s" (to_string x)

  let is_hidden = function
    | Std _ -> false
    | Hidden _ -> true
    | Shadowed _ -> true
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

  let is_hidden s = contains_double_underscore s
end

module ModuleName = struct
  include Name
  let is_hidden s = is_hidden s || contains_double_underscore (to_string s)
end

module ModuleTypeName = Name
module TypeName = Name
module ConstructorName = SimpleName
module FieldName = SimpleName
module UnboxedFieldName = SimpleName
module ExtensionName = SimpleName
module ExceptionName = SimpleName
module ValueName = Name
module MethodName = SimpleName
module InstanceVariableName = SimpleName
module LabelName = SimpleName
module PageName = SimpleName
module DefName = SimpleName
module LocalName = SimpleName
module AssetName = SimpleName
