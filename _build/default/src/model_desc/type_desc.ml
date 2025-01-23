(** Runtime representation of types. It is only useful for inspecting
    values, not types, eg. it is possible to write a generic printer but
    not to deserialize. *)
type 'a t =
  | Record : 'a field list -> 'a t
  | Variant : ('a -> case) -> 'a t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | List : 'a t -> 'a list t
  | Option : 'a t -> 'a option t
  | To_string : ('a -> string) -> 'a t
  | Indirect : ('a -> 'b) * 'b t -> 'a t

and 'a field = F : string * ('a -> 'b) * 'b t -> 'a field

and case = C : string * 'b * 'b t -> case | C0 : string -> case

let bool : bool t = To_string string_of_bool

let string : string t = To_string (fun s -> s)

let int : int t = To_string string_of_int
