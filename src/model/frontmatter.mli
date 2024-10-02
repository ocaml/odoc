type child = Page of string | Dir of string

type line = Children_order of child list | KV of string * string | V of string

type t = { children_order : child list option }

val parse : string -> t
