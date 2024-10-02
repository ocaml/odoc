type child = Page of string | Dir of string

type t = { children_order : child list option }

val parse : string -> t
