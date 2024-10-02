type child = Page of string | Dir of string

type t = { children_order : child list Location_.with_location option }

val empty : t

val parse : string Location_.with_location -> t
