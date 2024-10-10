type child = Page of string | Dir of string

type children_order = child Location_.with_location list Location_.with_location

type t = { children_order : children_order option }

val empty : t

val parse : string Location_.with_location -> t

val of_ast_frontmatter :
  Odoc_parser.Ast.frontmatter option -> (t, Error.t) result
