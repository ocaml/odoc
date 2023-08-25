open Odoc_model

module Html = Tyxml_html

val title_of_id : Paths.Identifier.t -> string * string
val html_of_doc : Comment.docs -> [> Html_types.div ] Tyxml_html.elt
val html_string_of_doc : Comment.docs -> string

val html_of_entry : Entry.t -> [> `Code | `Div ] Tyxml_html.elt list

val with_html : Entry.t -> Entry.with_html

(** Right-hand sides *)

val type_expr : ?needs_parentheses:bool -> Lang.TypeExpr.t -> string

val constructor_rhs : Entry.constructor_entry -> string
val field_rhs : Entry.field_entry -> string

val typedecl_rhs : Entry.type_decl_entry -> string option

val value_rhs : Entry.value_entry -> string
val html_of_strings :
  kind:string ->
  prefix_name:string option ->
  name:string option ->
  rhs:string option ->
  typedecl_params:string option ->
  doc:string ->
  [> `Code | `Div ] Tyxml_html.elt list

val rhs_of_kind : Entry.kind -> string option

(** Kinds *)

val string_of_kind : Entry.kind -> string
(** Does not include the rhs. *)

val kind_doc : string
val kind_typedecl : string
val kind_module : string
val kind_exception : string
val kind_class_type : string
val kind_class : string
val kind_method : string
val kind_extension_constructor : string
val kind_module_type : string
val kind_constructor : string
val kind_field : string
val kind_value : string
val kind_extension : string
