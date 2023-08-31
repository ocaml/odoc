open Odoc_model

type html = Html_types.div_content Tyxml.Html.elt

val of_entry : Entry.t -> html list

val url : Odoc_model.Paths.Identifier.Any.t -> string

(** The below is intended for search engine that do not use the Json output but
    Odoc as a library. Most search engine will use their own representation 
    instead of {!Entry.t}, and may not want to store the whole HTML in their 
    database. The following functions help rebuild the HTML from any 
    represention. *)

val of_strings :
  kind:string ->
  prefix_name:string option ->
  name:string option ->
  rhs:string option ->
  typedecl_params:string option ->
  doc:string ->
  html list
(** [of_string] generates the html of an entry using strings associated to 
    the relevant parts of the entry. If the strings have the correct values,
    it will return the same as {!of_entry} *)

val names_of_id : Paths.Identifier.t -> string * string
(** [names_of_id id] is [("X.Y", "foo")] if [id] corresponds to [X.Y.foo]. 
    The tuple is intended to be given respectively to the [prefix_name] and 
    [name] arguments of {!html_of_strings}. *)

val of_doc : Comment.docs -> html
(** [of_doc d] returns the HTML associated of the documentation comment [d],
    generated correctly for search (no links or anchors). *)

val html_string_of_doc : Comment.docs -> string
(** [html_string_of_doc d] is the same as {!of_doc} converted to a 
    string. *)

(** Right-hand sides *)

val rhs_of_kind : Entry.kind -> string option
(** [rhs_of_kind k] is the right-hand-side string associated with the metadata 
    included in the kind [k]. If [k] is [Value _], it may be [": int"] *)

val typedecl_params_of_entry : Entry.t -> string option
(** [typedecl_params_of_entry e] is [Some "'a"] if the entry correspond to
    ['a t]. If the entry is not a typedecl, or if the typedecl does not have a
    type parameter, then it returns [None]. *)

(** Kinds *)

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
