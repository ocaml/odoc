(**  This library is intended for search engine that do not use the Json output
    but Odoc as a library. Most search engine will use their own representation 
    instead of {!Entry.t}, and may not want to store the whole HTML in their 
    database.
    This library contains functions that are useful for the frontend of such 
    search engines. 
    These functions would have their place in Odoc_searc.html, but putting them
    there means that you need to link to a lot of dependencies to use them, and
    js-of-ocaml is unable to detect when these dependencies are unused. *)

val of_strings :
  kind:string ->
  prefix_name:string option ->
  name:string option ->
  rhs:string option ->
  typedecl_params:string option ->
  doc:string ->
  [> `Code | `Div ] Tyxml_html.elt list
(** [of_string] generates the html of an entry using strings associated to 
    the relevant parts of the entry. If the strings have the correct values,
    it will return the same HTML as {!Odoc_search.Html.of_entry}. Correct values
    are given by {!Odoc_search.Html}, and for kinds, bellow. *)

val kind_doc : string
(** Kinds *)

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
