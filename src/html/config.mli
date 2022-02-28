(* HTML renderer configuration *)

type t

val v :
  ?theme_uri:Types.uri ->
  ?support_uri:Types.uri ->
  semantic_uris:bool ->
  indent:bool ->
  flat:bool ->
  open_details:bool ->
  omit_breadcrumbs:bool ->
  omit_toc:bool ->
  content_only:bool ->
  unit ->
  t

val theme_uri : t -> Types.uri

val support_uri : t -> Types.uri

val semantic_uris : t -> bool

val indent : t -> bool

val flat : t -> bool

val open_details : t -> bool

val omit_breadcrumbs : t -> bool

val omit_toc : t -> bool

val content_only : t -> bool
