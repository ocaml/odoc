(* HTML renderer configuration *)

type t

val v :
  ?theme_uri:Types.uri ->
  ?support_uri:Types.uri ->
  semantic_uris:bool ->
  indent:bool ->
  flat:bool ->
  open_details:bool ->
  as_json:bool ->
  unit ->
  t

val theme_uri : t -> Types.uri

val support_uri : t -> Types.uri

val semantic_uris : t -> bool

val indent : t -> bool

val flat : t -> bool

val open_details : t -> bool

val as_json : t -> bool
