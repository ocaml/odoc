open Html5.M

val keyword : string -> [> Html5_types.span ] elt

val def_div :
  [< Html5_types.div_content_fun ] elt list -> [> Html5_types.div ] elt

val def_summary :
  [< Html5_types.span_content_fun ] elt list -> [> Html5_types.summary ] elt

val anchor_region_div :
  id:string ->
  [< Html5_types.div_content_fun > `A ] elt list ->
  [> Html5_types.div ] elt
