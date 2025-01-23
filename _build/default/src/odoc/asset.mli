open Or_error

val compile :
  parent_id:string ->
  name:string ->
  output_dir:string ->
  (unit, [> msg ]) result
