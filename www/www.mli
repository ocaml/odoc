type db_format :=
  [ `ancient
  | `marshal
  | `js
  ]

val term : (db_format -> string -> unit) Cmdliner.Term.t
