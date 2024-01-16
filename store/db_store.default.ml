type db_format =
  [ `ancient
  | `marshal
  | `js
  ]

let available_backends = [ "marshal", `marshal; "js", `js ]

let storage_module = function
  | `marshal -> (module Storage_marshal : Db.Storage.S)
  | `js -> (module Storage_js : Db.Storage.S)
  | `ancient -> failwith "ancient is unsupported"
