let arg_enum = [ "ancient", `ancient ]

let storage_module = function
  | `ancient -> (module Storage_ancient : Db.Storage.S)
  | `marshal -> (module Storage_marshal : Db.Storage.S)
  | `js -> (module Storage_js : Db.Storage.S)
