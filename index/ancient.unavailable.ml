let arg_enum = []

let storage_module = function
  | `marshal -> (module Storage_marshal : Db.Storage.S)
  | `js -> (module Storage_js : Db.Storage.S)
