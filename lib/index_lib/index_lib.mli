val main :
     odoc_directory:string
  -> db_filename:string
  -> optimize:bool
  -> (module Db.Storage.S)
  -> unit
