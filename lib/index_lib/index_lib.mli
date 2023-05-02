val main :
     index:Odoc_search.Index_db.index
  -> db_filename:string
  -> optimize:bool
  -> (module Db.Storage.S)
  -> unit
