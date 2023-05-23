val main :
     index:Odoc_search.Entry.t list
  -> db_filename:string
  -> optimize:bool
  -> (module Db.Storage.S)
  -> unit
