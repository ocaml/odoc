val main :
     index_docstring:bool
  -> index_name:bool
  -> type_search:bool
  -> index:Odoc_search.Entry.t list
  -> db_filename:string
  -> (module Db.Storage.S)
  -> unit
