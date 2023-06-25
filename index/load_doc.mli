val run :
     db:Db.writer
  -> index_docstring:bool
  -> index_name:bool
  -> type_search:bool
  -> index:Odoc_search.Entry.t list
  -> unit
