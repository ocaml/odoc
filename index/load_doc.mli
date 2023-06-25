val register_entry :
     db:Db.writer
  -> index_name:bool
  -> type_search:bool
  -> index_docstring:bool
  -> Odoc_search.Entry.t
  -> unit
