val register_entry
  :  db:Db.writer
  -> index_name:bool
  -> type_search:bool
  -> index_docstring:bool
  -> pkg:Db.Entry.Package.t
  -> Odoc_search.Entry.t
  -> unit
(** [register_entry ~db ~index_name ~type_search ~index_docstring e] register
    the entry [e] in [db]. *)
