val register_entry
  :  db:Db_writer.t
  -> index_name:bool
  -> type_search:bool
  -> index_docstring:bool
  -> favourite:bool
  -> favoured_prefixes:string list
  -> pkg:Db.Entry.Package.t
  -> Odoc_index.Entry.t
  -> unit
(** [register_entry ~db ~index_name ~type_search ~index_docstring e] register
    the entry [e] in [db]. *)
