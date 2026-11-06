(** Finding the source file of a compiled module with [ocamlobjinfo]. *)

val get_source : Fpath.t -> Fpath.t list -> Fpath.t option
(** [get_source cmt dirs] reads the source file name recorded in [cmt] and looks
    for it in [dirs]. It also tries the names a preprocessed or generated file
    would have had. *)
