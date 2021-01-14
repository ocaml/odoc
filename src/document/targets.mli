(** Collect all the urls of pages defined by a model. 
    
    Roughly a simplified version of the normal process
    to convert a model into a document, only for extracting Urls.
    Used to determine the build targets.
*)

open Odoc_model.Lang

val unit : Compilation_unit.t -> Url.Path.t list

val page : Page.t -> Url.Path.t list
