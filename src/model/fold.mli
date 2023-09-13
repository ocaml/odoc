(** This module allows to fold over odoc values. It is notably used to construct
    a search database of every relevant item. It appear to be very generic but
    in reality it is quite specialized to fold over searchable items, and not
    every kind of odoc value you could fold over.*)

open Lang

(** The type of items you can fold over *)
type item =
  | CompilationUnit of Compilation_unit.t
  | TypeDecl of TypeDecl.t
  | Module of Module.t
  | Value of Value.t
  | Exception of Exception.t
  | ClassType of ClassType.t
  | Method of Method.t
  | Class of Class.t
  | Extension of Extension.t
  | ModuleType of ModuleType.t
  | Doc of Comment.docs_or_stop

(** Bellow are the folding functions. For items that may contain
    others, such as [signature], it folds recursively on the
    sub-items. It does not recurse into internal items. *)

val unit : f:('a -> item -> 'a) -> 'a -> Compilation_unit.t -> 'a
val page : f:('a -> item -> 'a) -> 'a -> Page.t -> 'a

val signature : f:('a -> item -> 'a) -> 'a -> Signature.t -> 'a
val signature_item : f:('a -> item -> 'a) -> 'a -> Signature.item -> 'a
val docs : f:('a -> item -> 'a) -> 'a -> Comment.docs_or_stop -> 'a
val include_ : f:('a -> item -> 'a) -> 'a -> Include.t -> 'a
val class_type : f:('a -> item -> 'a) -> 'a -> ClassType.t -> 'a
val class_signature : f:('a -> item -> 'a) -> 'a -> ClassSignature.t -> 'a
val class_signature_item :
  f:('a -> item -> 'a) -> 'a -> ClassSignature.item -> 'a
val class_ : f:('a -> item -> 'a) -> 'a -> Class.t -> 'a
val exception_ : f:('a -> item -> 'a) -> 'a -> Exception.t -> 'a
val type_extension : f:('a -> item -> 'a) -> 'a -> Extension.t -> 'a
val value : f:('a -> item -> 'a) -> 'a -> Value.t -> 'a
val module_ : f:('a -> item -> 'a) -> 'a -> Module.t -> 'a
val type_decl : f:('a -> item -> 'a) -> 'a -> TypeDecl.t -> 'a
val module_type : f:('a -> item -> 'a) -> 'a -> ModuleType.t -> 'a
val simple_expansion :
  f:('a -> item -> 'a) -> 'a -> ModuleType.simple_expansion -> 'a
val module_type_expr : f:('a -> item -> 'a) -> 'a -> ModuleType.expr -> 'a
val functor_parameter : f:('a -> item -> 'a) -> 'a -> FunctorParameter.t -> 'a
