(* Find *)

type class_type = [ `C of Component.Class.t | `CT of Component.ClassType.t ]

type type_ =
  [ `C of Component.Class.t
  | `CT of Component.ClassType.t
  | `T of Component.TypeDecl.t ]

type value = [ `E of Component.External.t | `V of Component.Value.t ]

type ('a, 'b) found = Found of 'a | Replaced of 'b

val careful_module_in_sig :
  Component.Signature.t ->
  string ->
  (Component.Module.t, Cpath.Resolved.module_) found option

val careful_type_in_sig :
  Component.Signature.t ->
  string ->
  ( [> `C of Component.Class.t
    | `CT of Component.ClassType.t
    | `T of Component.TypeDecl.t ],
    Component.TypeExpr.t )
  found
  option

val typename_of_typeid : [< `LCoreType of 'a | `LType of 'a * 'b ] -> 'a

val datatype_in_sig :
  Component.Signature.t -> string -> Component.TypeDecl.t option

val any_in_type :
  Component.TypeDecl.t ->
  string ->
  [> `Constructor of Component.TypeDecl.Constructor.t
  | `Field of Component.TypeDecl.Field.t ]
  option

val any_in_typext :
  Component.Extension.t ->
  string ->
  [> `ExtConstructor of
     Component.Extension.t * Component.Extension.Constructor.t ]
  option

val any_in_comment :
  [> `Heading of 'a * Ident.label * 'b ] Odoc_model.Location_.with_location list ->
  string ->
  [> `Label of Ident.label ] option

val any_in_sig :
  Component.Signature.t ->
  string ->
  [> `Class of Ident.class_ * Component.Signature.recursive * Component.Class.t
  | `ClassType of
    Ident.class_type * Component.Signature.recursive * Component.ClassType.t
  | `Constructor of
    Odoc_model.Names.TypeName.t
    * Component.TypeDecl.t
    * Component.TypeDecl.Constructor.t
  | `Exception of Ident.exception_ * Component.Exception.t
  | `ExtConstructor of Component.Extension.t * Component.Extension.Constructor.t
  | `External of Ident.value * Component.External.t
  | `Field of
    Odoc_model.Names.TypeName.t
    * Component.TypeDecl.t
    * Component.TypeDecl.Field.t
  | `Label of Ident.label
  | `Module of
    Ident.module_
    * Component.Signature.recursive
    * Component.Module.t Component.Delayed.t
  | `ModuleSubstitution of Ident.module_ * Component.ModuleSubstitution.t
  | `ModuleType of
    Ident.module_type * Component.ModuleType.t Component.Delayed.t
  | `Removed of
    [> `Module of Ident.module_ * Cpath.Resolved.module_
    | `Type of Ident.type_ * Component.TypeExpr.t ]
  | `Type of
    Ident.type_
    * Component.Signature.recursive
    * Component.TypeDecl.t Component.Delayed.t
  | `TypeSubstitution of Ident.type_ * Component.TypeDecl.t
  | `Value of Ident.value * Component.Value.t ]
  option

val signature_in_sig :
  Component.Signature.t ->
  string ->
  [> `Module of
     Ident.module_
     * Component.Signature.recursive
     * Component.Module.t Component.Delayed.t
  | `ModuleType of
    Ident.module_type * Component.ModuleType.t Component.Delayed.t ]
  option

val module_in_sig : Component.Signature.t -> string -> Component.Module.t option

val module_type_in_sig :
  Component.Signature.t -> string -> Component.ModuleType.t option

val opt_module_type_in_sig :
  Component.Signature.t -> string -> Component.ModuleType.t option option

val opt_value_in_sig : Component.Signature.t -> string -> value option

val type_in_sig :
  Component.Signature.t ->
  string ->
  [> `C of Component.Class.t
  | `CT of Component.ClassType.t
  | `T of Component.TypeDecl.t ]
  option

val class_type_in_sig :
  Component.Signature.t ->
  string ->
  [> `C of Component.Class.t | `CT of Component.ClassType.t ] option

val opt_label_in_sig : Component.Signature.t -> string -> Ident.label option

val find_in_sig :
  Component.Signature.t -> (Component.Signature.item -> 'a option) -> 'a option

val exception_in_sig :
  Component.Signature.t -> string -> Component.Exception.t option

val extension_in_sig :
  Component.Signature.t -> string -> Component.Extension.Constructor.t option

val label_parent_in_sig :
  Component.Signature.t ->
  string ->
  [> `C of Component.Class.t
  | `CT of Component.ClassType.t
  | `M of Component.Module.t
  | `MT of Component.ModuleType.t
  | `T of Component.TypeDecl.t ]
  option

val any_in_type_in_sig :
  Component.Signature.t ->
  string ->
  ( Odoc_model.Names.TypeName.t
  * [> `Constructor of Component.TypeDecl.Constructor.t
    | `Field of Component.TypeDecl.Field.t ] )
  option

val find_in_class_signature :
  Component.ClassSignature.t ->
  (Component.ClassSignature.item -> 'a option) ->
  'a option

val any_in_class_signature :
  Component.ClassSignature.t ->
  string ->
  [> `InstanceVariable of Component.InstanceVariable.t
  | `Method of Component.Method.t ]
  option

val method_in_class_signature :
  Component.ClassSignature.t -> string -> Component.Method.t option

val instance_variable_in_class_signature :
  Component.ClassSignature.t ->
  string ->
  [> `InstanceVariable of Component.InstanceVariable.t ] option
