module Kind : sig
  type 'a abstract =
    | Doc
    | TypeDecl of string option
    | Module
    | Exception of 'a
    | Class_type
    | Method
    | Class
    | TypeExtension
    | ExtensionConstructor of 'a
    | ModuleType
    | Constructor of 'a
    | Field of 'a
    | Val of 'a

  type t = Typexpr.t abstract

  val equal : 'a -> 'a -> bool
  val doc : 'a abstract
  val type_decl : string option -> 'a abstract
  val module_ : 'a abstract
  val exception_ : 'a -> 'a abstract
  val class_type : 'a abstract
  val method_ : 'a abstract
  val class_ : 'a abstract
  val type_extension : 'a abstract
  val extension_constructor : 'a -> 'a abstract
  val module_type : 'a abstract
  val constructor : 'a -> 'a abstract
  val field : 'a -> 'a abstract
  val val_ : 'a -> 'a abstract
end

module Package : sig
  type t =
    { name : string
    ; version : string
    }

  val v : name:string -> version:string -> t
end

type t =
  { name : string
  ; rhs : string option
  ; url : string
  ; kind : Kind.t
  ; cost : int
  ; doc_html : string
  ; pkg : Package.t option
  ; is_from_module_type : bool
  }

val compare : t -> t -> int
val equal : t -> t -> bool

module Set : Set.S with type elt = t

module Array : sig
  type elt = t
  type nonrec t = t array

  val is_empty : t -> bool
  val of_list : elt list -> t
  val equal_elt : elt -> elt -> bool
end

val pkg_link : t -> string option
val link : t -> string option

val v :
     name:string
  -> kind:Kind.t
  -> cost:int
  -> rhs:string option
  -> doc_html:string
  -> url:string
  -> is_from_module_type:bool
  -> ?pkg:Package.t option
  -> unit
  -> t
