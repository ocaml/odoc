module Kind : sig
  type t =
    | Doc
    | Module
    | Module_type
    | Class
    | Class_type
    | Method
    | Val of Typexpr.t
    | Type_decl of string option
    | Type_extension
    | Extension_constructor of Typexpr.t
    | Exception of Typexpr.t
    | Constructor of Typexpr.t
    | Field of Typexpr.t

  val equal : t -> t -> bool
  val get_type : t -> Typexpr.t option
end

module Package : sig
  type t = private
    { name : string
    ; version : string
    }

  val v : name:string -> version:string -> t
  val link : t -> string
end

type t =
  { name : string
  ; rhs : string option
  ; url : string
  ; kind : Kind.t
  ; cost : int
  ; doc_html : string
  ; pkg : Package.t
  }

val v
  :  name:string
  -> kind:Kind.t
  -> cost:int
  -> rhs:string option
  -> doc_html:string
  -> url:string
  -> pkg:Package.t
  -> unit
  -> t

val link : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool

module Set : Set.S with type elt = t

module Array : sig
  type elt = t
  type t = elt array option

  val of_list : elt list -> t
  val is_empty : t -> bool
  val minimum : t -> elt option
  val equal_elt : elt -> elt -> bool
  val compare_elt : elt -> elt -> int
end
