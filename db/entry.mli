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
  type t =
    { name : string
    ; version : string
    }

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
  ; is_from_module_type : bool
  }

val v
  :  name:string
  -> kind:Kind.t
  -> cost:int
  -> rhs:string option
  -> doc_html:string
  -> url:string
  -> is_from_module_type:bool
  -> pkg:Package.t
  -> unit
  -> t

val link : t -> string
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
