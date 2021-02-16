(** Text attached to references are written as single words to have a more
    readable output.
    References from the first comment (which is the doc of the entire module):
    {{!B.C}Doc-relative}
    {{!A.B.C}Doc-absolute} *)

(** References from inside the module's signature:
    {{!B.C}Defined-below}
    {{!A.B.C}Defined-below-but-absolute} *)

module B : sig
  module C : sig end
end

module D : sig
  open B

  (** {{!C}Through-open} *)
end
