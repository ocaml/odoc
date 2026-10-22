(** {!Here.top_comment}

    {1 Heading {!Here.heading}}

    {{!Here.link_text}text}

    {!modules: Here.module_list}

    {table {tr {td {!Here.table_cell}}}}

    @see <x> {!Here.see_tag}
    @raise Not_found {!Here.raise_tag} *)

type variant =
  | Constructor  (** {!Here.constructor} *)
  | Inline of { field : int  (** {!Here.inline_field} *) }

type record = { field : int  (** {!Here.record_field} *) }

type poly = [ `A  (** {!Here.poly_constructor} *) ]

type ext = ..

type ext += Ext  (** {!Here.extension_constructor} *)

exception Exn of { arg : int  (** {!Here.exception_field} *) }

module type S = sig
  val in_module_type : int  (** {!Here.module_type} *)
end

module F (X : sig
  val param : int  (** {!Here.functor_parameter} *)
end) : sig
  val body : int  (** {!Here.functor_body} *)
end

class c : object
  method m : int  (** {!Here.method_} *)

  val v : int  (** {!Here.instance_variable} *)
end

class type ct = object
  method n : int  (** {!Here.class_type_method} *)
end

include sig
  val included : int  (** {!Here.include_of_signature} *)
end

include sig
  type t

  val with_ : t  (** {!Here.include_of_signature_with} *)
end
with type t := int

module type MS := sig
  val in_subst : int  (** {!Here.module_type_substitution} *)
end

(** {!Here.include_doc} *)
include S

(**/**)

val hidden : int  (** {!Here.after_stop} *)
