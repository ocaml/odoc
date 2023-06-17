(** Parser for ocamldoc formatted comments. *)

module Ast = Ast
module Loc = Loc

(** Warnings produced during parsing. *)
module Warning : sig
  type t = Warning.t = { location : Loc.span; message : string }
  (** Warnings are represented as human-readable text. *)

  val to_string : t -> string
  (** [to_string] will format the location and warning as text to be
      printed. *)
end

type t = { ast : Ast.t; warnings : Warning.t list }
(** [type t] is the result of parsing. *)

val parse_comment : location:Lexing.position -> text:string -> t
(** [parse_comment ~location ~text] parses [text] as an ocamldoc formatted
    string. The parser will try to recover from any invalid syntax encountered,
    and therefore this will always produce a result of some sort with zero or
    more warnings. The location passed in should represent the start of the
    {i content} of the documentation comment - so for a line such as
    {[
      (** A comment starting in the first column (0) *)
    ]}
    the location should represent the space immediately before the [A], so the
    in the 4th column (3) *)
