(** [ocamlmark] support. *)

(** {1:parsing ocamlmark parsing} *)

val parse_comment :
  ?buffer:Buffer.t ->
  location:Lexing.position ->
  text:string ->
  unit ->
  Odoc_parser.Ast.t * Odoc_parser.Warning.t list
(** [parse_comment ~location ~text] parses the ocamlmark [text] assuming it
    corresponds to [location]. [buffer] is used as a scratch buffer. *)
