exception Parse_error of string

let error str = raise (Parse_error str)
let errorf a = Format.kasprintf error a

open Type_lexer
open Db.Typexpr

type a = int * float -> string

let pp_token f t =
  Format.pp_print_string f
    (match t with
    | ARROW -> "ARROW"
    | PARENS_OPEN -> "PARENS_OPEN"
    | PARENS_CLOSE -> "PARENS_CLOSE"
    | COMMA -> "COMMA"
    | ANY -> "ANY"
    | STAR -> "STAR"
    | POLY p -> Printf.sprintf "POLY %s" p
    | WORD w -> Printf.sprintf "WORD %s" w
    | EOF -> "EOF")

module Tokens_gen = struct
  type t =
    { lexer : Lexing.lexbuf -> token
    ; lexbuf : Lexing.lexbuf
    ; mutable peeked : token option
    }

  let get ({ lexer; lexbuf; peeked } as t) =
    match peeked with
    | None -> lexer lexbuf
    | Some token ->
        t.peeked <- None ;
        token

  let discard t = ignore (get t)

  let peek t =
    match t.peeked with
    | Some token -> token
    | None ->
        let token = get t in
        t.peeked <- Some token ;
        token
end

open Tokens_gen

let rec typ tokens =
  match peek tokens with
  | EOF -> any
  | ARROW ->
      discard tokens ;
      typ tokens
  | _ -> (
      let typ_left = typ2 tokens in
      match peek tokens with
      | ARROW ->
          discard tokens ;
          let typ_right = typ tokens in
          arrow typ_left typ_right
      | EOF -> typ_left
      | PARENS_CLOSE | COMMA -> typ_left
      | _ -> error "typ")

and typ2 tokens =
  let typ = typ1 tokens in
  let tups = tups tokens in
  match tups with
  | [] -> typ
  | _ :: _ -> tuple (typ :: tups)

and tups tokens =
  match peek tokens with
  | STAR ->
      discard tokens ;
      let typ = typ1 tokens in
      typ :: tups tokens
  | _ -> []

and parens tokens =
  let typ = typ tokens in
  match get tokens with
  | COMMA -> (
      let params = typ :: params tokens in
      match get tokens with
      | WORD w -> constr w params
      | _ -> error "parens 1")
  | PARENS_CLOSE -> typ
  | EOF -> typ
  | _ -> error "parens 2"

and params tokens =
  let typ = typ tokens in
  match get tokens with
  | COMMA -> typ :: params tokens
  | PARENS_CLOSE -> [ typ ]
  | tok -> errorf "params %a" pp_token tok

and constr_one_param tokens =
  match peek tokens with
  | WORD w ->
      discard tokens ;
      fun typ -> constr_one_param tokens @@ constr w [ typ ]
  | _ -> Fun.id

and typ1 tokens =
  match peek tokens with
  | EOF -> any
  | _ ->
      let typ = typ0 tokens in
      constr_one_param tokens typ

and typ0 tokens =
  match get tokens with
  | ANY -> any
  | POLY w -> poly w
  | WORD w -> constr w []
  | PARENS_OPEN -> parens tokens
  | _ -> error "typ0"

let of_string str =
  let tokens =
    { lexbuf = Lexing.from_string str; lexer = Type_lexer.token; peeked = None }
  in
  try Ok (typ tokens) with Parse_error msg -> Error msg
