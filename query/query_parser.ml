open Query_ast

type t = string list

let parse str = Parser.main Lexer.token (Lexing.from_string str)

let alphanum = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '\'' -> true
  | _ -> false

let naive_of_string str =
  List.filter (fun s -> String.length s > 0) (String.split_on_char ' ' str)

let of_string str =
  let ok, str_name, str_typ =
    match String.split_on_char ':' str with
    | [ a; b ] -> true, a, b
    | _ -> false, str, ""
  in
  let pretty, ps =
    match parse str_typ with
    | Any -> "_", []
    | typ ->
        ( Query_ast.show typ
        , List.filter
            (fun s -> List.length s > 0)
            (paths ~prefix:[] ~sgn:Db.Types.Pos typ) )
    | exception _ -> "<parse error>", []
  in
  let keywords = naive_of_string str_name in
  let keywords_pretty = String.concat " " keywords in
  ok, keywords, ps, keywords_pretty ^ " : " ^ pretty
