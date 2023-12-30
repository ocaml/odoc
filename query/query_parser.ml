let balance_parens str =
  let rec go i open_parens close_parens =
    if i >= String.length str
    then open_parens, close_parens
    else (
      match str.[i] with
      | '(' -> go (i + 1) (open_parens + 1) close_parens
      | ')' when open_parens > 0 -> go (i + 1) (open_parens - 1) close_parens
      | ')' -> go (i + 1) open_parens (close_parens + 1)
      | _ -> go (i + 1) open_parens close_parens)
  in
  let open_parens, close_parens = go 0 0 0 in
  String.make close_parens '(' ^ str ^ String.make open_parens ')'

let type_of_string str =
  let str = balance_parens str in
  let lexbuf = Lexing.from_string str in
  try Ok (Type_parser.main Type_lexer.token lexbuf) with
  | Type_parser.Error -> Error "parse error"

let naive_of_string str =
  List.filter (fun s -> String.length s > 0) (String.split_on_char ' ' str)

let guess_type_search str =
  String.length str >= 1
  && (str.[0] = '\'' || String.contains str '-' || String.contains str '(')

let of_string str =
  let str = String.trim str in
  let str_name, str_typ =
    match String.split_on_char ':' str with
    | [ a; b ] -> a, Ok b
    | _ when guess_type_search str -> "", Ok str
    | _ -> str, Error `empty
  in
  let typ =
    Result.bind str_typ (fun str_typ ->
      match type_of_string str_typ with
      | Ok Any -> Error `any
      | Ok typ -> Ok typ
      | Error _ -> Error `parse)
  in
  let words = naive_of_string str_name in
  words, typ
