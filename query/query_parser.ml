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
  try `typ (Type_parser.main Type_lexer.token lexbuf) with
  | _ -> `parse_error

let naive_of_string str =
  List.filter (fun s -> String.length s > 0) (String.split_on_char ' ' str)

let guess_type_search str =
  String.length str >= 1
  && (str.[0] = '\'' || String.contains str '-' || String.contains str '(')

type t =
  { name : string list
  ; typ : [ `typ of Db.Typexpr.t | `no_typ | `parse_error ]
  }

let of_string str =
  let query_name, typ =
    match String.index_opt str ':' with
    | None -> if guess_type_search str then "", type_of_string str else str, `no_typ
    | Some loc ->
      let str_name = String.sub str 0 loc in
      let str_typ = String.sub str (loc + 1) (String.length str - loc - 1) in
      str_name, type_of_string str_typ
  in
  let name = naive_of_string query_name in
  { name; typ }

let to_string { name; typ } =
  let words = String.concat " " name in
  match typ with
  | `typ typ -> words ^ " : " ^ Db.Typexpr.show typ
  | `parse_error -> words ^ " : <parsing error>"
  | `no_typ -> words
