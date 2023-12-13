type t = string list

let parse str = Parser.main Lexer.token (Lexing.from_string str)

let alphanum = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '\'' -> true
  | _ -> false

let naive_of_string str =
  List.filter (fun s -> String.length s > 0) (String.split_on_char ' ' str)

let guess_type_search str =
  String.length str >= 1
  && (str.[0] = '\'' || String.contains str '-' || String.contains str '(')

let of_string str =
  let str = String.trim str in
  let has_typ, str_name, str_typ =
    match String.split_on_char ':' str with
    | [ a; b ] -> true, a, b
    | _ when guess_type_search str -> true, "", str
    | _ -> false, str, ""
  in
  let pretty_typ, query_typ, paths_typ =
    match parse str_typ with
    | Any -> "_", [], []
    | typ ->
        ( Db.Typepath.show typ
        , List.filter
            (fun s -> List.length s > 0)
            (Db.Typepath.For_suffix_tree.of_typ ~ignore_any:true
               ~all_names:false typ)
        , Db.Typepath.For_distance.of_typ ~ignore_any:true typ )
    | exception _ -> "<parse error>", [], []
  in
  let query_name = naive_of_string str_name in
  let query_typ = if has_typ then Some query_typ else None in
  let pretty_query = String.concat " " query_name ^ " : " ^ pretty_typ in
  query_name, query_typ, paths_typ, pretty_query
