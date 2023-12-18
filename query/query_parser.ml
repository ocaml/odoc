let parse str = Parser.main Lexer.token (Lexing.from_string str)

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
  let pretty_typ, type_polarities, typ =
    match parse str_typ with
    | Any ->  "_", None, None
    | typ ->
        ( Db.Typexpr.show typ
        , Some (List.filter
            (fun (word, _count) -> String.length word > 0)
            (Db.Type_polarity.of_typ ~ignore_any:true ~all_names:false typ))
        , Some typ )
    | exception Parser.Error ->
      "<parse error>", None, None
  in
  let query_name = naive_of_string str_name in
  let type_polarities = if has_typ then type_polarities else None in
  let pretty_query = String.concat " " query_name ^ " : " ^ pretty_typ in
  query_name, type_polarities, typ, pretty_query
