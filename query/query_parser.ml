let parse str = Type_parser.of_string str

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
        match parse str_typ with
        | Ok Any -> Error `any
        | Ok typ -> Ok typ
        | Error _ -> Error `parse)
  in
  let words = naive_of_string str_name in
  words, typ
