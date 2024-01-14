module Entry = Db.Entry

type query =
  { name : string list
  ; type_paths : Type_distance.Type_path.t option
  }

let of_query { Query_parser.name; typ } =
  let type_paths =
    match typ with
    | `typ t -> Some (Type_distance.Type_path.of_typ ~ignore_any:true t)
    | _ -> None
  in
  { name; type_paths }

let type_distance query_type entry =
  match query_type, Entry.Kind.get_type entry.Entry.kind with
  | Some query_paths, Some entry_type ->
    Some (Type_distance.v ~query_paths ~entry:entry_type)
  | Some _, None -> Some 1000
  | _ -> None

let score query entry =
  let found, not_found = Name_cost.best_matches query.name entry.Db.Entry.name in
  let name_matches = found + not_found in
  let type_cost =
    match type_distance query.type_paths entry with
    | Some cost -> cost
    | None -> 0
  in
  10 * (name_matches + type_cost)
