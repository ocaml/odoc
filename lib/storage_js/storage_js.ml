open Common

type writer = out_channel

let open_out = open_out
let close_out = close_out

let save ~db t =
  let str = Marshal.to_string t [] in
  let str = Base64.encode_string str in
  Printf.fprintf db "function sherlodoc_db () { return %S; }\n%!" str

let load str =
  let str = Base64.decode_exn str in
  let db_types, db_names = Marshal.from_string str 0 in
  let db_types =
    Db.Trie_gen.map_leaf
      ~f:(fun occs ->
        Int.Map.map (fun arr -> arr |> Array.to_seq |> Db.Elt.Set.of_seq) occs)
      db_types
  in
  let db_names =
    Db.Trie_gen.map_leaf
      ~f:(fun arr -> arr |> Array.to_seq |> Db.Elt.Set.of_seq)
      db_names
  in
  [ Db.{ db_types; db_names } ]
