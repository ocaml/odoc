open Common

type writer = out_channel

let open_out = open_out
let close_out = close_out

let save ~db t =
  let t =
    Db.Storage.(
      ( Db.Trie.map_leaf
          ~f:(fun occs ->
            Int.Map.map
              (fun _sets ->
                (*sets |> Db.Elt.Set.elements |> Array.of_list
                |> Db.Caches.Array.memo*) ())
              occs)
          t.db
      , Db.Trie.map_leaf
          ~f:(fun _set ->
            (*set |> Db.Elt.Set.elements |> Array.of_list |> Db.Caches.Array.memo*)
            ())
          t.db_names ))
  in
  let str = Marshal.to_string t [] in
  let str = Base64.encode_string str in
  Printf.fprintf db "function sherlodoc_db () { return %S; }\n%!" str

let load str =
  let str = Base64.decode_exn str in
  [ Marshal.from_string str 0 ]
