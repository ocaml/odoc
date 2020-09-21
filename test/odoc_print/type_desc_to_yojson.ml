(** Generic converter to Yojson. *)

open Type_desc

type yojson = Yojson.Basic.t

let rec to_yojson : type a. a t -> a -> yojson =
 fun t a ->
  match t with
  | Record fields ->
      let field_to_yojson (F (name, get, t)) = (name, to_yojson t (get a)) in
      `Assoc (List.map field_to_yojson fields)
  | Variant get ->
      let case = function
        | C0 name -> [ `String name ]
        | C (name, a', t) -> [ `String name; to_yojson t a' ]
      in
      `List (case (get a))
  | Pair (t1, t2) ->
      let a1, a2 = a in
      `List [ to_yojson t1 a1; to_yojson t2 a2 ]
  | Triple (t1, t2, t3) ->
      let a1, a2, a3 = a in
      `List [ to_yojson t1 a1; to_yojson t2 a2; to_yojson t3 a3 ]
  | List t -> `List (List.map (to_yojson t) a)
  | Option t ->
      let opt = match a with Some a' -> [ to_yojson t a' ] | None -> [] in
      `List opt
  | To_string to_string -> `String (to_string a)
