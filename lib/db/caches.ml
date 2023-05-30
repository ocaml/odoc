module String = Cache.Make (struct
  type t = string

  let copy str = String.init (String.length str) (String.get str)
end)

module Char_list = struct
  module H = Hashtbl.Make (struct
    type t = char list

    let equal = List.equal Char.equal
    let hash = Hashtbl.hash
  end)

  let cache = H.create 128

  let memo lst =
    let rec go lst =
      try H.find cache lst
      with Not_found ->
        let lst =
          match lst with
          | [] -> []
          | x :: xs -> x :: go xs
        in
        H.add cache lst lst ;
        lst
    in
    go lst
end

module String_list = struct
  module H = Hashtbl.Make (struct
    type t = string list

    let equal = List.equal Stdlib.String.equal
    let hash = Hashtbl.hash
  end)

  let cache = H.create 128

  let memo lst =
    let rec go lst =
      try H.find cache lst
      with Not_found ->
        let lst = List.map String.memo lst in
        let lst =
          match lst with
          | [] -> []
          | x :: xs -> x :: go xs
        in
        H.add cache lst lst ;
        lst
    in
    go lst
end

module Cache_string_list_list = struct
  module H = Hashtbl.Make (struct
    type t = string list list

    let equal = List.equal (List.equal Stdlib.String.equal)
    let hash = Hashtbl.hash
  end)

  let cache = H.create 128

  let memo lst =
    let rec go lst =
      try H.find cache lst
      with Not_found ->
        let lst = List.map String_list.memo lst in
        let lst =
          match lst with
          | [] -> []
          | x :: xs -> x :: go xs
        in
        H.add cache lst lst ;
        lst
    in
    go lst
end

module Array = Cache.Make (struct
  type t = Elt.t array

  let copy = Fun.id
end)
