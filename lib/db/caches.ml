module Cache = Cache.Make (struct
type t = string

let copy str = String.init (String.length str) (String.get str)
end)

module Cache_list = struct
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

(*
   <* for%iter item do *>

  ...

  <* done *>
*)