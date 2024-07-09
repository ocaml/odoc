module List = struct
  let rec concat_map ?sep ~f = function
    | [] -> []
    | [ x ] -> f x
    | x :: xs -> (
        let hd = f x in
        let tl = concat_map ?sep ~f xs in
        match sep with None -> hd @ tl | Some sep -> hd @ (sep :: tl))
end
