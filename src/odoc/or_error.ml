type msg = [ `Msg of string ]

let ( >>= ) = Result.bind

let rec fold_list f acc = function
  | [] -> Ok acc
  | hd :: tl -> f acc hd >>= fun acc -> fold_list f acc tl
