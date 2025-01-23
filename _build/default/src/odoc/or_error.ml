type ('a, 'e) result = ('a, 'e) Result.result = Ok of 'a | Error of 'e

type msg = [ `Msg of string ]

let ( >>= ) r f = match r with Ok v -> f v | Error _ as e -> e

let rec fold_list f acc = function
  | [] -> Ok acc
  | hd :: tl -> f acc hd >>= fun acc -> fold_list f acc tl
