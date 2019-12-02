type ('a, 'e) result = ('a, 'e) Result.result =
  | Ok of 'a
  | Error of 'e

type 'a or_error = ('a, [ `Msg of string ]) result

let (>>=) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e
