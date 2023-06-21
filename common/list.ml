include Stdlib.List

let to_string ?(start="[") ?(sep="; ") ?(end_="]") a li =
  start ^ (li |> map a |> String.concat sep ) ^ end_



let pprint ?(start="[") ?(sep="; ") ?(end_="]") a li =
  start ^ (li |> map a |> String.concat sep ) ^ end_