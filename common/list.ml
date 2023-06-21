include Stdlib.List

let to_string ?(start="[") ?(sep="; ") ?(end_="]") a li =
  start ^ (li |> map a |> String.concat sep ) ^ end_