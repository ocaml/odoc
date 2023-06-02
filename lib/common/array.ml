include Stdlib.Array

let equal (a : 'a -> 'a -> bool) arr arr' =
  length arr = length arr' && for_all2 a arr arr'

let hash (a : 'a -> int) arr = Hashtbl.hash (map a arr)
