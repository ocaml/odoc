type msg = [ `Msg of string ]

val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

val fold_list :
  ('acc -> 'a -> ('acc, 'e) result) -> 'acc -> 'a list -> ('acc, 'e) result
