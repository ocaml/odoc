(** Let-open in class types, https://github.com/ocaml/odoc/issues/543
    This was added to the language in 4.06 *)

class type let_open =
  let open List in
  object end

class let_open' :
  let open List in
  object end
