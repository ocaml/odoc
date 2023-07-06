let anchor_of_id id = Odoc_model.Names.DefName.make_std ("def-" ^ id)

#if OCAML_VERSION >= (4, 14, 0)

let unpack_uid uid =
  match uid with
  | Shape.Uid.Compilation_unit s -> Some (s, None)
  | Item { comp_unit; id } -> Some (comp_unit, Some (string_of_int id))
  | Predef _ -> None
  | Internal -> None

#else

type uid = unit

let unpack_uid () = None


#endif
