let () =
  let stanzas =
    Gen_backend.gen_backend_rules "man" Man_t_rule.man_target_rule
      Gen_backend.files "4.08"
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
