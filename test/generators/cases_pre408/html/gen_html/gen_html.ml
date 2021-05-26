let () =
  let stanzas =
    Gen_backend.gen_backend_rules "html" Html_t_rule.html_target_rule
      Gen_backend.files "4.10"
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
