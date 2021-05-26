let () =
  let stanzas =
    Gen_backend.gen_backend_rules "latex" Latex_t_rule.latex_target_rule
      Gen_backend.files "4.10"
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
