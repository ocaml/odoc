let man_target_rule odocl targets : Gen_backend.sexp =
  List
    [
      Atom "rule";
      List
        [
          Atom "action";
          List
            (Atom "progn"
             ::
             List
               [
                 Atom "run";
                 Atom "odoc";
                 Atom "man-generate";
                 Atom "-o";
                 Atom "man.gen";
                 Atom ("%{dep:" ^ Fpath.to_string odocl ^ "}");
               ]
             :: Gen_backend.gen_targets targets);
        ];
    ]
