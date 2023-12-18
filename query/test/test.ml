let () =
  let open Alcotest in
  run "Query"
    [ "Array_succ", Test_array.tests_succ_ge @ Test_array.tests_succ_gt
    ; "Succ", Test_succ.tests_to_seq
    ; "Type_parser", Test_type_parser.tests
    ]
