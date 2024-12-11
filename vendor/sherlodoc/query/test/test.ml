let () =
  let open Alcotest in
  run "Query" [ "Succ", Test_succ.tests_to_seq; "Type_parser", Test_type_parser.tests ]
