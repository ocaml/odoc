[@@@ocaml.warning "-26"]
open Test

let%expect_test _ =
    let empty_table_heavy =
      test {|
I am a frontmatter
---
I am a docs|};
      [%expect
        {|
        ((output
          (( "\
            \nI am a frontmatter")
           ((("" (2 0) (2 11))
             (paragraph
              ((("" (2 0) (2 1)) (word I)) (("" (2 1) (2 2)) space)
               (("" (2 2) (2 4)) (word am)) (("" (2 4) (2 5)) space)
               (("" (2 5) (2 6)) (word a)) (("" (2 6) (2 7)) space)
               (("" (2 7) (2 11)) (word docs))))))))
         (warnings ())) |}]
   in
  ()
