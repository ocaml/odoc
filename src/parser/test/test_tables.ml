open Test

[@@@ocaml.warning "-32"]

let%expect_test _ =
  let module Heavy = struct
    let empty_table_heavy =
      test "{table }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (table (syntax heavy) (grid ()) (align "no alignment")))))
         (warnings ())) |}]

    let empty_row =
      test "{table {tr } }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (table (syntax heavy) (grid ((row ()))) (align "no alignment")))))
         (warnings ()))|}]

    let no_header =
      test "{table {tr {td}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 17))
            (table (syntax heavy) (grid ((row ((data ()))))) (align "no alignment")))))
         (warnings ())) |}]

    let no_data =
      test "{table {tr {th}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 17))
            (table (syntax heavy) (grid ((row ((header ())))))
             (align "no alignment")))))
         (warnings ())) |}]

    let bad_data =
      test "{table absurd content}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 22))
            (table (syntax heavy) (grid ()) (align "no alignment")))))
         (warnings
          ( "File \"f.ml\", line 1, characters 7-13:\
           \n'absurd' is not allowed in '{table ...}' (table).\
           \nSuggestion: Move outside of {table ...}, or inside {tr ...}"
            "File \"f.ml\", line 1, characters 14-21:\
           \n'content' is not allowed in '{table ...}' (table).\
           \nSuggestion: Move outside of {table ...}, or inside {tr ...}"))) |}]

    let bad_row =
      test "{table {tr absurd content}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 27))
            (table (syntax heavy) (grid ((row ()))) (align "no alignment")))))
         (warnings
          ( "File \"f.ml\", line 1, characters 11-17:\
           \n'absurd' is not allowed in '{tr ...}' (table row).\
           \nSuggestion: Move outside of {table ...}, or inside {td ...} or {th ...}"
            "File \"f.ml\", line 1, characters 18-25:\
           \n'content' is not allowed in '{tr ...}' (table row).\
           \nSuggestion: Move outside of {table ...}, or inside {td ...} or {th ...}"))) |}]

    let multiple_headers =
      test "{table {tr {th}} {tr {th}} {tr {td}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 37))
            (table (syntax heavy)
             (grid ((row ((header ()))) (row ((header ()))) (row ((data ())))))
             (align "no alignment")))))
         (warnings ())) |}]

    let complex_table =
      test
        {|
        {table
          {tr
            {th xxx}
            {th yyy}
          }
          {tr
            {td aaaa bbb ccc {i ddd}
            }
            {td
               {table {tr {td}}}
            }
          }
          {tr
            {td
               - aaa
               - bbb
               - ccc
            }
            {td
              {t
                 x | y | z
                 --|---|--
                 1 | 2 | 3
              }
            }
          }
        }
        |};
      [%expect
        {|
        ((output
          (((f.ml (2 8) (28 9))
            (table (syntax heavy)
             (grid
              ((row
                ((header
                  (((f.ml (4 16) (4 19))
                    (paragraph (((f.ml (4 16) (4 19)) (word xxx)))))))
                 (header
                  (((f.ml (5 16) (5 19))
                    (paragraph (((f.ml (5 16) (5 19)) (word yyy)))))))))
               (row
                ((data
                  (((f.ml (8 16) (8 36))
                    (paragraph
                     (((f.ml (8 16) (8 20)) (word aaaa)) ((f.ml (8 20) (8 21)) space)
                      ((f.ml (8 21) (8 24)) (word bbb)) ((f.ml (8 24) (8 25)) space)
                      ((f.ml (8 25) (8 28)) (word ccc)) ((f.ml (8 28) (8 29)) space)
                      ((f.ml (8 29) (8 36))
                       (italic (((f.ml (8 32) (8 35)) (word ddd))))))))))
                 (data
                  (((f.ml (11 15) (11 32))
                    (table (syntax heavy) (grid ((row ((data ())))))
                     (align "no alignment")))))))
               (row
                ((data
                  (((f.ml (16 15) (18 20))
                    (unordered light
                     ((((f.ml (16 17) (16 20))
                        (paragraph (((f.ml (16 17) (16 20)) (word aaa))))))
                      (((f.ml (17 17) (17 20))
                        (paragraph (((f.ml (17 17) (17 20)) (word bbb))))))
                      (((f.ml (18 17) (18 20))
                        (paragraph (((f.ml (18 17) (18 20)) (word ccc)))))))))))
                 (data
                  (((f.ml (21 14) (25 15))
                    (table (syntax light)
                     (grid
                      ((row
                        ((header
                          (((f.ml (22 17) (22 18))
                            (paragraph (((f.ml (22 17) (22 18)) (word x)))))))
                         (header
                          (((f.ml (22 21) (22 22))
                            (paragraph (((f.ml (22 21) (22 22)) (word y)))))))
                         (header
                          (((f.ml (22 25) (22 26))
                            (paragraph (((f.ml (22 25) (22 26)) (word z)))))))))
                       (row
                        ((data
                          (((f.ml (24 17) (24 18))
                            (paragraph (((f.ml (24 17) (24 18)) (word 1)))))))
                         (data
                          (((f.ml (24 21) (24 22))
                            (paragraph (((f.ml (24 21) (24 22)) (word 2)))))))
                         (data
                          (((f.ml (24 25) (24 26))
                            (paragraph (((f.ml (24 25) (24 26)) (word 3)))))))))))
                     (align (default default default))))))))))
             (align "no alignment")))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Light = struct
    let empty_table_light =
      test "{t }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (table (syntax light) (grid ()) (align "no alignment")))))
         (warnings ())) |}]

    let simple =
      test {|
        {t
          | a |
        }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 8) (4 9))
            (table (syntax light)
             (grid
              ((row
                ((data
                  (((f.ml (3 12) (3 13))
                    (paragraph (((f.ml (3 12) (3 13)) (word a)))))))))))
             (align "no alignment")))))
         (warnings ())) |}]

    let stars =
      test
        {|
        {t
          |a|   *b*|
          |*c| d* |
        }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 8) (5 9))
            (table (syntax light)
             (grid
              ((row
                ((data
                  (((f.ml (3 11) (3 12))
                    (paragraph (((f.ml (3 11) (3 12)) (word a)))))))
                 (data
                  (((f.ml (3 16) (3 19))
                    (paragraph (((f.ml (3 16) (3 19)) (word *b*)))))))))
               (row
                ((data
                  (((f.ml (4 11) (4 13))
                    (paragraph (((f.ml (4 11) (4 13)) (word *c)))))))
                 (data
                  (((f.ml (4 15) (4 17))
                    (paragraph (((f.ml (4 15) (4 17)) (word d*)))))))))))
             (align "no alignment")))))
         (warnings ())) |}]

    let backquotes =
      test {|
      {t
         | `a |`
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (4 7))
            (table (syntax light)
             (grid
              ((row
                ((data
                  (((f.ml (3 11) (3 13))
                    (paragraph (((f.ml (3 11) (3 13)) (word `a)))))))
                 (data
                  (((f.ml (3 15) (3 16))
                    (paragraph (((f.ml (3 15) (3 16)) (word `)))))))))))
             (align "no alignment")))))
         (warnings ())) |}]

    let no_header =
      test {|
      {t
       |---|---|
       | x | y |
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (5 7))
            (table (syntax light)
             (grid
              ((row
                ((data
                  (((f.ml (4 9) (4 10)) (paragraph (((f.ml (4 9) (4 10)) (word x)))))))
                 (data
                  (((f.ml (4 13) (4 14))
                    (paragraph (((f.ml (4 13) (4 14)) (word y)))))))))))
             (align (default default))))))
         (warnings ())) |}]

    let no_align =
      test {|
      {t
       | x | y |
       | x | y |
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (5 7))
              (table (syntax light)
               (grid
                ((row
                  ((data
                    (((f.ml (3 9) (3 10)) (paragraph (((f.ml (3 9) (3 10)) (word x)))))))
                   (data
                    (((f.ml (3 13) (3 14))
                      (paragraph (((f.ml (3 13) (3 14)) (word y)))))))))
                 (row
                  ((data
                    (((f.ml (4 9) (4 10)) (paragraph (((f.ml (4 9) (4 10)) (word x)))))))
                   (data
                    (((f.ml (4 13) (4 14))
                      (paragraph (((f.ml (4 13) (4 14)) (word y)))))))))))
               (align "no alignment")))))
           (warnings ())) |}]

    let only_align =
      test {|
      {t
        |--|--|
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (4 7))
            (table (syntax light) (grid ()) (align (default default))))))
         (warnings ())) |}]

    let no_data =
      test {|
      {t
       | x | y |
       |---|---|
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (5 7))
              (table (syntax light)
               (grid
                ((row
                  ((header
                    (((f.ml (3 9) (3 10)) (paragraph (((f.ml (3 9) (3 10)) (word x)))))))
                   (header
                    (((f.ml (3 13) (3 14))
                      (paragraph (((f.ml (3 13) (3 14)) (word y)))))))))))
               (align (default default))))))
           (warnings ())) |}]

    let alignment =
      test
        {|
      {t
       | a | b | c | d |
       |---|:--|--:|:-:|
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (5 7))
            (table (syntax light)
             (grid
              ((row
                ((header
                  (((f.ml (3 9) (3 10)) (paragraph (((f.ml (3 9) (3 10)) (word a)))))))
                 (header
                  (((f.ml (3 13) (3 14))
                    (paragraph (((f.ml (3 13) (3 14)) (word b)))))))
                 (header
                  (((f.ml (3 17) (3 18))
                    (paragraph (((f.ml (3 17) (3 18)) (word c)))))))
                 (header
                  (((f.ml (3 21) (3 22))
                    (paragraph (((f.ml (3 21) (3 22)) (word d)))))))))))
             (align (default left right center))))))
         (warnings ())) |}]

    let no_bars =
      test
        {|
      {t
        a | b | c | d
       ---|:--|--:|:-:
        a | b | c | d
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (6 7))
              (table (syntax light)
               (grid
                ((row
                  ((header
                    (((f.ml (3 8) (3 9)) (paragraph (((f.ml (3 8) (3 9)) (word a)))))))
                   (header
                    (((f.ml (3 12) (3 13))
                      (paragraph (((f.ml (3 12) (3 13)) (word b)))))))
                   (header
                    (((f.ml (3 16) (3 17))
                      (paragraph (((f.ml (3 16) (3 17)) (word c)))))))
                   (header
                    (((f.ml (3 20) (3 21))
                      (paragraph (((f.ml (3 20) (3 21)) (word d)))))))))
                 (row
                  ((data
                    (((f.ml (5 8) (5 9)) (paragraph (((f.ml (5 8) (5 9)) (word a)))))))
                   (data
                    (((f.ml (5 12) (5 13))
                      (paragraph (((f.ml (5 12) (5 13)) (word b)))))))
                   (data
                    (((f.ml (5 16) (5 17))
                      (paragraph (((f.ml (5 16) (5 17)) (word c)))))))
                   (data
                    (((f.ml (5 20) (5 21))
                      (paragraph (((f.ml (5 20) (5 21)) (word d)))))))))))
               (align (default left right center))))))
           (warnings ())) |}]

    let light_table_new_lines =
      test
        {|
      {t

       | a | b | c | d |

       |---|---|---|---|

       | a | b | c | d |

      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (10 7))
              (table (syntax light)
               (grid
                ((row
                  ((header
                    (((f.ml (4 9) (4 10)) (paragraph (((f.ml (4 9) (4 10)) (word a)))))))
                   (header
                    (((f.ml (4 13) (4 14))
                      (paragraph (((f.ml (4 13) (4 14)) (word b)))))))
                   (header
                    (((f.ml (4 17) (4 18))
                      (paragraph (((f.ml (4 17) (4 18)) (word c)))))))
                   (header
                    (((f.ml (4 21) (4 22))
                      (paragraph (((f.ml (4 21) (4 22)) (word d)))))))))
                 (row
                  ((data
                    (((f.ml (8 9) (8 10)) (paragraph (((f.ml (8 9) (8 10)) (word a)))))))
                   (data
                    (((f.ml (8 13) (8 14))
                      (paragraph (((f.ml (8 13) (8 14)) (word b)))))))
                   (data
                    (((f.ml (8 17) (8 18))
                      (paragraph (((f.ml (8 17) (8 18)) (word c)))))))
                   (data
                    (((f.ml (8 21) (8 22))
                      (paragraph (((f.ml (8 21) (8 22)) (word d)))))))))))
               (align (default default default default))))))
           (warnings ())) |}]

    let light_table_markup =
      test
        {|
      {t
       | {i a} {:google.com} \t | | {m b} {e c} {% xyz %} | {b d} [foo] |
       |---|---|---|---|
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (5 7))
              (table (syntax light)
               (grid
                ((row
                  ((header
                    (((f.ml (3 9) (3 14))
                      (paragraph
                       (((f.ml (3 9) (3 14))
                         (italic (((f.ml (3 12) (3 13)) (word a))))))))
                     ((f.ml (3 15) (3 28))
                      (paragraph (((f.ml (3 15) (3 28)) (google.com ())))))
                     ((f.ml (3 29) (3 31))
                      (paragraph (((f.ml (3 29) (3 31)) (word "\\t")))))))
                   (header ())
                   (header
                    (((f.ml (3 36) (3 41))
                      (paragraph (((f.ml (3 36) (3 41)) (math_span b)))))
                     ((f.ml (3 42) (3 47))
                      (paragraph
                       (((f.ml (3 42) (3 47))
                         (emphasis (((f.ml (3 45) (3 46)) (word c))))))))
                     ((f.ml (3 48) (3 57))
                      (paragraph (((f.ml (3 48) (3 57)) (raw_markup () " xyz ")))))))
                   (header
                    (((f.ml (3 60) (3 65))
                      (paragraph
                       (((f.ml (3 60) (3 65)) (bold (((f.ml (3 63) (3 64)) (word d))))))))
                     ((f.ml (3 66) (3 71))
                      (paragraph (((f.ml (3 66) (3 71)) (code_span foo)))))))))))
               (align (default default default default))))))
           (warnings ())) |}]

    let light_table_markup_with_newlines =
      test
        {|
      {t | h1           | h2          |
         |--------------|-------------|
         | {e with
              newlines} | {b d} [foo] |
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (6 7))
            (table (syntax light)
             (grid
              ((row
                ((header
                  (((f.ml (2 11) (2 13))
                    (paragraph (((f.ml (2 11) (2 13)) (word h1)))))))
                 (header
                  (((f.ml (2 26) (2 28))
                    (paragraph (((f.ml (2 26) (2 28)) (word h2)))))))))
               (row
                ((data
                  (((f.ml (4 11) (5 23))
                    (paragraph
                     (((f.ml (4 11) (5 23))
                       (emphasis
                        (((f.ml (4 14) (4 18)) (word with))
                         ((f.ml (4 18) (5 14)) space)
                         ((f.ml (5 14) (5 22)) (word newlines))))))))))
                 (data
                  (((f.ml (5 26) (5 31))
                    (paragraph
                     (((f.ml (5 26) (5 31)) (bold (((f.ml (5 29) (5 30)) (word d))))))))
                   ((f.ml (5 32) (5 37))
                    (paragraph (((f.ml (5 32) (5 37)) (code_span foo)))))))))))
             (align (default default))))))
         (warnings
          ( "File \"f.ml\", line 4, character 11 to line 5, character 23:\
           \nLine break is not allowed in '{t ...}' (table)."))) |}]

    let no_space =
      test
        {|
       {t
         | a | b |c| d |
         |---|--:|:--|:-:|
       }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 7) (5 8))
            (table (syntax light)
             (grid
              ((row
                ((header
                  (((f.ml (3 11) (3 12))
                    (paragraph (((f.ml (3 11) (3 12)) (word a)))))))
                 (header
                  (((f.ml (3 15) (3 16))
                    (paragraph (((f.ml (3 15) (3 16)) (word b)))))))
                 (header
                  (((f.ml (3 18) (3 19))
                    (paragraph (((f.ml (3 18) (3 19)) (word c)))))))
                 (header
                  (((f.ml (3 21) (3 22))
                    (paragraph (((f.ml (3 21) (3 22)) (word d)))))))))))
             (align (default right left center))))))
         (warnings ())) |}]

    let multiple_headers =
      test
        {|
      {t
       ||a|b|
       |:-|---:|
       |c|d|
       |cc|dd|
       |-:|:-:|
       |e|f|
       |g|h||
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (10 7))
            (table (syntax light)
             (grid
              ((row
                ((header ())
                 (header
                  (((f.ml (3 9) (3 10)) (paragraph (((f.ml (3 9) (3 10)) (word a)))))))
                 (header
                  (((f.ml (3 11) (3 12))
                    (paragraph (((f.ml (3 11) (3 12)) (word b)))))))))
               (row
                ((data
                  (((f.ml (5 8) (5 9)) (paragraph (((f.ml (5 8) (5 9)) (word c)))))))
                 (data
                  (((f.ml (5 10) (5 11))
                    (paragraph (((f.ml (5 10) (5 11)) (word d)))))))))
               (row
                ((data
                  (((f.ml (6 8) (6 10))
                    (paragraph (((f.ml (6 8) (6 10)) (word cc)))))))
                 (data
                  (((f.ml (6 11) (6 13))
                    (paragraph (((f.ml (6 11) (6 13)) (word dd)))))))))
               (row
                ((data
                  (((f.ml (7 8) (7 10))
                    (paragraph (((f.ml (7 8) (7 10)) (word -:)))))))
                 (data
                  (((f.ml (7 11) (7 14))
                    (paragraph (((f.ml (7 11) (7 14)) (word :-:)))))))))
               (row
                ((data
                  (((f.ml (8 8) (8 9)) (paragraph (((f.ml (8 8) (8 9)) (word e)))))))
                 (data
                  (((f.ml (8 10) (8 11))
                    (paragraph (((f.ml (8 10) (8 11)) (word f)))))))))
               (row
                ((data
                  (((f.ml (9 8) (9 9)) (paragraph (((f.ml (9 8) (9 9)) (word g)))))))
                 (data
                  (((f.ml (9 10) (9 11))
                    (paragraph (((f.ml (9 10) (9 11)) (word h)))))))
                 (data ())))))
             (align (left right))))))
         (warnings ())) |}]

    let block_element_in_cell =
      test
        {|
           {t
           | {[ a ]} | b |
           |---|---|
           }
          |};
      [%expect
        {|
        ((output
          (((f.ml (2 11) (5 12))
            (table (syntax light)
             (grid
              ((row
                ((header ())
                 (header
                  (((f.ml (3 23) (3 24))
                    (paragraph (((f.ml (3 23) (3 24)) (word b)))))))))))
             (align (default default))))))
         (warnings
          ( "File \"f.ml\", line 3, characters 13-20:\
           \n'{[...]}' (code block) is not allowed in '{t ...}' (table)."))) |}]

    let block_element_in_row =
      test
        {|
           {t
           {[ a ]}
           | a | b |
           |---|---|
           }
          |};
      [%expect
        {|
        ((output
          (((f.ml (2 11) (6 12))
            (table (syntax light)
             (grid
              ((row
                ((header
                  (((f.ml (4 13) (4 14))
                    (paragraph (((f.ml (4 13) (4 14)) (word a)))))))
                 (header
                  (((f.ml (4 17) (4 18))
                    (paragraph (((f.ml (4 17) (4 18)) (word b)))))))))))
             (align (default default))))))
         (warnings
          ( "File \"f.ml\", line 3, characters 11-18:\
           \n'{[...]}' (code block) is not allowed in '{t ...}' (table)."))) |}]

    let more_cells_later =
      test
        {|
      {t
       | x | y |
       |---|---|
       | x | y | z |
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (6 7))
            (table (syntax light)
             (grid
              ((row
                ((header
                  (((f.ml (3 9) (3 10)) (paragraph (((f.ml (3 9) (3 10)) (word x)))))))
                 (header
                  (((f.ml (3 13) (3 14))
                    (paragraph (((f.ml (3 13) (3 14)) (word y)))))))))
               (row
                ((data
                  (((f.ml (5 9) (5 10)) (paragraph (((f.ml (5 9) (5 10)) (word x)))))))
                 (data
                  (((f.ml (5 13) (5 14))
                    (paragraph (((f.ml (5 13) (5 14)) (word y)))))))
                 (data
                  (((f.ml (5 17) (5 18))
                    (paragraph (((f.ml (5 17) (5 18)) (word z)))))))))))
             (align (default default))))))
         (warnings ())) |}]

    let less_cells_later =
      test
        {|
      {t
       | x | y |
       |---|---|
       x 
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (6 7))
            (table (syntax light)
             (grid
              ((row
                ((header
                  (((f.ml (3 9) (3 10)) (paragraph (((f.ml (3 9) (3 10)) (word x)))))))
                 (header
                  (((f.ml (3 13) (3 14))
                    (paragraph (((f.ml (3 13) (3 14)) (word y)))))))))
               (row
                ((data
                  (((f.ml (5 7) (5 8)) (paragraph (((f.ml (5 7) (5 8)) (word x)))))))))))
             (align (default default))))))
         (warnings ())) |}]
  end in
  ()
