open Test

[@@@ocaml.warning "-32"]

let heavy () =
  let module Heavy = struct
    let empty_table_heavy = test "{table }"

    let empty_row = test "{table {tr } }"

    let no_header = test "{table {tr {td}}}"

    let no_data = test "{table {tr {th}}}"

    let bad_data = test "{table absurd content}"

    let bad_row = test "{table {tr absurd content}}"

    let multiple_headers = test "{table {tr {th}} {tr {th}} {tr {td}}}"

    let unclosed_table = test "{table {tr {td}}"

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
        |}
  end in
  ()

let light () =
  let module Light = struct
    let empty_table_light = test "{t }"

    let unclosed_table = test "{t "

    let simple = test {|
        {t
          | a |
        }
      |}

    let stars =
      test
        {|
        {t
          |a|   *b*|
          |*c| d* |
        }
      |}

    let backquotes = test {|
      {t
         | `a |`
      }
      |}

    let no_header =
      test {|
      {t
       |---|---|
       | x | y |
      }
      |}

    let no_align =
      test {|
      {t
       | x | y |
       | x | y |
      }
      |}

    let only_align = test {|
      {t
        |--|--|
      }
      |}

    let no_data =
      test {|
      {t
       | x | y |
       |---|---|
      }
      |}

    let alignment =
      test
        {|
      {t
       | a | b | c | d |
       |---|:--|--:|:-:|
      }
      |}

    let no_bars =
      test
        {|
      {t
        a | b | c | d
       ---|:--|--:|:-:
        a | b | c | d
      }
      |}

    let light_table_new_lines =
      test
        {|
      {t

       | a | b | c | d |

       |---|---|---|---|

       | a | b | c | d |

      }
      |}

    let light_table_markup =
      test
        {|
      {t
       | {i a} {:google.com} \t | | {m b} {e c} {% xyz %} | {b d} [foo] |
       |---|---|---|---|
      }
      |}

    let light_table_markup_with_newlines =
      test
        {|
      {t | h1           | h2          |
         |--------------|-------------|
         | {e with
              newlines} | {b d} [foo] |
      }
      |}

    let no_space =
      test
        {|
       {t
         | a | b |c| d |
         |---|--:|:--|:-:|
       }
      |}

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
      |}

    let block_element_in_cell =
      test
        {|
           {t
           | {[ a ]} | b |
           |---|---|
           }
          |}

    let block_element_in_row =
      test
        {|
           {t
           {[ a ]}
           | a | b |
           |---|---|
           }
          |}

    let more_cells_later =
      test
        {|
      {t
       | x | y |
       |---|---|
       | x | y | z |
      }
      |}

    let less_cells_later =
      test
        {|
      {t
       | x | y |
       |---|---|
       x 
      }
      |}

    let multiple_word =
      test
        {|
      {t
  | Header and other word |
  |-----------------------|
  | cell and other words  |
      }
      |}

    let multiple_word_header =
      test
        {|
      {t
  | Header other word |
  |-------------------|
  | Header other word |
      }
      |}
  end in
  ()
