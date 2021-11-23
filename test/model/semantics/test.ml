open Odoc_model
open Odoc_model_desc

type sections_allowed = [ `All | `No_titles | `None ]

let warning_desc = Type_desc.To_string Error.to_string

let parser_output_desc =
  let open Type_desc in
  Indirect
    ( Error.unpack_warnings,
      Record
        [
          F ("value", fst, Indirect (fst, Comment_desc.docs));
          F ("warnings", snd, List warning_desc);
        ] )

let test ?(sections_allowed = `No_titles)
    ?(location = { Location_.line = 1; column = 0 }) str =
  let dummy_filename = "f.ml" in
  let dummy_page = `Page (None, Names.PageName.make_std dummy_filename) in
  let location =
    {
      Lexing.pos_fname = dummy_filename;
      pos_lnum = location.line;
      pos_bol = 0;
      pos_cnum = location.column;
    }
  in
  let parser_output =
    Semantics.parse_comment ~internal_tags:Odoc_model.Semantics.Expect_none
      ~sections_allowed ~containing_definition:dummy_page ~location ~text:str
  in
  let print_json_desc desc t =
    let yojson = Type_desc_to_yojson.to_yojson desc t in
    Yojson.Basic.pretty_print Format.std_formatter yojson
  in
  print_json_desc parser_output_desc parser_output;
  Format.pp_print_flush Format.std_formatter ()

[@@@ocaml.warning "-32"]

let%expect_test _ =
  let module Simple_reference = struct
    let basic =
      test "{!foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let leading_whitespace =
      test "{! foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let trailing_whitespace =
      test "{!foo }";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let adjacent_word_leading =
      test "bar{!foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Word": "bar" },
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let explicit_leading_space =
      test "bar {!foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Word": "bar" },
                "`Space",
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let adjacent_word_trailing =
      test "{!foo}bar";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] },
                { "`Word": "bar" }
              ]
            }
          ],
          "warnings": []
        } |}]

    let explicit_trailing_space =
      test "{!foo} bar";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] },
                "`Space",
                { "`Word": "bar" }
              ]
            }
          ],
          "warnings": []
        } |}]

    let kind =
      test "{!val:foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let empty =
      test "{!}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let whitespace_only =
      test "{! }";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": " " } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-3:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let internal_whitespace =
      test "{!( * )}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "(*)", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    (* TODO Limiting the character combinations allowed will make it easier to
       catch expressions accidentally written inside references. This can also
       be caught by a good resolver and resolver error messages. *)
    (* t "expression" *)
    let unterminated =
      test "{!foo";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 5-5:\nEnd of text is not allowed in '{!...}' (cross-reference)."
          ]
        } |}]

    let empty_kind =
      test "{!:foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": ":foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nUnknown reference qualifier ''."
          ]
        } |}]

    let whitespace_kind =
      test "{! :foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": " :foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-3:\nUnknown reference qualifier ''."
          ]
        } |}]

    let with_kind_but_empty =
      test "{!val:}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val:" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-6:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let with_kind_but_whitespace =
      test "{!val: }";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val: " } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-7:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let leading_whitespace_in_kind =
      test "{! val:foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let internal_whitespace_in_kind =
      test "{!va l:foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "va l:foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-6:\nUnknown reference qualifier 'va l'."
          ]
        } |}]

    let internal_whitespace_in_referent =
      test "{!val:( * )}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "(*)", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let two_colons =
      test "{!val:foo:bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val:foo:bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nUnknown reference qualifier 'val:foo'."
          ]
        } |}]

    let space_before_colon =
      test "{!val :foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let space_after_colon =
      test "{!val: foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let unterminated_after_kind =
      test "{!val:foo";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 9-9:\nEnd of text is not allowed in '{!...}' (cross-reference)."
          ]
        } |}]

    let operator =
      test "{!(>>=)}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "(>>=)", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let operator_with_dash =
      test "{!(@->)}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "(@->)", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let operator_with_dot =
      test "{!(*.)}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "(*.)", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let operator_with_colon =
      test "{!(>::)}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "(>::)", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]
  end in
  ()

let%expect_test _ =
  let module Raw_markup = struct
    let html_target =
      test "{%html:foo%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "foo" ] } ] } ],
          "warnings": []
        } |}]

    let whitespace =
      test "{%html: foo bar %}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Raw_markup": [ "html", " foo bar " ] } ] }
          ],
          "warnings": []
        } |}]

    let whitespace_only =
      test "{%html: %}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", " " ] } ] } ],
          "warnings": []
        } |}]

    let empty =
      test "{%html:%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "" ] } ] } ],
          "warnings": []
        } |}]

    let html_payload =
      test "{%html:<e>foo</e>%}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Raw_markup": [ "html", "<e>foo</e>" ] } ] }
          ],
          "warnings": []
        } |}]

    let colon =
      test "{%html:foo:bar%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "foo:bar" ] } ] } ],
          "warnings": []
        } |}]

    let no_target =
      test "{%foo%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\n'{%...%}' (raw markup) needs a target language.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let empty_target =
      test "{%:foo%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-8:\n'{%:': bad raw markup target.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let whitespace_target =
      test "{% :foo%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-9:\n'{% :': bad raw markup target.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let multiline_target =
      test "{%\n:foo%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, character 0 to line 2, character 6:\n'{%\n:': bad raw markup target.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let percent_in_target =
      test "{%%:%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-6:\n'{%%:': bad raw markup target.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let percent_in_payload =
      test "{%html:%%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "%" ] } ] } ],
          "warnings": []
        } |}]

    let multiple_percent_in_target =
      test "{%%%foo%%:%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-12:\n'{%%%foo%%:': bad raw markup target.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let multiple_percent_in_payload =
      test "{%html:%%foo%%%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "%%foo%%" ] } ] } ],
          "warnings": []
        } |}]

    let opener_in_target =
      test "{%{%:foo%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-10:\n'{%{%:': bad raw markup target.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let opener_in_payload =
      test "{%html:{%%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "{%" ] } ] } ],
          "warnings": []
        } |}]

    let right_brace_in_target =
      test "{%}:%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-6:\n'{%}:': bad raw markup target.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let right_brace_in_payload =
      test "{%html:}%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "}" ] } ] } ],
          "warnings": []
        } |}]

    let unterminated =
      test "{%";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nEnd of text is not allowed in '{%...%}' (raw markup).",
            "File \"f.ml\", line 1, characters 0-2:\n'{%...%}' (raw markup) needs a target language.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]

    let unterminated_after_target =
      test "{%html:";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Raw_markup": [ "html", "" ] } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 7-7:\nEnd of text is not allowed in '{%...%}' (raw markup)."
          ]
        } |}]

    let degenerate =
      test "{%}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "}" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 3-3:\nEnd of text is not allowed in '{%...%}' (raw markup).",
            "File \"f.ml\", line 1, characters 0-3:\n'{%...%}' (raw markup) needs a target language.\nSuggestion: try '{%html:...%}'."
          ]
        } |}]
  end in
  ()

let%expect_test _ =
  let module Section_contexts = struct
    let titles_allowed =
      test "{0 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages."
          ]
        } |}]

    let titles_no_high_levels =
      test "{6 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\n'6': bad heading level (0-5 allowed)."
          ]
        } |}]

    let two_titles =
      test "{0 Foo}\n{0 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages.",
            "File \"f.ml\", line 2, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages."
          ]
        } |}]

    let no_heading =
      test "foo";
      [%expect
        {|
        { "value": [ { "`Paragraph": [ { "`Word": "foo" } ] } ], "warnings": [] } |}]

    let heading_after_paragraph =
      test "foo\n{0 Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Word": "foo" } ] },
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages."
          ]
        } |}]

    let two_top_level_section_headings =
      test "{1 Foo}\n{1 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Section", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Section", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let two_headings_second_higher =
      test "{1 Foo}\n{0 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Section", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages."
          ]
        } |}]

    let three_headings_last_two_higher =
      test "{3 Foo}\n{1 Bar}\n{2 Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subsubsection",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Section", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "baz" ] },
                [ { "`Word": "Baz" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let none =
      test "{1 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Section", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let title_no_titles_allowed =
      test "{0 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages."
          ]
        } |}]

    let two_titles_none_allowed =
      test "{0 Foo}\n{0 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages.",
            "File \"f.ml\", line 2, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages."
          ]
        } |}]

    let two_headings_none_allowed =
      test "{1 Foo}\n{1 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Section", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Section", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let multiple_with_bad_section =
      test "{0 Foo}\n{0 Foo}\n{6 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages.",
            "File \"f.ml\", line 2, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages.",
            "File \"f.ml\", line 3, characters 0-7:\n'6': bad heading level (0-5 allowed)."
          ]
        } |}]

    let promoted_duplicates =
      test "{6 Foo}\n{6 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\n'6': bad heading level (0-5 allowed).",
            "File \"f.ml\", line 2, characters 0-7:\n'6': bad heading level (0-5 allowed)."
          ]
        } |}]

    let section_promoted_to_duplicate =
      test "{5 Foo}\n{6 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-7:\n'6': bad heading level (0-5 allowed)."
          ]
        } |}]
  end in
  ()

let%expect_test _ =
  let module Heading = struct
    let basic =
      test "{2 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let subsection =
      test "{3 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subsubsection",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let subsubsection =
      test "{4 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Paragraph", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let leading_whitespace =
      test "{2  Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let no_leading_whitespace =
      test "{2Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-2:\n'{2' should be followed by space, a tab, or a new line."
          ]
        } |}]

    let no_leading_whitespace_h3 =
      test "{3Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subsubsection",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-2:\n'{3' should be followed by space, a tab, or a new line."
          ]
        } |}]

    let leading_newline =
      test "{2\nFoo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let leading_cr_lf =
      test "{2\r\nFoo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let leading_blank_line =
      test "{2\n\nFoo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-0:\nBlank line is not allowed in '{2 ...}' (section heading)."
          ]
        } |}]

    let leading_blank_line_h3 =
      test "{3\n\nFoo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subsubsection",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-0:\nBlank line is not allowed in '{3 ...}' (section heading)."
          ]
        } |}]

    let trailing_whitespace =
      test "{2 Foo }";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let trailing_newline =
      test "{2 Foo\n}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let trailing_blank_line =
      test "{2 Foo\n\n}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo-" ] },
                [ { "`Word": "Foo" }, "`Space" ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-0:\nBlank line is not allowed in '{2 ...}' (section heading)."
          ]
        } |}]

    let nested_markup =
      test "{2 [foo]}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Code_span": "foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let nested_code_with_uppercase =
      test "{2 [Foo]}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Code_span": "Foo" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let nested_code_with_spaces =
      test "{2 [ foo bar  baz  \t]}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "-foo-bar--baz---" ] },
                [ { "`Code_span": " foo bar  baz  \t" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let nested_code_with_newline =
      test "{2 [foo\nbar\r\nbaz]}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo-bar--baz" ] },
                [ { "`Code_span": "foo\nbar\r\nbaz" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let nested_style =
      test "{2 {e foo bar}}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo-bar" ] },
                [
                  {
                    "`Styled": [
                      "`Emphasis",
                      [ { "`Word": "foo" }, "`Space", { "`Word": "bar" } ]
                    ]
                  }
                ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let words =
      test "{2 foo bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo-bar" ] },
                [ { "`Word": "foo" }, "`Space", { "`Word": "bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let nested_heading =
      test "{2 {2 Foo}}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "" ] },
                []
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            { "`Paragraph": [ { "`Word": "}" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 3-5:\n'{2 ...}' (section heading) is not allowed in '{2 ...}' (section heading).",
            "File \"f.ml\", line 1, characters 0-2:\n'{2 ...}' (section heading) should not be empty.",
            "File \"f.ml\", line 1, characters 3-5:\n'{2 ...}' (section heading) should begin on its own line.",
            "File \"f.ml\", line 1, characters 10-11:\nUnpaired '}' (end of markup).\nSuggestion: try '\\}'."
          ]
        } |}]

    let in_list =
      test "- {2 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [ [ { "`Paragraph": [ { "`Word": "Foo" } ] } ] ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-4:\n'{2 ...}' (section heading) is not allowed in '-' (bulleted list item).\nSuggestion: move '{2' outside of any other markup."
          ]
        } |}]

    let followed_by_junk =
      test "{2 Foo} bar";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            { "`Paragraph": [ { "`Word": "bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 8-11:\nParagraph should begin on its own line."
          ]
        } |}]

    let preceded_by_junk =
      test "foo {2 Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Word": "foo" }, "`Space" ] },
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 4-6:\n'{2 ...}' (section heading) should begin on its own line."
          ]
        } |}]

    let followed_by_block =
      test "{2 Foo}\nbar";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            { "`Paragraph": [ { "`Word": "bar" } ] }
          ],
          "warnings": []
        } |}]

    let preceded_by_block =
      test "foo\n{2 Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Word": "foo" } ] },
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let label =
      test "{2:foo Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "true" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let whitespace_before_colon =
      test "{2 :foo Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, ":foo-bar" ] },
                [ { "`Word": ":foo" }, "`Space", { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let whitespace_after_colon =
      test "{2: foo Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo-bar" ] },
                [ { "`Word": "foo" }, "`Space", { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-3:\nHeading label should not be empty."
          ]
        } |}]

    let label_only =
      test "{2:foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "true" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                []
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-6:\n'{2 ...}' (section heading) should not be empty."
          ]
        } |}]

    let label_only_with_whitespace =
      test "{2:foo }";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "true" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                []
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-6:\n'{2 ...}' (section heading) should not be empty."
          ]
        } |}]

    let in_list_outside_item =
      test "{ul {2 Foo}}";
      [%expect
        {|
        {
          "value": [
            { "`List": [ "`Unordered", [] ] },
            { "`Paragraph": [ { "`Word": "}" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 4-6:\n'{2 ...}' (section heading) is not allowed in '{ul ...}' (bulleted list).\nSuggestion: move '{2 ...}' (section heading) outside the list.",
            "File \"f.ml\", line 1, characters 7-10:\n'Foo' is not allowed in '{ul ...}' (bulleted list).\nSuggestion: move 'Foo' into a list item, '{li ...}' or '{- ...}'.",
            "File \"f.ml\", line 1, characters 0-3:\n'{ul ...}' (bulleted list) should not be empty.",
            "File \"f.ml\", line 1, characters 11-12:\nUnpaired '}' (end of markup).\nSuggestion: try '\\}'."
          ]
        } |}]

    let preceded_by_shorthand_list =
      test "- foo\n{2 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [ [ { "`Paragraph": [ { "`Word": "foo" } ] } ] ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let nested_in_two_lists =
      test "{ul {li - foo\n{2 Bar}}}";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [
                  [
                    {
                      "`List": [
                        "`Unordered",
                        [ [ { "`Paragraph": [ { "`Word": "foo" } ] } ] ]
                      ]
                    },
                    { "`Paragraph": [ { "`Word": "Bar" } ] }
                  ]
                ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-2:\n'{2 ...}' (section heading) is not allowed in '{li ...}' (list item).\nSuggestion: move '{2' outside of any other markup."
          ]
        } |}]

    let bad_level_long_number =
      test "{22 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-8:\n'22': bad heading level (0-5 allowed)."
          ]
        } |}]

    let bad_level_long_number_with_label =
      test "{22:foo Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "true"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-12:\n'22': bad heading level (0-5 allowed)."
          ]
        } |}]

    let bad_level_leading_zero =
      test "{02 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 1-3:\n'02': leading zero in heading level."
          ]
        } |}]

    let bad_level_leading_zero_with_label =
      test "{02:foo Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "true" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 1-7:\n'02': leading zero in heading level."
          ]
        } |}]

    let bad_level_title =
      test "{0 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Title", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\nTitle-level headings {0 ...} are only allowed in pages."
          ]
        } |}]

    let bad_level_too_deep =
      test "{6 Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                {
                  "heading_level": "`Subparagraph",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\n'6': bad heading level (0-5 allowed)."
          ]
        } |}]

    let link_in_markup =
      test "{2 {{:foo}}}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "" ] },
                [ { "`Styled": [ "`Emphasis", [] ] } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 3-11:\n'{{:...} ...}' (external link) is not allowed in '{2 ...}' (section heading)."
          ]
        } |}]

    let reference_in_markup =
      test "{2 {!foo}}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "" ] },
                [ { "`Styled": [ "`Emphasis", [] ] } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 3-9:\n'{!...}' (cross-reference) is not allowed in '{2 ...}' (section heading)."
          ]
        } |}]

    let two =
      test "{2 Foo}\n{2 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]

    let greater =
      test "{2 Foo}\n{3 Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            {
              "`Heading": [
                {
                  "heading_level": "`Subsubsection",
                  "heading_label_explicit": "false"
                },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": []
        } |}]
  end in
  ()

let%expect_test _ =
  let module Author = struct
    let basic =
      test "@author Foo Bar";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo Bar" } } ], "warnings": [] } |}]

    let empty =
      test "@author";
      [%expect
        {|
        {
          "value": [ { "`Tag": { "`Author": "" } } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\n'@author' should not be empty."
          ]
        } |}]

    let whitespace_only =
      test "@author";
      [%expect
        {|
        {
          "value": [ { "`Tag": { "`Author": "" } } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-7:\n'@author' should not be empty."
          ]
        } |}]

    let extra_whitespace =
      test "@author  Foo Bar";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo Bar" } } ], "warnings": [] } |}]

    let newline =
      test "@author Foo Bar";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo Bar" } } ], "warnings": [] } |}]

    let cr_lf =
      test "@author Foo Bar";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo Bar" } } ], "warnings": [] } |}]

    let blank_line =
      test "@author Foo Bar";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo Bar" } } ], "warnings": [] } |}]

    let followed_by_junk =
      test "@author Foo\nbar";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            { "`Paragraph": [ { "`Word": "bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-3:\nParagraph is not allowed in the tags section.\nSuggestion: move 'bar' before any tags."
          ]
        } |}]

    let followed_by_code_span =
      test "@author Foo\n[bar]";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            { "`Paragraph": [ { "`Code_span": "bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-5:\nParagraph is not allowed in the tags section.\nSuggestion: move '[...]' (code) before any tags."
          ]
        } |}]

    let followed_by_code_block =
      test "@author Foo\n{[bar]}";
      [%expect
        {|
        {
          "value": [ { "`Tag": { "`Author": "Foo" } }, { "`Code_block": "bar" } ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-7:\n'{[...]}' (code block) is not allowed in the tags section.\nSuggestion: move '{[...]}' (code block) before any tags."
          ]
        } |}]

    let followed_by_verbatim =
      test "@author Foo\n{v bar v}";
      [%expect
        {|
        {
          "value": [ { "`Tag": { "`Author": "Foo" } }, { "`Verbatim": "bar" } ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-9:\n'{v ... v}' (verbatim text) is not allowed in the tags section.\nSuggestion: move '{v ... v}' (verbatim text) before any tags."
          ]
        } |}]

    let followed_by_modules =
      test "@author foo\n{!modules:Foo}";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "foo" } },
            { "`Modules": [ [ { "`Root": [ "Foo", "`TUnknown" ] }, "None" ] ] }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-14:\n'{!modules ...}' is not allowed in the tags section.\nSuggestion: move '{!modules ...}' before any tags."
          ]
        } |}]

    let followed_by_list =
      test "@author Foo\n{ul {li bar}}";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            {
              "`List": [
                "`Unordered",
                [ [ { "`Paragraph": [ { "`Word": "bar" } ] } ] ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-3:\n'{ul ...}' (bulleted list) is not allowed in the tags section.\nSuggestion: move '{ul ...}' (bulleted list) before any tags."
          ]
        } |}]

    let followed_by_shorthand_list =
      test "@author Foo\n- bar";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            {
              "`List": [
                "`Unordered",
                [ [ { "`Paragraph": [ { "`Word": "bar" } ] } ] ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-1:\n'-' (bulleted list item) is not allowed in the tags section.\nSuggestion: move '-' (bulleted list item) before any tags."
          ]
        } |}]

    let followed_by_section_heading =
      test "@author Foo\n{2 Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "bar" ] },
                [ { "`Word": "Bar" } ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-2:\n'{2 ...}' (section heading) is not allowed in the tags section.\nSuggestion: move '{2 ...}' (section heading) before any tags."
          ]
        } |}]

    let followed_by_author =
      test "@author Foo\n@author Bar";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": []
        } |}]

    let followed_by_author_cr_lf =
      test "@author Foo\n@author Bar";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": []
        } |}]

    let in_author =
      test "@author Foo @author Bar";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo @author Bar" } } ], "warnings": [] } |}]

    let in_author_at_start =
      test "@author @author Foo";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "@author Foo" } } ], "warnings": [] } |}]

    let preceded_by_paragraph =
      test "foo\n@author Bar";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Word": "foo" } ] },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": []
        } |}]

    let no_markup =
      test "@author Foo [Bar]";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo [Bar]" } } ], "warnings": [] } |}]

    let in_paragraph =
      test "foo @author Bar";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Word": "foo" }, "`Space" ] },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 4-15:\n'@author' should begin on its own line."
          ]
        } |}]

    let in_code =
      test "[@author Foo]";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "@author Foo" } ] } ],
          "warnings": []
        } |}]

    let in_style =
      test "{b @author Foo}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Styled": [ "`Bold", [] ] } ] },
            { "`Tag": { "`Author": "Foo}" } }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 3-15:\n'@author' is not allowed in '{b ...}' (boldface text).",
            "File \"f.ml\", line 1, characters 0-2:\n'{b ...}' (boldface text) should not be empty.",
            "File \"f.ml\", line 1, characters 3-15:\n'@author' should begin on its own line."
          ]
        } |}]

    let in_heading =
      test "{2 @author Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "" ] },
                []
              ]
            },
            { "`Tag": { "`Author": "Foo}" } }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 3-15:\n'@author' is not allowed in '{2 ...}' (section heading).",
            "File \"f.ml\", line 1, characters 0-2:\n'{2 ...}' (section heading) should not be empty.",
            "File \"f.ml\", line 1, characters 3-15:\n'@author' should begin on its own line."
          ]
        } |}]

    let after_shorthand_list =
      test "- foo\n@author Bar";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [ [ { "`Paragraph": [ { "`Word": "foo" } ] } ] ]
              ]
            },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": []
        } |}]

    let in_shorthand_list =
      test "- foo @author Bar";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [
                  [
                    { "`Paragraph": [ { "`Word": "foo" }, "`Space" ] },
                    {
                      "`Paragraph": [
                        { "`Word": "@author" },
                        "`Space",
                        { "`Word": " Bar" }
                      ]
                    }
                  ]
                ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-17:\n'@author' is not allowed in '-' (bulleted list item).\nSuggestion: move '@author' outside of any other markup."
          ]
        } |}]

    let in_shorthand_list_at_start =
      test "- @author Foo";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [
                  [
                    {
                      "`Paragraph": [
                        { "`Word": "@author" },
                        "`Space",
                        { "`Word": " Foo" }
                      ]
                    }
                  ]
                ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-13:\n'@author' is not allowed in '-' (bulleted list item).\nSuggestion: move '@author' outside of any other markup."
          ]
        } |}]

    let in_list_item =
      test "{ul {li foo @author Bar}}";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [
                  [
                    { "`Paragraph": [ { "`Word": "foo" }, "`Space" ] },
                    {
                      "`Paragraph": [
                        { "`Word": "@author" },
                        "`Space",
                        { "`Word": " Bar}}" }
                      ]
                    }
                  ]
                ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 12-25:\n'@author' is not allowed in '{li ...}' (list item).\nSuggestion: move '@author' outside of any other markup.",
            "File \"f.ml\", line 1, characters 25-25:\nEnd of text is not allowed in '{li ...}' (list item).",
            "File \"f.ml\", line 1, characters 25-25:\nEnd of text is not allowed in '{ul ...}' (bulleted list)."
          ]
        } |}]

    let in_list_item_at_start =
      test "{ul {li @author Foo}}";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [
                  [
                    {
                      "`Paragraph": [
                        { "`Word": "@author" },
                        "`Space",
                        { "`Word": " Foo}}" }
                      ]
                    }
                  ]
                ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 8-21:\n'@author' is not allowed in '{li ...}' (list item).\nSuggestion: move '@author' outside of any other markup.",
            "File \"f.ml\", line 1, characters 21-21:\nEnd of text is not allowed in '{li ...}' (list item).",
            "File \"f.ml\", line 1, characters 21-21:\nEnd of text is not allowed in '{ul ...}' (bulleted list)."
          ]
        } |}]

    let in_list_item_on_new_line =
      test "{ul {li foo\n@author Bar}}";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [
                  [
                    { "`Paragraph": [ { "`Word": "foo" } ] },
                    {
                      "`Paragraph": [
                        { "`Word": "@author" },
                        "`Space",
                        { "`Word": " Bar}}" }
                      ]
                    }
                  ]
                ]
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 2, characters 0-13:\n'@author' is not allowed in '{li ...}' (list item).\nSuggestion: move '@author' outside of any other markup.",
            "File \"f.ml\", line 2, characters 13-13:\nEnd of text is not allowed in '{li ...}' (list item).",
            "File \"f.ml\", line 2, characters 13-13:\nEnd of text is not allowed in '{ul ...}' (bulleted list)."
          ]
        } |}]

    let in_list =
      test "{ul @author Foo}";
      [%expect
        {|
        {
          "value": [ { "`List": [ "`Unordered", [] ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 4-16:\n'@author' is not allowed in '{ul ...}' (bulleted list).\nSuggestion: move '@author' outside the list.",
            "File \"f.ml\", line 1, characters 16-16:\nEnd of text is not allowed in '{ul ...}' (bulleted list).",
            "File \"f.ml\", line 1, characters 0-3:\n'{ul ...}' (bulleted list) should not be empty."
          ]
        } |}]

    let in_code_block =
      test "{[@author Foo]}";
      [%expect
        {|
          { "value": [ { "`Code_block": "@author Foo" } ], "warnings": [] } |}]

    let in_verbatim =
      test "{v @author Foo v}";
      [%expect
        {|
          { "value": [ { "`Verbatim": "@author Foo" } ], "warnings": [] } |}]

    let after_code_block =
      test "{[foo]} @author Bar";
      [%expect
        {|
        {
          "value": [ { "`Code_block": "foo" }, { "`Tag": { "`Author": "Bar" } } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 8-19:\n'@author' should begin on its own line."
          ]
        } |}]

    let after_verbatim =
      test "{v foo v} @author Bar";
      [%expect
        {|
        {
          "value": [ { "`Verbatim": "foo" }, { "`Tag": { "`Author": "Bar" } } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 10-21:\n'@author' should begin on its own line."
          ]
        } |}]

    let after_heading =
      test "{2 Foo} @author Bar";
      [%expect
        {|
        {
          "value": [
            {
              "`Heading": [
                { "heading_level": "`Subsection", "heading_label_explicit": "false" },
                { "`Label": [ { "`Page": [ "None", "f.ml" ] }, "foo" ] },
                [ { "`Word": "Foo" } ]
              ]
            },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 8-19:\n'@author' should begin on its own line."
          ]
        } |}]

    let after_list =
      test "{ul {li foo}} @author Bar";
      [%expect
        {|
        {
          "value": [
            {
              "`List": [
                "`Unordered",
                [ [ { "`Paragraph": [ { "`Word": "foo" } ] } ] ]
              ]
            },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 14-25:\n'@author' should begin on its own line."
          ]
        } |}]

    let preceded_by_whitespace =
      test "@author Foo Bar";
      [%expect
        {|
          { "value": [ { "`Tag": { "`Author": "Foo Bar" } } ], "warnings": [] } |}]

    let second_preceded_by_whitespace =
      test "@author Foo\n @author Bar";
      [%expect
        {|
        {
          "value": [
            { "`Tag": { "`Author": "Foo" } },
            { "`Tag": { "`Author": "Bar" } }
          ],
          "warnings": []
        } |}]

    let prefix =
      test "@authorfoo";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Word": "@authorfoo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-10:\nUnknown tag '@authorfoo'."
          ]
        } |}]
  end in
  ()

let%expect_test _ =
  let module Reference_component_kind = struct
    let no_kind_with_quotes =
      test "{!\"foo\".\"bar\"}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let no_kind =
      test "{!foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TUnknown" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let class_ =
      test "{!class-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TClass" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let class_type =
      test "{!class-type-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TClassType" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let class_type_alt =
      test "{!classtype-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TClassType" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\n'classtype' is deprecated, use 'class-type' instead."
          ]
        } |}]

    let constructor =
      test "{!constructor-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TConstructor" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let constructor_alt =
      test "{!const-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TConstructor" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\n'const' is deprecated, use 'constructor' instead."
          ]
        } |}]

    let dash_in_page_name =
      test "{!page-\"foo-bar\"}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo-bar", "`TPage" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let dot_and_dash_in_page_name =
      test "{!page-\"foo-bar.v0.0.1\"}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo-bar.v0.0.1", "`TPage" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let exception_ =
      test "{!exception-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TException" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let exception_alt =
      test "{!exn-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TException" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\n'exn' is deprecated, use 'exception' instead."
          ]
        } |}]

    let extension =
      test "{!extension-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TExtension" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field =
      test "{!field-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TField" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_alt =
      test "{!recfield-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TField" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-14:\n'recfield' is deprecated, use 'field' instead."
          ]
        } |}]

    let heading =
      test "{!section-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TLabel" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let heading_alt =
      test "{!label-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TLabel" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\n'label' is deprecated, use 'section' instead."
          ]
        } |}]

    let instance_variable =
      test "{!instance-variable-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [ { "`Root": [ "foo", "`TInstanceVariable" ] }, [] ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let kind_with_quotes =
      test "{!module-type-\"Bar\".module-\"Moo\".class-\"There\"}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Class": [
                        {
                          "`Module": [
                            { "`Root": [ "Bar", "`TModuleType" ] },
                            "Moo"
                          ]
                        },
                        "There"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let method_ =
      test "{!method-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TMethod" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_ =
      test "{!module-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TModule" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_type =
      test "{!module-type-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TModuleType" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_type_alt =
      test "{!modtype-Foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "Foo", "`TModuleType" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-13:\n'modtype' is deprecated, use 'module-type' instead."
          ]
        } |}]

    let page =
      test "{!page-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TPage" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let type_ =
      test "{!type-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TType" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let val_ =
      test "{!val-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let val_alt =
      test "{!value-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\n'value' is deprecated, use 'val' instead."
          ]
        } |}]

    let longident =
      test "{!module-Foo.type-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Type": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let hyphenated_kind_longident =
      test "{!module-type-Foo.module-type-Bar.type-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Type": [
                        {
                          "`ModuleType": [
                            { "`Root": [ "Foo", "`TModuleType" ] },
                            "Bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let empty =
      test "{!}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let empty_qualifier =
      test "{!-foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "-foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-6:\nUnknown reference qualifier ''."
          ]
        } |}]

    let empty_identifier =
      test "{!val-}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val-" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-6:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let invalid_qualifier =
      test "{!foo-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nUnknown reference qualifier 'foo'."
          ]
        } |}]

    let empty_first_component =
      test "{!.foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": ".foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let empty_second_component =
      test "{!Foo.}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "Foo." } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-6:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let second_component_empty_qualifier =
      test "{!Foo.-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "Foo.-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-10:\nUnknown reference qualifier ''."
          ]
        } |}]

    let second_component_empty_identifier =
      test "{!Foo.val-}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "Foo.val-" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 10-10:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let first_component_empty_identifier =
      test "{!module-.foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "module-.foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 9-9:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let something_in_invalid =
      test "{!foo-bar.baz}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo-bar.baz" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nUnknown reference qualifier 'foo'."
          ]
        } |}]

    let something_in_something =
      test "{!foo.bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_module =
      test "{!module-Foo.bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_module_type =
      test "{!module-type-Foo.bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "Foo", "`TModuleType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_type =
      test "{!type-foo.bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "foo", "`TType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_class =
      test "{!class-foo.bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "foo", "`TClass" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_class_type =
      test "{!class-type-foo.bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "foo", "`TClassType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_page =
      test "{!page-foo.bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Dot": [ { "`Root": [ "foo", "`TPage" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_constructor =
      test "{!constructor-Foo.bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "constructor-Foo.bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-17:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_exception =
      test "{!exception-Foo.bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "exception-Foo.bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_extension =
      test "{!extension-Foo.bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "extension-Foo.bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_field =
      test "{!field-foo.bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "field-foo.bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_section =
      test "{!section-foo.bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "section-foo.bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-13:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_instance_variable =
      test "{!instance-variable-foo.bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "instance-variable-foo.bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-23:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_method =
      test "{!method-foo.bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "method-foo.bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_val =
      test "{!val-foo.bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val-foo.bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', 'page-', or an unqualified reference."
          ]
        } |}]

    let something_in_something_nested =
      test "{!foo.bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_module_nested =
      test "{!Foo.module-Bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Module": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_module_type_nested =
      test "{!Foo.module-type-Bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        {
                          "`ModuleType": [
                            { "`Root": [ "Foo", "`TUnknown" ] },
                            "Bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_type_nested =
      test "{!Foo.type-bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Type": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_class_nested =
      test "{!Foo.class-bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Class": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_class_type_nested =
      test "{!foo.class-type-bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        {
                          "`ClassType": [
                            { "`Root": [ "foo", "`TUnknown" ] },
                            "bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let something_in_page_nested =
      test "{!foo.page-bar.baz}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo.page-bar.baz" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_constructor_nested =
      test "{!Foo.constructor-Bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.constructor-Bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-21:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_exception_nested =
      test "{!Foo.exception-bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.exception-bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_extension_nested =
      test "{!Foo.extension-bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.extension-bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_field_nested =
      test "{!foo.field-bar.baz}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo.field-bar.baz" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-15:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_section_nested =
      test "{!foo.section-bar.baz}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo.section-bar.baz" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-17:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.instance-variable-bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-27:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_method_nested =
      test "{!foo.method-bar.baz}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo.method-bar.baz" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-16:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let something_in_val_nested =
      test "{!Foo.val-bar.baz}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "Foo.val-bar.baz" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-13:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_empty =
      test "{!.module-Foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": ".module-Foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let module_in_something =
      test "{!Foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Module": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_in_module =
      test "{!module-Foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Module": [ { "`Root": [ "Foo", "`TModule" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_in_module_type =
      test "{!module-type-Foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Module": [ { "`Root": [ "Foo", "`TModuleType" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_in_class =
      test "{!class-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "class-foo.module-Bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_class_type =
      test "{!class-type-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-type-foo.module-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-16:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_constructor =
      test "{!constructor-Foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "constructor-Foo.module-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-17:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_exception =
      test "{!exception-Foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "exception-Foo.module-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_extension =
      test "{!extension-Foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "extension-Foo.module-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_field =
      test "{!field-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "field-foo.module-Bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_section =
      test "{!section-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "section-foo.module-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-13:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_instance_variable =
      test "{!instance-variable-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [ { "`Code_span": "instance-variable-foo.module-Bar" } ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-23:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_method =
      test "{!method-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "method-foo.module-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_page =
      test "{!page-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "page-foo.module-Bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_type =
      test "{!type-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "type-foo.module-Bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_val =
      test "{!val-foo.module-Bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val-foo.module-Bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_something_nested =
      test "{!Foo.Bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Module": [
                        { "`Dot": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                        "Baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_in_module_nested =
      test "{!Foo.module-Bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Module": [
                        { "`Module": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                        "Baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_in_module_type_nested =
      test "{!Foo.module-type-Bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Module": [
                        {
                          "`ModuleType": [
                            { "`Root": [ "Foo", "`TUnknown" ] },
                            "Bar"
                          ]
                        },
                        "Baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_in_class_nested =
      test "{!Foo.class-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.class-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-15:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_class_type_nested =
      test "{!Foo.class-type-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.class-type-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-20:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_constructor_nested =
      test "{!Foo.constructor-Bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.constructor-Bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-21:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_exception_nested =
      test "{!Foo.exception-Bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.exception-Bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_extension_nested =
      test "{!Foo.extension-Bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.extension-Bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_field_nested =
      test "{!foo.field-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.field-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-15:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_section_nested =
      test "{!foo.section-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.section-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-17:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "foo.instance-variable-bar.module-Baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-27:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_method_nested =
      test "{!foo.method-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.method-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-16:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_page_nested =
      test "{!foo.page-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.page-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_type_nested =
      test "{!Foo.type-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.type-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_in_val_nested =
      test "{!Foo.val-bar.module-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.val-bar.module-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-13:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_type_in_something =
      test "{!Foo.module-type-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`ModuleType": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_type_in_module =
      test "{!module-Foo.module-type-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`ModuleType": [ { "`Root": [ "Foo", "`TModule" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_type_in_module_type =
      test "{!module-type-Foo.module-type-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`ModuleType": [
                        { "`Root": [ "Foo", "`TModuleType" ] },
                        "Bar"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let module_type_in_class =
      test "{!class-foo.module-type-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.module-type-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let module_type_in_page =
      test "{!page-foo.module-type-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.module-type-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let type_in_something =
      test "{!Foo.type-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Type": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let type_in_module =
      test "{!module-Foo.type-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Type": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let type_in_module_type =
      test "{!module-type-Foo.type-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Type": [ { "`Root": [ "Foo", "`TModuleType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let type_in_class =
      test "{!class-foo.type-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "class-foo.type-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let type_in_page =
      test "{!page-foo.type-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "page-foo.type-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let constructor_in_empty =
      test "{!.constructor-Foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": ".constructor-Foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let constructor_in_something =
      test "{!foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Constructor": [ { "`Root": [ "foo", "`TUnknown" ] }, "Bar" ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let constructor_in_type =
      test "{!type-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Constructor": [ { "`Root": [ "foo", "`TType" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let constructor_in_class =
      test "{!class-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_class_type =
      test "{!class-type-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-type-foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-16:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_constructor =
      test "{!constructor-Foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "constructor-Foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-17:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_exception =
      test "{!exception-Foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "exception-Foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_extension =
      test "{!extension-Foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "extension-Foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_field =
      test "{!field-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "field-foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_section =
      test "{!section-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "section-foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-13:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_instance_variable =
      test "{!instance-variable-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "instance-variable-foo.constructor-Bar" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-23:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_method =
      test "{!method-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "method-foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_module =
      test "{!module-Foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "module-Foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_module_type =
      test "{!module-type-Foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "module-type-Foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-17:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_page =
      test "{!page-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_val =
      test "{!val-foo.constructor-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "val-foo.constructor-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_something_nested =
      test "{!foo.bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Constructor": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "Baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let constructor_in_type_nested =
      test "{!foo.type-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Constructor": [
                        { "`Type": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "Baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let constructor_in_class_nested =
      test "{!Foo.class-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.class-bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-15:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_class_type_nested =
      test "{!Foo.class-type-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "Foo.class-type-bar.constructor-Baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-20:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_constructor_nested =
      test "{!Foo.constructor-Bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "Foo.constructor-Bar.constructor-Baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-21:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_exception_nested =
      test "{!Foo.exception-Bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [ { "`Code_span": "Foo.exception-Bar.constructor-Baz" } ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_extension_nested =
      test "{!Foo.extension-Bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [ { "`Code_span": "Foo.extension-Bar.constructor-Baz" } ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_field_nested =
      test "{!foo.field-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.field-bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-15:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_section_nested =
      test "{!foo.section-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.section-bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-17:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "foo.instance-variable-bar.constructor-Baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-27:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_method_nested =
      test "{!foo.method-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.method-bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-16:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_module_nested =
      test "{!Foo.module-Bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.module-Bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-16:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_module_type_nested =
      test "{!Foo.module-type-Bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "Foo.module-type-Bar.constructor-Baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-21:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_page_nested =
      test "{!foo.page-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.page-bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let constructor_in_val_nested =
      test "{!Foo.val-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.val-bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-13:\nExpected 'type-' or an unqualified reference."
          ]
        } |}]

    let field_in_empty =
      test "{!.field-foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": ".field-foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let field_in_something =
      test "{!foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Field": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_module =
      test "{!module-Foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Field": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_module_type =
      test "{!module-type-Foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Field": [ { "`Root": [ "Foo", "`TModuleType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_type =
      test "{!type-foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Field": [ { "`Root": [ "foo", "`TType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_class =
      test "{!class-foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Field": [ { "`Root": [ "foo", "`TClass" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_class_type =
      test "{!class-type-foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Field": [ { "`Root": [ "foo", "`TClassType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_constructor =
      test "{!constructor-Foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "constructor-Foo.field-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-17:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_exception =
      test "{!exception-Foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "exception-Foo.field-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_extension =
      test "{!extension-Foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "extension-Foo.field-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_field =
      test "{!field-foo.field-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "field-foo.field-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_section =
      test "{!section-foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "section-foo.field-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-13:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_instance_variable =
      test "{!instance-variable-foo.field-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "instance-variable-foo.field-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-23:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_method =
      test "{!method-foo.field-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "method-foo.field-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_page =
      test "{!page-foo.field-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "page-foo.field-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_val =
      test "{!val-foo.field-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val-foo.field-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_something_nested =
      test "{!foo.bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_module_nested =
      test "{!Foo.module-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Module": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_module_type_nested =
      test "{!Foo.module-type-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        {
                          "`ModuleType": [
                            { "`Root": [ "Foo", "`TUnknown" ] },
                            "Bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_type_nested =
      test "{!Foo.type-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Type": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_class_nested =
      test "{!Foo.class-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Class": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_class_type_nested =
      test "{!Foo.class-type-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        {
                          "`ClassType": [
                            { "`Root": [ "Foo", "`TUnknown" ] },
                            "bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let field_in_constructor_nested =
      test "{!Foo.constructor-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.constructor-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-21:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_exception_nested =
      test "{!Foo.exception-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.exception-Bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_extension_nested =
      test "{!Foo.extension-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.extension-Bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_field_nested =
      test "{!Foo.field-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.field-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-15:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_section_nested =
      test "{!foo.section-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.section-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-17:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "foo.instance-variable-bar.field-baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-27:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_method_nested =
      test "{!foo.method-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.method-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-16:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_page_nested =
      test "{!foo.page-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.page-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let field_in_val_nested =
      test "{!Foo.val-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.val-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-13:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let exception_in_something =
      test "{!Foo.exception-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Exception": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let exception_in_module =
      test "{!module-Foo.exception-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Exception": [ { "`Root": [ "Foo", "`TModule" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let exception_in_class =
      test "{!class-foo.exception-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.exception-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let exception_in_page =
      test "{!page-foo.exception-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.exception-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let extension_in_something =
      test "{!Foo.extension-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Extension": [ { "`Root": [ "Foo", "`TUnknown" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let extension_in_module =
      test "{!module-Foo.extension-Bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Extension": [ { "`Root": [ "Foo", "`TModule" ] }, "Bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let extension_in_class =
      test "{!class-foo.extension-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.extension-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let extension_in_page =
      test "{!page-foo.extension-Bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.extension-Bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let val_in_something =
      test "{!Foo.val-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Value": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let val_in_module =
      test "{!module-Foo.val-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Value": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let val_in_class =
      test "{!class-foo.val-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "class-foo.val-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let val_in_page =
      test "{!page-foo.val-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "page-foo.val-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let class_in_something =
      test "{!Foo.class-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Class": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let class_in_module =
      test "{!module-Foo.class-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Class": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let class_in_class =
      test "{!class-foo.class-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "class-foo.class-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let class_in_page =
      test "{!page-foo.class-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "page-foo.class-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let class_type_in_something =
      test "{!Foo.class-type-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`ClassType": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let class_type_in_module =
      test "{!module-Foo.class-type-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`ClassType": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let class_type_in_class =
      test "{!class-foo.class-type-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.class-type-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let class_type_in_page =
      test "{!page-foo.class-type-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.class-type-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_empty =
      test "{!.method-foo}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": ".method-foo" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-2:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let method_in_something =
      test "{!foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Method": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let method_in_class =
      test "{!class-foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Method": [ { "`Root": [ "foo", "`TClass" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let method_in_class_type =
      test "{!class-type-foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Method": [ { "`Root": [ "foo", "`TClassType" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let method_in_constructor =
      test "{!constructor-Foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "constructor-Foo.method-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-17:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_exception =
      test "{!exception-Foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "exception-Foo.method-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_extension =
      test "{!extension-Foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "extension-Foo.method-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_field =
      test "{!field-foo.method-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "field-foo.method-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_section =
      test "{!section-foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "section-foo.method-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-13:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_instance_variable =
      test "{!instance-variable-foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [ { "`Code_span": "instance-variable-foo.method-bar" } ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-23:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_method =
      test "{!method-foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "method-foo.method-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_module =
      test "{!module-Foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "module-Foo.method-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_module_type =
      test "{!module-type-Foo.method-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "module-type-Foo.method-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-17:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_page =
      test "{!page-foo.method-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "page-foo.method-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_type =
      test "{!type-foo.method-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "type-foo.method-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_val =
      test "{!val-foo.method-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "val-foo.method-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_something_nested =
      test "{!foo.bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Method": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let method_in_class_nested =
      test "{!Foo.class-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Method": [
                        { "`Class": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let method_in_class_type_nested =
      test "{!Foo.class-type-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Method": [
                        {
                          "`ClassType": [
                            { "`Root": [ "Foo", "`TUnknown" ] },
                            "bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let method_in_constructor_nested =
      test "{!foo.constructor-Bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.constructor-Bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-21:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_exception_nested =
      test "{!Foo.exception-Bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.exception-Bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_extension_nested =
      test "{!Foo.extension-Bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.extension-Bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-19:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_field_nested =
      test "{!foo.field-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.field-bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-15:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_section_nested =
      test "{!foo.section-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.section-bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-17:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "foo.instance-variable-bar.method-baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-27:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_method_nested =
      test "{!foo.method-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.method-bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-16:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_module_nested =
      test "{!Foo.module-Bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.module-Bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-16:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_module_type_nested =
      test "{!Foo.module-type-Bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.module-type-Bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-21:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_page_nested =
      test "{!foo.page-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "foo.page-bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_type_nested =
      test "{!Foo.type-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.type-bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let method_in_val_nested =
      test "{!Foo.val-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "Foo.val-bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-13:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let instance_variable_in_something =
      test "{!Foo.instance-variable-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`InstanceVariable": [
                        { "`Root": [ "Foo", "`TUnknown" ] },
                        "bar"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let instance_variable_in_module =
      test "{!module-Foo.instance-variable-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [ { "`Code_span": "module-Foo.instance-variable-bar" } ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-12:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let instance_variable_in_class =
      test "{!class-foo.instance-variable-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`InstanceVariable": [
                        { "`Root": [ "foo", "`TClass" ] },
                        "bar"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let instance_variable_in_page =
      test "{!page-foo.instance-variable-bar}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.instance-variable-bar" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let section_in_something =
      test "{!Foo.section-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Label": [ { "`Root": [ "Foo", "`TUnknown" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let section_in_module =
      test "{!module-Foo.section-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Label": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let section_in_class =
      test "{!class-foo.section-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Label": [ { "`Root": [ "foo", "`TClass" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let section_in_page =
      test "{!page-foo.section-bar}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    { "`Label": [ { "`Root": [ "foo", "`TPage" ] }, "bar" ] },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let page_in_something =
      test "{!foo.page-bar}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo.page-bar" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nPage label is not allowed in the last component of a reference path.\nSuggestion: 'page-bar' should be first."
          ]
        } |}]

    let inner_parent_something_in_something =
      test "{!foo.bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_something_in_module =
      test "{!module-Foo.bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Dot": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_something_in_class =
      test "{!class-foo.bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Dot": [ { "`Root": [ "foo", "`TClass" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_something_in_page =
      test "{!page-foo.bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let inner_parent_module_in_module =
      test "{!module-Foo.module-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Module": [ { "`Root": [ "Foo", "`TModule" ] }, "Bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_module_in_class =
      test "{!class-foo.module-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.module-Bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_parent_module_type_in_module =
      test "{!module-Foo.module-type-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        {
                          "`ModuleType": [
                            { "`Root": [ "Foo", "`TModule" ] },
                            "Bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_module_type_in_class =
      test "{!class-foo.module-type-Bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "class-foo.module-type-Bar.field-baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_parent_type_in_module =
      test "{!module-Foo.type-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Type": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_type_in_class =
      test "{!class-foo.type-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.type-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_parent_class_in_module =
      test "{!module-Foo.class-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        { "`Class": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_class_in_class =
      test "{!class-foo.class-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.class-bar.field-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_parent_class_type_in_module =
      test "{!module-Foo.class-type-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Field": [
                        {
                          "`ClassType": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_parent_class_type_in_class =
      test "{!class-foo.class-type-bar.field-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "class-foo.class-type-bar.field-baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_label_parent_something_in_something =
      test "{!foo.bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_label_parent_something_in_page =
      test "{!page-foo.bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Dot": [ { "`Root": [ "foo", "`TPage" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_label_parent_module_in_module =
      test "{!module-Foo.module-Bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Module": [ { "`Root": [ "Foo", "`TModule" ] }, "Bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_label_parent_module_in_class =
      test "{!class-foo.module-Bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.module-Bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_label_parent_module_type_in_module =
      test "{!module-Foo.module-type-Bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        {
                          "`ModuleType": [
                            { "`Root": [ "Foo", "`TModule" ] },
                            "Bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_label_parent_module_type_in_class =
      test "{!class-foo.module-type-Bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.module-type-Bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_label_parent_type_in_module =
      test "{!module-Foo.type-bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Type": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_label_parent_type_in_class =
      test "{!class-foo.type-bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.type-bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_label_parent_class_in_module =
      test "{!module-Foo.class-bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Class": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_label_parent_class_in_class =
      test "{!class-foo.class-bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.class-bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_label_parent_class_type_in_module =
      test "{!module-Foo.class-bar.baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Class": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_label_parent_class_type_in_class =
      test "{!class-foo.class-bar.baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.class-bar.baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_page_in_something =
      test "{!foo.page-bar.baz}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "foo.page-bar.baz" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 6-14:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let inner_class_signature_something_in_something =
      test "{!foo.bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Method": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_class_signature_something_in_page =
      test "{!page-foo.bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let inner_class_signature_class_in_module =
      test "{!module-Foo.class-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Method": [
                        { "`Class": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_class_signature_class_in_class =
      test "{!class-foo.class-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.class-bar.method-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_class_signature_class_type_in_module =
      test "{!module-Foo.class-type-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Method": [
                        {
                          "`ClassType": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_class_signature_class_type_in_class =
      test "{!class-foo.class-type-bar.method-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "class-foo.class-type-bar.method-baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_signature_something_in_something =
      test "{!foo.bar.type-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Type": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_signature_something_in_page =
      test "{!page-foo.bar.type-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.bar.type-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let inner_signature_module_in_module =
      test "{!module-Foo.module-Bar.type-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Type": [
                        { "`Module": [ { "`Root": [ "Foo", "`TModule" ] }, "Bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_signature_module_in_class =
      test "{!class-foo.module-Bar.type-baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "class-foo.module-Bar.type-baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_signature_module_type_in_module =
      test "{!module-Foo.module-type-Bar.type-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Type": [
                        {
                          "`ModuleType": [
                            { "`Root": [ "Foo", "`TModule" ] },
                            "Bar"
                          ]
                        },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_signature_module_type_in_class =
      test "{!class-foo.module-type-Bar.type-baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "class-foo.module-type-Bar.type-baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let inner_datatype_something_in_something =
      test "{!foo.bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Constructor": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "Baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_datatype_something_in_page =
      test "{!page-foo.bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            { "`Paragraph": [ { "`Code_span": "page-foo.bar.constructor-Baz" } ] }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-10:\nExpected 'module-', 'module-type-', 'type-', 'class-', 'class-type-', or an unqualified reference."
          ]
        } |}]

    let inner_datatype_type_in_module =
      test "{!module-Foo.type-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Constructor": [
                        { "`Type": [ { "`Root": [ "Foo", "`TModule" ] }, "bar" ] },
                        "Baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let inner_datatype_type_in_class =
      test "{!class-foo.type-bar.constructor-Baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Code_span": "class-foo.type-bar.constructor-Baz" }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-11:\nExpected 'module-', 'module-type-', or an unqualified reference."
          ]
        } |}]

    let kind_conflict =
      test "{!val:type-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TType" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-15:\nOld-style reference kind ('val:') does not match new ('type-')."
          ]
        } |}]

    let kind_agreement =
      test "{!val:val-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": []
        } |}]

    let kind_agreement_alt =
      test "{!value:val-foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Reference": [ { "`Root": [ "foo", "`TValue" ] }, [] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-7:\n'value' is deprecated, use 'val' instead."
          ]
        } |}]

    let canonical_something =
      test "@canonical Foo";
      [%expect
        {|
        {
          "value": [],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-14:\nUnexpected tag '@canonical' at this location."
          ]
        } |}]

    let canonical_module =
      test "@canonical module-Foo";
      [%expect
        {|
        {
          "value": [],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-21:\nUnexpected tag '@canonical' at this location."
          ]
        } |}]

    let canonical_path =
      test "@canonical Foo.Bar";
      [%expect
        {|
        {
          "value": [],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-18:\nUnexpected tag '@canonical' at this location."
          ]
        } |}]

    let canonical_val =
      test "@canonical val-foo";
      [%expect
        {|
        {
          "value": [],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-18:\nUnexpected tag '@canonical' at this location."
          ]
        } |}]

    let canonical_bad_parent =
      test "@canonical bar.page-foo";
      [%expect
        {|
        {
          "value": [],
          "warnings": [
            "File \"f.ml\", line 1, characters 0-23:\nUnexpected tag '@canonical' at this location."
          ]
        } |}]

    let canonical_empty_component =
      test "@canonical .Foo";
      [%expect
        {|
        {
          "value": [],
          "warnings": [
            "File \"f.ml\", line 1, characters 11-15:\nExpected a valid path."
          ]
        } |}]

    let canonical_empty_name =
      test "@canonical Foo.";
      [%expect
        {|
        {
          "value": [],
          "warnings": [
            "File \"f.ml\", line 1, characters 11-15:\nExpected a valid path."
          ]
        } |}]

    let internal_whitespace =
      test "{!foo. bar .baz}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                {
                  "`Reference": [
                    {
                      "`Dot": [
                        { "`Dot": [ { "`Root": [ "foo", "`TUnknown" ] }, "bar" ] },
                        "baz"
                      ]
                    },
                    []
                  ]
                }
              ]
            }
          ],
          "warnings": []
        } |}]

    let replacement_text_empty_identifier =
      test "{{!val-} foo}";
      [%expect
        {|
        {
          "value": [
            {
              "`Paragraph": [
                { "`Styled": [ "`Emphasis", [ { "`Word": "foo" } ] ] }
              ]
            }
          ],
          "warnings": [
            "File \"f.ml\", line 1, characters 7-7:\nIdentifier in reference should not be empty."
          ]
        } |}]

    let reference_with_unmatched_quotation =
      test "{!\"\"foo\"}";
      [%expect
        {|
        {
          "value": [ { "`Paragraph": [ { "`Code_span": "\"\"foo\"" } ] } ],
          "warnings": [
            "File \"f.ml\", line 1, characters 2-9:\nUnmatched quotation!"
          ]
        } |}]
  end in
  ()
