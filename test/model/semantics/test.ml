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
          F ("value", fst, Indirect (fst, Comment_desc.elements));
          F ("warnings", snd, List warning_desc);
        ] )

let test ?(tags_allowed = true) ?(location = { Location_.line = 1; column = 0 })
    str =
  let filename = "f.ml" in
  let dummy_page =
    Paths.Identifier.Mk.page (None, Names.PageName.make_std filename)
  in
  let location =
    Expect_test_helper.lexing_position ~filename ~line:location.line
      ~column:location.column ()
  in
  let parser_output =
    Semantics.parse_comment ~internal_tags:Odoc_model.Semantics.Expect_none
      ~tags_allowed ~containing_definition:dummy_page ~location ~text:str
  in
  let yojson = Type_desc_to_yojson.to_yojson parser_output_desc parser_output in
  Expect_test_helper.print ~input:str ~output:(Yojson.Basic.to_string yojson)

[@@@ocaml.warning "-32"]

let simple_reference () =
  let module Simple_reference = struct
    let basic = test "{!foo}"

    let leading_whitespace = test "{! foo}"

    let trailing_whitespace = test "{!foo }"

    let adjacent_word_leading = test "bar{!foo}"

    let explicit_leading_space = test "bar {!foo}"

    let adjacent_word_trailing = test "{!foo}bar"

    let explicit_trailing_space = test "{!foo} bar"

    let kind = test "{!val:foo}"

    let empty = test "{!}"

    let whitespace_only = test "{! }"

    let internal_whitespace = test "{!( * )}"

    let internal_quoted_whitespace = test "{!\"( * )\"}"

    (* TODO Limiting the character combinations allowed will make it easier to
       catch expressions accidentally written inside references. This can also
       be caught by a good resolver and resolver error messages. *)
    (* t "expression" *)
    let unterminated = test "{!foo"

    let empty_kind = test "{!:foo}"

    let whitespace_kind = test "{! :foo}"

    let with_kind_but_empty = test "{!val:}"

    let with_kind_but_whitespace = test "{!val: }"

    let leading_whitespace_in_kind = test "{! val:foo}"

    let internal_whitespace_in_kind = test "{!va l:foo}"

    let internal_whitespace_in_referent = test "{!val:( * )}"

    let internal_quoted_whitespace_in_referent = test "{!val:\"( * )\"}"

    let two_colons = test "{!val:foo:bar}"

    let space_before_colon = test "{!val :foo}"

    let space_after_colon = test "{!val: foo}"

    let unterminated_after_kind = test "{!val:foo"

    let operator = test "{!(>>=)}"

    let operator_with_dash = test "{!(@->)}"

    let operator_with_dot = test "{!(*.)}"

    let operator_with_colon = test "{!(>::)}"
  end in
  ()

let raw_markup () =
  let module Raw_markup = struct
    let html_target = test "{%html:foo%}"

    let whitespace = test "{%html: foo bar %}"

    let whitespace_only = test "{%html: %}"

    let empty = test "{%html:%}"

    let html_payload = test "{%html:<e>foo</e>%}"

    let colon = test "{%html:foo:bar%}"

    let no_target = test "{%foo%}"

    let empty_target = test "{%:foo%}"

    let whitespace_target = test "{% :foo%}"

    let multiline_target = test "{%\n:foo%}"

    let percent_in_target = test "{%%:%}"

    let percent_in_payload = test "{%html:%%}"

    let multiple_percent_in_target = test "{%%%foo%%:%}"

    let multiple_percent_in_payload = test "{%html:%%foo%%%}"

    let opener_in_target = test "{%{%:foo%}"

    let opener_in_payload = test "{%html:{%%}"

    let right_brace_in_target = test "{%}:%}"

    let right_brace_in_payload = test "{%html:}%}"

    let unterminated = test "{%"

    let unterminated_after_target = test "{%html:"

    let degenerate = test "{%}"
  end in
  ()

let section_contexts () =
  let module Section_contexts = struct
    let titles_allowed = test "{0 Foo}"

    let titles_no_high_levels = test "{6 Foo}"

    let two_titles = test "{0 Foo}\n{0 Bar}"

    let no_heading = test "foo"

    let heading_after_paragraph = test "foo\n{0 Bar}"

    let two_top_level_section_headings = test "{1 Foo}\n{1 Bar}"

    let two_headings_second_higher = test "{1 Foo}\n{0 Bar}"

    let three_headings_last_two_higher = test "{3 Foo}\n{1 Bar}\n{2 Baz}"

    let none = test "{1 Foo}"

    let title_no_titles_allowed = test "{0 Foo}"

    let two_titles_none_allowed = test "{0 Foo}\n{0 Bar}"

    let two_headings_none_allowed = test "{1 Foo}\n{1 Bar}"

    let multiple_with_bad_section = test "{0 Foo}\n{0 Foo}\n{6 Foo}"

    let promoted_duplicates = test "{6 Foo}\n{6 Bar}"

    let section_promoted_to_duplicate = test "{5 Foo}\n{6 Bar}"
  end in
  ()

let heading () =
  let module Heading = struct
    let basic = test "{2 Foo}"

    let subsection = test "{3 Foo}"

    let subsubsection = test "{4 Foo}"

    let leading_whitespace = test "{2  Foo}"

    let no_leading_whitespace = test "{2Foo}"

    let no_leading_whitespace_h3 = test "{3Foo}"

    let leading_newline = test "{2\nFoo}"

    let leading_cr_lf = test "{2\r\nFoo}"

    let leading_blank_line = test "{2\n\nFoo}"

    let leading_blank_line_h3 = test "{3\n\nFoo}"

    let trailing_whitespace = test "{2 Foo }"

    let trailing_newline = test "{2 Foo\n}"

    let trailing_blank_line = test "{2 Foo\n\n}"

    let nested_markup = test "{2 [foo]}"

    let nested_code_with_uppercase = test "{2 [Foo]}"

    let nested_code_with_spaces = test "{2 [ foo bar  baz  \t]}"

    let nested_code_with_newline = test "{2 [foo\nbar\r\nbaz]}"

    let nested_style = test "{2 {e foo bar}}"

    let words = test "{2 foo bar}"

    let nested_heading = test "{2 {2 Foo}}"

    let in_list = test "- {2 Foo}"

    let followed_by_junk = test "{2 Foo} bar"

    let preceded_by_junk = test "foo {2 Bar}"

    let followed_by_block = test "{2 Foo}\nbar"

    let preceded_by_block = test "foo\n{2 Bar}"

    let label = test "{2:foo Bar}"

    let whitespace_before_colon = test "{2 :foo Bar}"

    let whitespace_after_colon = test "{2: foo Bar}"

    let label_only = test "{2:foo}"

    let label_only_with_whitespace = test "{2:foo }"

    let in_list_outside_item = test "{ul {2 Foo}}"

    let preceded_by_shorthand_list = test "- foo\n{2 Bar}"

    let nested_in_two_lists = test "{ul {li - foo\n{2 Bar}}}"

    let bad_level_long_number = test "{22 Foo}"

    let bad_level_long_number_with_label = test "{22:foo Bar}"

    let bad_level_leading_zero = test "{02 Foo}"

    let bad_level_leading_zero_with_label = test "{02:foo Bar}"

    let bad_level_title = test "{0 Foo}"

    let bad_level_too_deep = test "{6 Foo}"

    let link_in_markup = test "{2 {{:foo}}}"

    let reference_in_markup = test "{2 {!foo}}"

    let two = test "{2 Foo}\n{2 Bar}"

    let greater = test "{2 Foo}\n{3 Bar}"
  end in
  ()

let author () =
  let module Author = struct
    let basic = test "@author Foo Bar"

    let empty = test "@author"

    let whitespace_only = test "@author"

    let extra_whitespace = test "@author  Foo Bar"

    let newline = test "@author Foo Bar"

    let cr_lf = test "@author Foo Bar"

    let blank_line = test "@author Foo Bar"

    let followed_by_junk = test "@author Foo\nbar"

    let followed_by_code_span = test "@author Foo\n[bar]"

    let followed_by_code_block = test "@author Foo\n{[bar]}"

    let followed_by_verbatim = test "@author Foo\n{v bar v}"

    let followed_by_modules = test "@author foo\n{!modules:Foo}"

    let followed_by_list = test "@author Foo\n{ul {li bar}}"

    let followed_by_shorthand_list = test "@author Foo\n- bar"

    let followed_by_section_heading = test "@author Foo\n{2 Bar}"

    let followed_by_author = test "@author Foo\n@author Bar"

    let followed_by_author_cr_lf = test "@author Foo\n@author Bar"

    let in_author = test "@author Foo @author Bar"

    let in_author_at_start = test "@author @author Foo"

    let preceded_by_paragraph = test "foo\n@author Bar"

    let no_markup = test "@author Foo [Bar]"

    let in_paragraph = test "foo @author Bar"

    let in_code = test "[@author Foo]"

    let in_style = test "{b @author Foo}"

    let in_heading = test "{2 @author Foo}"

    let after_shorthand_list = test "- foo\n@author Bar"

    let in_shorthand_list = test "- foo @author Bar"

    let in_shorthand_list_at_start = test "- @author Foo"

    let in_list_item = test "{ul {li foo @author Bar}}"

    let in_list_item_at_start = test "{ul {li @author Foo}}"

    let in_list_item_on_new_line = test "{ul {li foo\n@author Bar}}"

    let in_list = test "{ul @author Foo}"

    let in_code_block = test "{[@author Foo]}"

    let in_verbatim = test "{v @author Foo v}"

    let after_code_block = test "{[foo]} @author Bar"

    let after_verbatim = test "{v foo v} @author Bar"

    let after_heading = test "{2 Foo} @author Bar"

    let after_list = test "{ul {li foo}} @author Bar"

    let preceded_by_whitespace = test "@author Foo Bar"

    let second_preceded_by_whitespace = test "@author Foo\n @author Bar"

    let prefix = test "@authorfoo"

    let not_allowed = test ~tags_allowed:false "@author Foo bar"
  end in
  ()

let reference_component_kind () =
  let module Reference_component_kind = struct
    let no_kind_with_quotes = test "{!\"foo\".\"bar\"}"

    let no_kind = test "{!foo}"

    let class_ = test "{!class-foo}"

    let class_type = test "{!class-type-foo}"

    let class_type_alt = test "{!classtype-foo}"

    let constructor = test "{!constructor-Foo}"

    let constructor_alt = test "{!const-Foo}"

    let dash_in_page_name = test "{!page-\"foo-bar\"}"

    let dot_and_dash_in_page_name = test "{!page-\"foo-bar.v0.0.1\"}"

    let exception_ = test "{!exception-Foo}"

    let exception_alt = test "{!exn-Foo}"

    let extension = test "{!extension-Foo}"

    let field = test "{!field-foo}"

    let field_alt = test "{!recfield-foo}"

    let heading = test "{!section-foo}"

    let heading_alt = test "{!label-foo}"

    let instance_variable = test "{!instance-variable-foo}"

    let kind_with_quotes =
      test "{!module-type-\"Bar\".module-\"Moo\".class-\"There\"}"

    let method_ = test "{!method-foo}"

    let module_ = test "{!module-Foo}"

    let module_type = test "{!module-type-Foo}"

    let module_type_alt = test "{!modtype-Foo}"

    let page = test "{!page-foo}"

    let type_ = test "{!type-foo}"

    let val_ = test "{!val-foo}"

    let val_alt = test "{!value-foo}"

    let longident = test "{!module-Foo.type-bar}"

    let hyphenated_kind_longident =
      test "{!module-type-Foo.module-type-Bar.type-baz}"

    let empty = test "{!}"

    let empty_qualifier = test "{!-foo}"

    let empty_identifier = test "{!val-}"

    let invalid_qualifier = test "{!foo-bar}"

    let empty_first_component = test "{!.foo}"

    let empty_second_component = test "{!Foo.}"

    let second_component_empty_qualifier = test "{!Foo.-bar}"

    let second_component_empty_identifier = test "{!Foo.val-}"

    let first_component_empty_identifier = test "{!module-.foo}"

    let something_in_invalid = test "{!foo-bar.baz}"

    let something_in_something = test "{!foo.bar}"

    let something_in_module = test "{!module-Foo.bar}"

    let something_in_module_type = test "{!module-type-Foo.bar}"

    let something_in_type = test "{!type-foo.bar}"

    let something_in_class = test "{!class-foo.bar}"

    let something_in_class_type = test "{!class-type-foo.bar}"

    let something_in_page = test "{!page-foo.bar}"

    let something_in_constructor = test "{!constructor-Foo.bar}"

    let something_in_exception = test "{!exception-Foo.bar}"

    let something_in_extension = test "{!extension-Foo.bar}"

    let something_in_field = test "{!field-foo.bar}"

    let something_in_section = test "{!section-foo.bar}"

    let something_in_instance_variable = test "{!instance-variable-foo.bar}"

    let something_in_method = test "{!method-foo.bar}"

    let something_in_val = test "{!val-foo.bar}"

    let something_in_something_nested = test "{!foo.bar.baz}"

    let something_in_module_nested = test "{!Foo.module-Bar.baz}"

    let something_in_module_type_nested = test "{!Foo.module-type-Bar.baz}"

    let something_in_type_nested = test "{!Foo.type-bar.baz}"

    let something_in_class_nested = test "{!Foo.class-bar.baz}"

    let something_in_class_type_nested = test "{!foo.class-type-bar.baz}"

    let something_in_page_nested = test "{!foo.page-bar.baz}"

    let something_in_constructor_nested = test "{!Foo.constructor-Bar.baz}"

    let something_in_exception_nested = test "{!Foo.exception-bar.baz}"

    let something_in_extension_nested = test "{!Foo.extension-bar.baz}"

    let something_in_field_nested = test "{!foo.field-bar.baz}"

    let something_in_section_nested = test "{!foo.section-bar.baz}"

    let something_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.baz}"

    let something_in_method_nested = test "{!foo.method-bar.baz}"

    let something_in_val_nested = test "{!Foo.val-bar.baz}"

    let module_in_empty = test "{!.module-Foo}"

    let module_in_something = test "{!Foo.module-Bar}"

    let module_in_module = test "{!module-Foo.module-Bar}"

    let module_in_module_type = test "{!module-type-Foo.module-Bar}"

    let module_in_class = test "{!class-foo.module-Bar}"

    let module_in_class_type = test "{!class-type-foo.module-Bar}"

    let module_in_constructor = test "{!constructor-Foo.module-Bar}"

    let module_in_exception = test "{!exception-Foo.module-Bar}"

    let module_in_extension = test "{!extension-Foo.module-Bar}"

    let module_in_field = test "{!field-foo.module-Bar}"

    let module_in_section = test "{!section-foo.module-Bar}"

    let module_in_instance_variable = test "{!instance-variable-foo.module-Bar}"

    let module_in_method = test "{!method-foo.module-Bar}"

    let module_in_page = test "{!page-foo.module-Bar}"

    let module_in_type = test "{!type-foo.module-Bar}"

    let module_in_val = test "{!val-foo.module-Bar}"

    let module_in_something_nested = test "{!Foo.Bar.module-Baz}"

    let module_in_module_nested = test "{!Foo.module-Bar.module-Baz}"

    let module_in_module_type_nested = test "{!Foo.module-type-Bar.module-Baz}"

    let module_in_class_nested = test "{!Foo.class-bar.module-Baz}"

    let module_in_class_type_nested = test "{!Foo.class-type-bar.module-Baz}"

    let module_in_constructor_nested = test "{!Foo.constructor-Bar.module-Baz}"

    let module_in_exception_nested = test "{!Foo.exception-Bar.module-Baz}"

    let module_in_extension_nested = test "{!Foo.extension-Bar.module-Baz}"

    let module_in_field_nested = test "{!foo.field-bar.module-Baz}"

    let module_in_section_nested = test "{!foo.section-bar.module-Baz}"

    let module_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.module-Baz}"

    let module_in_method_nested = test "{!foo.method-bar.module-Baz}"

    let module_in_page_nested = test "{!foo.page-bar.module-Baz}"

    let module_in_type_nested = test "{!Foo.type-bar.module-Baz}"

    let module_in_val_nested = test "{!Foo.val-bar.module-Baz}"

    let module_type_in_something = test "{!Foo.module-type-Bar}"

    let module_type_in_module = test "{!module-Foo.module-type-Bar}"

    let module_type_in_module_type = test "{!module-type-Foo.module-type-Bar}"

    let module_type_in_class = test "{!class-foo.module-type-Bar}"

    let module_type_in_page = test "{!page-foo.module-type-Bar}"

    let type_in_something = test "{!Foo.type-bar}"

    let type_in_module = test "{!module-Foo.type-bar}"

    let type_in_module_type = test "{!module-type-Foo.type-bar}"

    let type_in_class = test "{!class-foo.type-bar}"

    let type_in_page = test "{!page-foo.type-bar}"

    let constructor_in_empty = test "{!.constructor-Foo}"

    let constructor_in_something = test "{!foo.constructor-Bar}"

    let constructor_in_type = test "{!type-foo.constructor-Bar}"

    let constructor_in_class = test "{!class-foo.constructor-Bar}"

    let constructor_in_class_type = test "{!class-type-foo.constructor-Bar}"

    let constructor_in_constructor = test "{!constructor-Foo.constructor-Bar}"

    let constructor_in_exception = test "{!exception-Foo.constructor-Bar}"

    let constructor_in_extension = test "{!extension-Foo.constructor-Bar}"

    let constructor_in_field = test "{!field-foo.constructor-Bar}"

    let constructor_in_section = test "{!section-foo.constructor-Bar}"

    let constructor_in_instance_variable =
      test "{!instance-variable-foo.constructor-Bar}"

    let constructor_in_method = test "{!method-foo.constructor-Bar}"

    let constructor_in_module = test "{!module-Foo.constructor-Bar}"

    let constructor_in_module_type = test "{!module-type-Foo.constructor-Bar}"

    let constructor_in_page = test "{!page-foo.constructor-Bar}"

    let constructor_in_val = test "{!val-foo.constructor-Bar}"

    let constructor_in_something_nested = test "{!foo.bar.constructor-Baz}"

    let constructor_in_type_nested = test "{!foo.type-bar.constructor-Baz}"

    let constructor_in_class_nested = test "{!Foo.class-bar.constructor-Baz}"

    let constructor_in_class_type_nested =
      test "{!Foo.class-type-bar.constructor-Baz}"

    let constructor_in_constructor_nested =
      test "{!Foo.constructor-Bar.constructor-Baz}"

    let constructor_in_exception_nested =
      test "{!Foo.exception-Bar.constructor-Baz}"

    let constructor_in_extension_nested =
      test "{!Foo.extension-Bar.constructor-Baz}"

    let constructor_in_field_nested = test "{!foo.field-bar.constructor-Baz}"

    let constructor_in_section_nested =
      test "{!foo.section-bar.constructor-Baz}"

    let constructor_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.constructor-Baz}"

    let constructor_in_method_nested = test "{!foo.method-bar.constructor-Baz}"

    let constructor_in_module_nested = test "{!Foo.module-Bar.constructor-Baz}"

    let constructor_in_module_type_nested =
      test "{!Foo.module-type-Bar.constructor-Baz}"

    let constructor_in_page_nested = test "{!foo.page-bar.constructor-Baz}"

    let constructor_in_val_nested = test "{!Foo.val-bar.constructor-Baz}"

    let field_in_empty = test "{!.field-foo}"

    let field_in_something = test "{!foo.field-bar}"

    let field_in_module = test "{!module-Foo.field-bar}"

    let field_in_module_type = test "{!module-type-Foo.field-bar}"

    let field_in_type = test "{!type-foo.field-bar}"

    let field_in_class = test "{!class-foo.field-bar}"

    let field_in_class_type = test "{!class-type-foo.field-bar}"

    let field_in_constructor = test "{!constructor-Foo.field-bar}"

    let field_in_exception = test "{!exception-Foo.field-bar}"

    let field_in_extension = test "{!extension-Foo.field-bar}"

    let field_in_field = test "{!field-foo.field-bar}"

    let field_in_section = test "{!section-foo.field-bar}"

    let field_in_instance_variable = test "{!instance-variable-foo.field-bar}"

    let field_in_method = test "{!method-foo.field-bar}"

    let field_in_page = test "{!page-foo.field-bar}"

    let field_in_val = test "{!val-foo.field-bar}"

    let field_in_something_nested = test "{!foo.bar.field-baz}"

    let field_in_module_nested = test "{!Foo.module-Bar.field-baz}"

    let field_in_module_type_nested = test "{!Foo.module-type-Bar.field-baz}"

    let field_in_type_nested = test "{!Foo.type-bar.field-baz}"

    let field_in_class_nested = test "{!Foo.class-bar.field-baz}"

    let field_in_class_type_nested = test "{!Foo.class-type-bar.field-baz}"

    let field_in_constructor_nested = test "{!Foo.constructor-bar.field-baz}"

    let field_in_exception_nested = test "{!Foo.exception-Bar.field-baz}"

    let field_in_extension_nested = test "{!Foo.extension-Bar.field-baz}"

    let field_in_field_nested = test "{!Foo.field-bar.field-baz}"

    let field_in_section_nested = test "{!foo.section-bar.field-baz}"

    let field_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.field-baz}"

    let field_in_method_nested = test "{!foo.method-bar.field-baz}"

    let field_in_page_nested = test "{!foo.page-bar.field-baz}"

    let field_in_val_nested = test "{!Foo.val-bar.field-baz}"

    let exception_in_something = test "{!Foo.exception-Bar}"

    let exception_in_module = test "{!module-Foo.exception-Bar}"

    let exception_in_class = test "{!class-foo.exception-Bar}"

    let exception_in_page = test "{!page-foo.exception-Bar}"

    let extension_in_something = test "{!Foo.extension-Bar}"

    let extension_in_module = test "{!module-Foo.extension-Bar}"

    let extension_in_class = test "{!class-foo.extension-Bar}"

    let extension_in_page = test "{!page-foo.extension-Bar}"

    let val_in_something = test "{!Foo.val-bar}"

    let val_in_module = test "{!module-Foo.val-bar}"

    let val_in_class = test "{!class-foo.val-bar}"

    let val_in_page = test "{!page-foo.val-bar}"

    let class_in_something = test "{!Foo.class-bar}"

    let class_in_module = test "{!module-Foo.class-bar}"

    let class_in_class = test "{!class-foo.class-bar}"

    let class_in_page = test "{!page-foo.class-bar}"

    let class_type_in_something = test "{!Foo.class-type-bar}"

    let class_type_in_module = test "{!module-Foo.class-type-bar}"

    let class_type_in_class = test "{!class-foo.class-type-bar}"

    let class_type_in_page = test "{!page-foo.class-type-bar}"

    let method_in_empty = test "{!.method-foo}"

    let method_in_something = test "{!foo.method-bar}"

    let method_in_class = test "{!class-foo.method-bar}"

    let method_in_class_type = test "{!class-type-foo.method-bar}"

    let method_in_constructor = test "{!constructor-Foo.method-bar}"

    let method_in_exception = test "{!exception-Foo.method-bar}"

    let method_in_extension = test "{!extension-Foo.method-bar}"

    let method_in_field = test "{!field-foo.method-bar}"

    let method_in_section = test "{!section-foo.method-bar}"

    let method_in_instance_variable = test "{!instance-variable-foo.method-bar}"

    let method_in_method = test "{!method-foo.method-bar}"

    let method_in_module = test "{!module-Foo.method-bar}"

    let method_in_module_type = test "{!module-type-Foo.method-bar}"

    let method_in_page = test "{!page-foo.method-bar}"

    let method_in_type = test "{!type-foo.method-bar}"

    let method_in_val = test "{!val-foo.method-bar}"

    let method_in_something_nested = test "{!foo.bar.method-baz}"

    let method_in_class_nested = test "{!Foo.class-bar.method-baz}"

    let method_in_class_type_nested = test "{!Foo.class-type-bar.method-baz}"

    let method_in_constructor_nested = test "{!foo.constructor-Bar.method-baz}"

    let method_in_exception_nested = test "{!Foo.exception-Bar.method-baz}"

    let method_in_extension_nested = test "{!Foo.extension-Bar.method-baz}"

    let method_in_field_nested = test "{!foo.field-bar.method-baz}"

    let method_in_section_nested = test "{!foo.section-bar.method-baz}"

    let method_in_instance_variable_nested =
      test "{!foo.instance-variable-bar.method-baz}"

    let method_in_method_nested = test "{!foo.method-bar.method-baz}"

    let method_in_module_nested = test "{!Foo.module-Bar.method-baz}"

    let method_in_module_type_nested = test "{!Foo.module-type-Bar.method-baz}"

    let method_in_page_nested = test "{!foo.page-bar.method-baz}"

    let method_in_type_nested = test "{!Foo.type-bar.method-baz}"

    let method_in_val_nested = test "{!Foo.val-bar.method-baz}"

    let instance_variable_in_something = test "{!Foo.instance-variable-bar}"

    let instance_variable_in_module = test "{!module-Foo.instance-variable-bar}"

    let instance_variable_in_class = test "{!class-foo.instance-variable-bar}"

    let instance_variable_in_page = test "{!page-foo.instance-variable-bar}"

    let section_in_something = test "{!Foo.section-bar}"

    let section_in_module = test "{!module-Foo.section-bar}"

    let section_in_class = test "{!class-foo.section-bar}"

    let section_in_page = test "{!page-foo.section-bar}"

    let page_in_something = test "{!foo.page-bar}"

    let inner_parent_something_in_something = test "{!foo.bar.field-baz}"

    let inner_parent_something_in_module = test "{!module-Foo.bar.field-baz}"

    let inner_parent_something_in_class = test "{!class-foo.bar.field-baz}"

    let inner_parent_something_in_page = test "{!page-foo.bar.field-baz}"

    let inner_parent_module_in_module =
      test "{!module-Foo.module-Bar.field-baz}"

    let inner_parent_module_in_class = test "{!class-foo.module-Bar.field-baz}"

    let inner_parent_module_type_in_module =
      test "{!module-Foo.module-type-Bar.field-baz}"

    let inner_parent_module_type_in_class =
      test "{!class-foo.module-type-Bar.field-baz}"

    let inner_parent_type_in_module = test "{!module-Foo.type-bar.field-baz}"

    let inner_parent_type_in_class = test "{!class-foo.type-bar.field-baz}"

    let inner_parent_class_in_module = test "{!module-Foo.class-bar.field-baz}"

    let inner_parent_class_in_class = test "{!class-foo.class-bar.field-baz}"

    let inner_parent_class_type_in_module =
      test "{!module-Foo.class-type-bar.field-baz}"

    let inner_parent_class_type_in_class =
      test "{!class-foo.class-type-bar.field-baz}"

    let inner_label_parent_something_in_something = test "{!foo.bar.baz}"

    let inner_label_parent_something_in_page = test "{!page-foo.bar.baz}"

    let inner_label_parent_module_in_module =
      test "{!module-Foo.module-Bar.baz}"

    let inner_label_parent_module_in_class = test "{!class-foo.module-Bar.baz}"

    let inner_label_parent_module_type_in_module =
      test "{!module-Foo.module-type-Bar.baz}"

    let inner_label_parent_module_type_in_class =
      test "{!class-foo.module-type-Bar.baz}"

    let inner_label_parent_type_in_module = test "{!module-Foo.type-bar.baz}"

    let inner_label_parent_type_in_class = test "{!class-foo.type-bar.baz}"

    let inner_label_parent_class_in_module = test "{!module-Foo.class-bar.baz}"

    let inner_label_parent_class_in_class = test "{!class-foo.class-bar.baz}"

    let inner_label_parent_class_type_in_module =
      test "{!module-Foo.class-bar.baz}"

    let inner_label_parent_class_type_in_class =
      test "{!class-foo.class-bar.baz}"

    let inner_page_in_something = test "{!foo.page-bar.baz}"

    let inner_class_signature_something_in_something =
      test "{!foo.bar.method-baz}"

    let inner_class_signature_something_in_page =
      test "{!page-foo.bar.method-baz}"

    let inner_class_signature_class_in_module =
      test "{!module-Foo.class-bar.method-baz}"

    let inner_class_signature_class_in_class =
      test "{!class-foo.class-bar.method-baz}"

    let inner_class_signature_class_type_in_module =
      test "{!module-Foo.class-type-bar.method-baz}"

    let inner_class_signature_class_type_in_class =
      test "{!class-foo.class-type-bar.method-baz}"

    let inner_signature_something_in_something = test "{!foo.bar.type-baz}"

    let inner_signature_something_in_page = test "{!page-foo.bar.type-baz}"

    let inner_signature_module_in_module =
      test "{!module-Foo.module-Bar.type-baz}"

    let inner_signature_module_in_class =
      test "{!class-foo.module-Bar.type-baz}"

    let inner_signature_module_type_in_module =
      test "{!module-Foo.module-type-Bar.type-baz}"

    let inner_signature_module_type_in_class =
      test "{!class-foo.module-type-Bar.type-baz}"

    let inner_datatype_something_in_something =
      test "{!foo.bar.constructor-Baz}"

    let inner_datatype_something_in_page =
      test "{!page-foo.bar.constructor-Baz}"

    let inner_datatype_type_in_module =
      test "{!module-Foo.type-bar.constructor-Baz}"

    let inner_datatype_type_in_class =
      test "{!class-foo.type-bar.constructor-Baz}"

    let kind_conflict = test "{!val:type-foo}"

    let kind_agreement = test "{!val:val-foo}"

    let kind_agreement_alt = test "{!value:val-foo}"

    let canonical_something = test "@canonical Foo"

    let canonical_module = test "@canonical module-Foo"

    let canonical_path = test "@canonical Foo.Bar"

    let canonical_val = test "@canonical val-foo"

    let canonical_bad_parent = test "@canonical bar.page-foo"

    let canonical_empty_component = test "@canonical .Foo"

    let canonical_empty_name = test "@canonical Foo."

    let internal_whitespace = test "{!foo. bar .baz}"

    let replacement_text_empty_identifier = test "{{!val-} foo}"

    let reference_with_unmatched_quotation = test "{!\"\"foo\"}"
  end in
  ()

let reference_path () =
  let module Reference_path = struct
    (* Absolute references *)

    let abs = test "{!/foo/bar}"

    let abs_label_parent_page = test "{!/foo/bar.label}"

    let abs_label_parent_module = test "{!/foo/Bar.label}"

    (* References to current package root *)

    let root_to_page = test "{!//foo/bar}"

    let root_to_module = test "{!//foo/Bar}"

    let root_label_parent_page = test "{!//foo/bar.label}"

    let root_label_parent_module = test "{!//foo/Bar.label}"

    (* Relative paths *)

    let relative = test "{!foo/bar}"

    let relative = test "{!foo/bar/baz}"

    let relative_module = test "{!foo/Bar}"

    let relative_label_parent_page = test "{!foo/bar.label}"

    let relative_label_parent_module = test "{!foo/Bar.label}"

    let dot_relative = test "{!./bar}"

    let dot_relative_module = test "{!./Bar}"

    let dot_relative_label_parent_page = test "{!./bar.label}"

    let dot_relative_label_parent_module = test "{!./Bar.label}"

    (* Prefix *)

    let abs_label_parent_page_prefix = test "{!/foo/bar.section-label}"

    let abs_label_parent_module_prefix = test "{!/foo/Bar.section-label}"

    let root_label_parent_page_prefix = test "{!//foo/bar.section-label}"

    let root_label_parent_module_prefix = test "{!//foo/Bar.section-label}"

    let relative_tag_after_slash = test "{!foo/page-bar}"

    let relative_tag_after_slash = test "{!foo/module-Bar}"

    let relative_tag_after_slash_label_parent =
      test "{!page_path/page-pagename.section-sectionname}"

    (* Errors *)

    let err_abs_only = test "{!/}"

    let err_relative_only = test "{!foo/}"

    let err_root_only = test "{!//}"

    let err_relative_empty = test "{!foo/}"

    let err_dot_relative_empty = test "{!./}"

    let err_page_prefix_after_dot = test "{!foo.page-bar}"

    let err_unsupported_kind = test "{!foo/type-bar}"

    let err_relative_empty_component = test "{!foo//bar}"

    let err_current_package_empty_component = test "{!///bar}"

    let err_last_empty_component = test "{!foo/}"

    let err_first_empty_component = test "{!/}"

    let err_current_package_empty_component = test "{!//}"

    (* Old kind compatibility *)

    let oldkind_abs_page = test "{!section:/foo.label}"

    let oldkind_abs_module = test "{!section:/Foo.label}"

    let oldkind_relative_page = test "{!section:foo/bar.label}"

    let oldkind_relative_module = test "{!section:foo/Bar.label}"

    let oldkind_root_page = test "{!section://foo/bar.label}"

    let oldkind_root_module = test "{!section://foo/Bar.label}"
  end in
  ()

let groups =
  [
    ("simple_reference", simple_reference);
    ("raw_markup", raw_markup);
    ("section_contexts", section_contexts);
    ("heading", heading);
    ("author", author);
    ("reference_component_kind", reference_component_kind);
    ("reference_path", reference_path);
  ]

let () = Expect_test_helper.run ~package:"odoc" groups
