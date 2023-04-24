open Odoc_model

let words_of_string s = String.split_on_char ' ' s

let words_of_identifier = function
  | `Class (_, n) -> [ Names.ClassName.to_string n ]
  | `ClassType (_, n) -> [ Names.ClassTypeName.to_string n ]
  | `Constructor (_, n) -> [ Names.ConstructorName.to_string n ]
  | `Exception (_, n) -> [ Names.ExceptionName.to_string n ]
  | `Extension (_, n) -> [ Names.ExtensionName.to_string n ]
  | `Field (_, n) -> [ Names.FieldName.to_string n ]
  | `InstanceVariable (_, n) -> [ Names.InstanceVariableName.to_string n ]
  | `Label (_, n) -> [ Names.LabelName.to_string n ]
  | `Method (_, n) -> [ Names.MethodName.to_string n ]
  | `Module (_, n) -> [ Names.ModuleName.to_string n ]
  | `ModuleType (_, n) -> [ Names.ModuleTypeName.to_string n ]
  | `Type (_, n) -> [ Names.TypeName.to_string n ]
  | `Value (_, n) -> [ Names.ValueName.to_string n ]
  | _ -> []

let words_of_resolved = function
  | `Identifier v -> words_of_identifier v
  | r -> words_of_identifier r

let words_of_reference = function
  | `Root (r, _) -> [ r ]
  | `Dot (_, n) -> [ n ]
  | `Resolved r -> words_of_resolved r
  | r -> words_of_identifier r

let rec words_of_non_link = function
  | `Space -> []
  | `Word w -> [ w ]
  | `Code_span s -> words_of_string s
  | `Raw_markup (_, _s) -> []
  | `Styled (_, lst) -> words_of_link_content lst

and words_of_element = function
  | `Styled (_, lst) -> words_of_paragraph lst
  | `Reference (r, _) -> words_of_reference r
  | `Link (_, r) -> words_of_link_content r
  | `Space -> []
  | `Word w -> [ w ]
  | `Code_span s -> words_of_string s
  | `Raw_markup (_, _s) -> []

and words_of_link_content lst =
  List.concat_map (fun r -> words_of_non_link r.Odoc_model.Location_.value) lst

and words_of_paragraph lst =
  List.concat_map
    (fun elt -> words_of_element elt.Odoc_model.Location_.value)
    lst

let words_of_doc = function
  | `Paragraph p -> words_of_paragraph p
  | `Heading (_, _, p) -> words_of_link_content p
  | _ -> []

let words_of_docs lst =
  List.concat_map (fun elt -> words_of_doc elt.Odoc_model.Location_.value) lst
  |> List.filter_map (fun word ->
         let word =
           word |> Db.list_of_string |> List.rev_map Char.lowercase_ascii
         in
         let word =
           List.filter
             (fun chr ->
               (chr >= 'a' && chr <= 'z') || (chr >= '0' && chr <= '9'))
             word
         in
         if word = [] then None else Some word)
  |> List.sort_uniq (List.compare Char.compare)
