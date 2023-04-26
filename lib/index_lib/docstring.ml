open Odoc_model

let words_of_string s = String.split_on_char ' ' s
let words_of_identifier id = [ Comment.Identifier.name id ]

let words_of_resolved = function
  | `Identifier v -> words_of_identifier v
  | r -> words_of_identifier (Comment.Reference.Resolved.identifier r)

let words_of_reference : Comment.Reference.t -> _ = function
  | `Root (r, _) -> [ r ]
  | `Dot (_, n) -> [ n ]
  | `Resolved r -> words_of_resolved r
  | `InstanceVariable (_, name) -> [ Names.InstanceVariableName.to_string name ]
  | `Module (_, name) -> [ Names.ModuleName.to_string name ]
  | `ModuleType (_, name) -> [ Names.ModuleTypeName.to_string name ]
  | `Method (_, name) -> [ Names.MethodName.to_string name ]
  | `Field (_, name) -> [ Names.FieldName.to_string name ]
  | `Label (_, name) -> [ Names.LabelName.to_string name ]
  | `Type (_, name) -> [ Names.TypeName.to_string name ]
  | `Exception (_, name) -> [ Names.ExceptionName.to_string name ]
  | `Class (_, name) -> [ Names.ClassName.to_string name ]
  | `ClassType (_, name) -> [ Names.ClassTypeName.to_string name ]
  | `Value (_, name) -> [ Names.ValueName.to_string name ]
  | `Constructor (_, name) -> [ Names.ConstructorName.to_string name ]
  | `Extension (_, name) -> [ Names.ExtensionName.to_string name ]

let rec words_of_non_link : Comment.non_link_inline_element -> _ = function
  | `Math_span s -> words_of_string s
  | `Space -> []
  | `Word w -> [ w ]
  | `Code_span s -> words_of_string s
  | `Raw_markup (_, _s) -> []
  | `Styled (_, lst) -> words_of_link_content lst

and words_of_element : Comment.inline_element -> _ = function
  | `Math_span s -> words_of_string s
  | `Styled (_, lst) -> words_of_paragraph lst
  | `Reference (r, _) -> words_of_reference r
  | `Link (_, r) -> words_of_link_content r
  | `Space -> []
  | `Word w -> [ w ]
  | `Code_span s -> words_of_string s
  | `Raw_markup (_, _s) -> []

and words_of_link_content (lst : Comment.link_content) =
  List.concat_map (fun r -> words_of_non_link r.Odoc_model.Location_.value) lst

and words_of_paragraph (lst : Comment.paragraph) =
  List.concat_map
    (fun elt -> words_of_element elt.Odoc_model.Location_.value)
    lst

let words_of_doc : Comment.block_element -> _ = function
  | `Paragraph p -> words_of_paragraph p
  | `Heading (_, _, p) -> words_of_link_content p
  | _ -> []

let words_of_docs (lst : Odoc_model.Comment.docs) =
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
