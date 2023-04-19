module ModuleName = Odoc_model.Names.ModuleName
module H = Tyxml.Html
open Strings_of_odoc

let rec string_of_non_link = function
  | `Space -> H.txt " "
  | `Word w -> H.txt w
  | `Code_span s -> H.code [ H.txt s ]
  | `Raw_markup (_, s) -> H.txt s
  | `Styled (_, lst) -> string_of_link_content lst

and string_of_element = function
  | `Styled (_, lst) -> string_of_paragraph lst
  | `Reference (r, _) -> H.code [ H.txt (string_of_reference r) ]
  | `Link (_, r) -> string_of_link_content r
  | `Space -> H.txt " "
  | `Word w -> H.txt w
  | `Code_span s -> H.code [ H.txt s ]
  | `Raw_markup (_, s) -> H.txt s

and string_of_link_content lst =
  H.span
    (List.map (fun r -> string_of_non_link r.Odoc_model.Location_.value) lst)

and string_of_paragraph lst =
  H.span
    (List.map (fun elt -> string_of_element elt.Odoc_model.Location_.value) lst)

let string_of_doc = function
  | `Paragraph p -> Some (H.p [ string_of_paragraph p ])
  | `Heading (_, _, p) -> Some (H.p [ string_of_link_content p ])
  | _ -> None

let string_of_docs lst =
  List.find_map (fun elt -> string_of_doc elt.Odoc_model.Location_.value) lst
