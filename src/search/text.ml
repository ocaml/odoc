module Of_document = struct
  (** Get plain text doc-comment from a doc comment *)

  let rec source s =
    let token = function
      | Odoc_document.Types.Source.Elt e -> inline e
      | Tag (_, t) -> source t
    in
    String.concat "" @@ List.map token s

  and inline i =
    let one o =
      match o.Odoc_document.Types.Inline.desc with
      | Text t -> t
      | Entity "#45" -> "-"
      | Entity "gt" -> ">"
      | Entity e -> "&" ^ e
      | Linebreak -> "\n"
      | Styled (_, t) -> inline t
      | Link (_, t) -> inline t
      | InternalLink { content; _ } -> inline content
      | Source s -> source s
      | Math m -> m
      | Raw_markup _ -> ""
    in
    String.concat "" @@ List.map one i

  let rec documented_src d =
    let one o =
      match o with
      | Odoc_document.Types.DocumentedSrc.Code c -> source c
      | Documented { code; _ } -> inline code
      | Nested { code; _ } -> documented_src code
      | Subpage _ -> ""
      | Alternative (Expansion { summary; _ }) -> source summary
    in
    String.concat "" @@ List.map one d
end

module Of_comments = struct
  (** Get plain text doc-comment from a doc comment *)

  let get_value x = x.Odoc_model.Location_.value

  let rec string_of_doc (doc : Odoc_model.Comment.docs) =
    doc |> List.map get_value
    |> List.map s_of_block_element
    |> String.concat "\n"

  and s_of_block_element (be : Odoc_model.Comment.block_element) =
    match be with
    | `Paragraph is -> inlines is
    | `Tag _ -> ""
    | `List (_, ls) ->
        List.map (fun x -> x |> List.map get_value |> List.map nestable) ls
        |> List.concat |> String.concat " "
    | `Heading (_, _, h) -> inlines h
    | `Modules _ -> ""
    | `Code_block (_, s, _todo) -> s |> get_value
    | `Verbatim v -> v
    | `Math_block m -> m
    | `Table _ -> (* TODO *) ""

  and nestable (n : Odoc_model.Comment.nestable_block_element) =
    s_of_block_element (n :> Odoc_model.Comment.block_element)

  and inlines is =
    is |> List.map get_value |> List.map inline |> String.concat ""

  and inline (i : Odoc_model.Comment.inline_element) =
    match i with
    | `Code_span s -> s
    | `Word w -> w
    | `Math_span m -> m
    | `Space -> " "
    | `Reference (_, c) -> link_content c
    | `Link (_, c) -> link_content c
    | `Styled (_, b) -> inlines b
    | `Raw_markup (_, _) -> ""

  and link_content l =
    l |> List.map get_value
    |> List.map non_link_inline_element
    |> String.concat ""

  and non_link_inline_element (n : Odoc_model.Comment.non_link_inline_element) =
    inline (n :> Odoc_model.Comment.inline_element)
end

let of_type te =
  let te_text = Odoc_document.ML.type_expr te in
  let te_doc = Odoc_document.Codefmt.render te_text in
  Of_document.source te_doc

let of_doc doc = Of_comments.string_of_doc doc

let of_record fields =
  let te_text = Odoc_document.ML.record fields in
  Of_document.documented_src te_text
