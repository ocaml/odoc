(* Partially resolve references in docs *)
open Odoc_model

module Resolve = struct
  let lookup_signature env name : Paths.Reference.Signature.t option =
    let all =
      (Ident_env.lookup_module_by_name env name
        :> Paths.Identifier.Signature.t list)
      @ (Ident_env.lookup_module_type_by_name env name
          :> Paths.Identifier.Signature.t list)
    in
    match all with x :: _ -> Some (`Resolved (`Identifier x)) | _ -> None

  let lookup_any env name : Paths.Reference.t option =
    let all =
      (Ident_env.lookup_module_by_name env name :> Paths.Identifier.t list)
      @ (Ident_env.lookup_module_type_by_name env name
          :> Paths.Identifier.t list)
      @ (Ident_env.lookup_type_by_name env name :> Paths.Identifier.t list)
      @ (Ident_env.lookup_value_by_name env name :> Paths.Identifier.t list)
    in
    match all with x :: _ -> Some (`Resolved (`Identifier x)) | _ -> None

  let rec signature env (x : Odoc_model.Paths.Reference.Signature.t) :
      Odoc_model.Paths.Reference.Signature.t =
    match x with
    | `Resolved _ -> x
    | `Root (r, t) -> (
        match t with
        | `TUnknown -> (
            match lookup_signature env r with Some x -> x | None -> x)
        | `TModule | `TModuleType -> x)
    | `Dot (p, n) -> `Dot (label_parent env p, n)
    | `Module_path _ -> x
    | `Module (x, n) -> `Module (signature env x, n)
    | `ModuleType (x, n) -> `ModuleType (signature env x, n)

  and label_parent env (x : Odoc_model.Paths.Reference.LabelParent.t) :
      Odoc_model.Paths.Reference.LabelParent.t =
    match x with
    | `Resolved _ -> x
    | `Dot (p, n) -> `Dot (label_parent env p, n)
    | `Root (r, t) -> (
        match t with
        | `TUnknown -> (
            match lookup_signature env r with
            | Some x -> (x :> Odoc_model.Paths.Reference.LabelParent.t)
            | None -> x)
        | `TModule | `TModuleType | `TClass | `TClassType | `TType | `TPage
        | `TChildPage | `TChildModule ->
            x)
    | `Module_path _ | `Any_path _ | `Page_path _ -> x
    | `Module (x, n) -> `Module (signature env x, n)
    | `ModuleType (x, n) -> `ModuleType (signature env x, n)
    | `Type (x, n) -> `Type (signature env x, n)
    | `Class (x, n) -> `Class (signature env x, n)
    | `ClassType (x, n) -> `ClassType (signature env x, n)

  let do_resolve env (x : Odoc_model.Paths.Reference.t) :
      Odoc_model.Paths.Reference.t =
    match x with
    | `Resolved _ -> x
    | `Root
        ( _r,
          ( `TModule | `TModuleType | `TType | `TConstructor | `TField
          | `TExtension | `TExtensionDecl | `TException | `TValue | `TClass
          | `TClassType | `TMethod | `TInstanceVariable | `TLabel | `TPage
          | `TAsset | `TChildPage | `TChildModule | `TUnknown ) ) ->
        x
    | `Dot (p, n) -> `Dot (label_parent env p, n)
    | `Page_path _ | `Module_path _ | `Asset_path _ | `Any_path _ -> x
    | `Module (r, n) -> `Module (signature env r, n)
    | `ModuleType (r, n) -> `ModuleType (signature env r, n)
    | `Type (r, n) -> `Type (signature env r, n)
    | `Constructor _ -> x (* TODO *)
    | `Field _ -> x (* TODO *)
    | `Extension _ -> x (* TODO *)
    | `ExtensionDecl _ -> x (* TODO *)
    | `Exception _ -> x (* TODO *)
    | `Value _ -> x (* TODO *)
    | `Class (r, n) -> `Class (signature env r, n)
    | `ClassType (r, n) -> `ClassType (signature env r, n)
    | `Method _ -> x (* TODO *)
    | `InstanceVariable _ -> x (* TODO *)
    | `Label _ -> x (* TODO *)
end

let with_location :
    ('a -> 'b) -> 'a Comment.with_location -> 'b Comment.with_location =
 fun handler v -> { v with value = handler v.value }

let rec inline_element env (x : Comment.inline_element) : Comment.inline_element
    =
  match x with
  | `Space -> x
  | `Word _ -> x
  | `Code_span _ -> x
  | `Math_span _ -> x
  | `Raw_markup _ -> x
  | `Reference (r, x) -> `Reference (Resolve.do_resolve env r, x)
  | `Styled (s, i) ->
      `Styled (s, List.map (with_location (inline_element env)) i)
  | `Link _ -> x

let paragraph env = List.map (with_location (inline_element env))

let rec nestable_block_element :
    Ident_env.t ->
    Comment.nestable_block_element ->
    Comment.nestable_block_element =
 fun env -> function
  | `Paragraph p -> `Paragraph (paragraph env p)
  | `Code_block _ as x -> x
  | `Math_block _ as x -> x
  | `Verbatim _ as x -> x
  | `Modules _ as x -> x
  | `Table _ as x -> x
  | `List (kind, items) ->
      `List
        ( kind,
          List.map (List.map (with_location (nestable_block_element env))) items
        )
  | `Media _ as x -> x

let tag : Comment.tag -> Comment.tag = function x -> x

let block_element env : Comment.block_element -> Comment.block_element =
  function
  | #Comment.nestable_block_element as e ->
      (nestable_block_element env e :> Comment.block_element)
  | `Heading (attrs, label, content) ->
      `Heading
        (attrs, label, List.map (with_location (inline_element env)) content)
  | `Tag t -> `Tag t

let resolve env (v : Comment.elements) =
  List.map (with_location (block_element env)) v
