(** Print .odocl files. *)

open Odoc_utils
open ResultMonad
open Odoc_odoc
open Odoc_model_desc

let print_json_desc desc x =
  let yojson = Type_desc_to_yojson.to_yojson desc x in
  Yojson.Basic.pretty_print Format.std_formatter yojson

module Element = struct
  open Odoc_model.Lang

  type t =
    | Module of Module.t
    | ModuleType of ModuleType.t
    | Type of TypeDecl.t
    | ClassType of ClassType.t
    | Class of Class.t
    | Value of Value.t
end

let rec signature_of_simple_expansion :
    Odoc_model.Lang.ModuleType.simple_expansion -> Odoc_model.Lang.Signature.t =
  function
  | Signature sg -> sg
  | Functor (_, e) -> signature_of_simple_expansion e

and signature_of_module_type_expr = function
  | Odoc_model.Lang.ModuleType.Signature sg -> Some sg
  | Path { p_expansion = Some exp; _ } ->
      Some (signature_of_simple_expansion exp)
  | Path { p_expansion = None; _ } -> None
  | Functor (_, e) -> signature_of_module_type_expr e
  | TypeOf { t_expansion = Some e; _ } -> Some (signature_of_simple_expansion e)
  | TypeOf _ -> None
  | With { w_expansion = Some e; _ } -> Some (signature_of_simple_expansion e)
  | With _ -> None
  | Strengthen { s_expansion = Some e; _ } ->
      Some (signature_of_simple_expansion e)
  | Strengthen _ -> None

and signature_of_module :
    Odoc_model.Lang.Module.t -> Odoc_model.Lang.Signature.t option =
 fun m ->
  match m.type_ with
  | Alias (_, Some e) -> Some (signature_of_simple_expansion e)
  | Alias (_, None) -> None
  | ModuleType m -> signature_of_module_type_expr m

and signature_of_module_type :
    Odoc_model.Lang.ModuleType.t -> Odoc_model.Lang.Signature.t option =
 fun m ->
  match m.expr with Some e -> signature_of_module_type_expr e | None -> None

let rec find_sg_map fn (sg : Odoc_model.Lang.Signature.t) =
  let rec inner = function
    | Odoc_model.Lang.Signature.Include i :: xs -> (
        match find_sg_map fn i.expansion.content with
        | None -> inner xs
        | Some y -> Some y)
    | x :: xs -> ( match fn x with Some y -> Some y | None -> inner xs)
    | [] -> None
  in
  inner sg.items

let find_module name sg =
  let open Odoc_model.Lang.Signature in
  find_sg_map
    (function
      | Module (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.Module m)
      | _ -> None)
    sg

let find_module_type name sg =
  let open Odoc_model.Lang.Signature in
  find_sg_map
    (function
      | ModuleType ({ id; _ } as m)
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.ModuleType m)
      | _ -> None)
    sg

let find_type name sg =
  let open Odoc_model.Lang.Signature in
  find_sg_map
    (function
      | Type (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.Type m)
      | ClassType (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.ClassType m)
      | Class (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.Class m)
      | _ -> None)
    sg

let find_value name sg =
  let open Odoc_model.Lang.Signature in
  find_sg_map
    (function
      | Value ({ id; _ } as m) when Odoc_model.Paths.Identifier.name id = name
        ->
          Some (Element.Value m)
      | _ -> None)
    sg

(* Really cut-down reference lookup! *)
let rec handle_ref :
    Odoc_model.Lang.Signature.t ->
    Odoc_model.Paths.Reference.t ->
    Element.t option =
 fun sg r ->
  let ( >>= ) m f = match m with Some x -> f x | None -> None in
  let ( ||> ) f1 f2 x = match f1 x with Some x -> Some x | None -> f2 x in
  let signature_of_element : Element.t -> Odoc_model.Lang.Signature.t option =
   fun e ->
    match e with
    | Element.Module m -> signature_of_module m
    | Element.ModuleType mt -> signature_of_module_type mt
    | _ -> None
  in
  match r with
  | `Root (n, `TUnknown) ->
      let find =
        find_module n ||> find_module_type n ||> find_type n ||> find_value n
      in
      find sg
      (* Assume this is a module *)
  | `Root (name, `TModule) -> find_module name sg
  | `Root (name, `TModuleType) -> find_module_type name sg
  | `Root (name, `TType) -> find_type name sg
  | `Dot (parent, n) ->
      let find =
        find_module n ||> find_module_type n ||> find_type n ||> find_value n
      in
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element >>= find
  | `Module (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.ModuleName.to_string m)
  | `ModuleType (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.ModuleTypeName.to_string m)
  | `Type (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.TypeName.to_string m)
  | `Value (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.ValueName.to_string m)
  | _ ->
      Format.eprintf "Reference unhandled\n%!";
      None

let print_element elt =
  match elt with
  | Element.Module m -> print_json_desc Lang_desc.module_t m
  | Element.ModuleType m -> print_json_desc Lang_desc.moduletype_t m
  | Element.Type t -> print_json_desc Lang_desc.typedecl_t t
  | Element.Value v -> print_json_desc Lang_desc.value_t v
  | Element.ClassType v -> print_json_desc Lang_desc.classtype_t v
  | Element.Class v -> print_json_desc Lang_desc.class_t v

let print_short c elt =
  let open Odoc_xref2 in
  let open Component.Fmt in
  match elt with
  | Element.Module m ->
      let m' = Component.Of_Lang.(module_ (empty ()) m) in
      Format.fprintf Format.std_formatter "@[<v 2>module %a %a@]"
        (model_identifier c)
        (m.id :> Odoc_model.Paths.Identifier.t)
        (module_ c) m'
  | Element.ModuleType m ->
      let m' = Component.Of_Lang.(module_type (empty ()) m) in
      Format.fprintf Format.std_formatter "@[<v 2>module type %a %a@]"
        (model_identifier c)
        (m.id :> Odoc_model.Paths.Identifier.t)
        (module_type c) m'
  | Element.Type t ->
      let t' = Component.Of_Lang.(type_decl (empty ()) t) in
      Format.fprintf Format.std_formatter "@[<v 2>type %a %a@]"
        (model_identifier c)
        (t.id :> Odoc_model.Paths.Identifier.t)
        (type_decl c) t'
  | Element.Value v ->
      let v' = Component.Of_Lang.(value (empty ()) v) in
      Format.fprintf Format.std_formatter "@[<v 2>val %a %a@]"
        (model_identifier c)
        (v.id :> Odoc_model.Paths.Identifier.t)
        (value c) v'
  | Element.ClassType ct ->
      let ct' = Component.Of_Lang.(class_type (empty ()) ct) in
      Format.fprintf Format.std_formatter "@[<v 2>val %a %a@]"
        (model_identifier c)
        (ct.id :> Odoc_model.Paths.Identifier.t)
        (class_type c) ct'
  | Element.Class cls ->
      let cls' = Component.Of_Lang.(class_ (empty ()) cls) in
      Format.fprintf Format.std_formatter "@[<v 2>val %a %a@]"
        (model_identifier c)
        (cls.id :> Odoc_model.Paths.Identifier.t)
        (class_ c) cls'

(* [--refs]: print the references in the doc comments of a unit or a page,
   one per line. A [{!modules:}] entry is printed as [modules:], followed by
   the references in its synopsis, indented. *)
module Refs = struct
  open Odoc_model
  open Odoc_model.Lang

  let print_ref c indent (r : Paths.Reference.t) =
    Format.printf "%s%a@." indent Odoc_xref2.Component.Fmt.(model_reference c) r

  let rec inline c indent (x : Comment.inline_element Location_.with_location) =
    match x.value with
    | `Reference (r, _) -> print_ref c indent r
    | `Styled (_, xs) -> List.iter (inline c indent) xs
    | `Space _ | `Word _ | `Code_span _ | `Math_span _ | `Raw_markup _ | `Link _
      ->
        ()

  and nestable c indent
      (x : Comment.nestable_block_element Location_.with_location) =
    match x.value with
    | `Paragraph p -> List.iter (inline c indent) p
    | `Code_block (_, _, Some output) -> List.iter (nestable c indent) output
    | `Code_block (_, _, None) | `Math_block _ | `Verbatim _ -> ()
    | `Modules ms -> List.iter (module_entry c indent) ms
    | `Table { data; _ } ->
        List.iter
          (List.iter (fun (cell, _) -> List.iter (nestable c indent) cell))
          data
    | `List (_, items) -> List.iter (List.iter (nestable c indent)) items
    | `Media (`Reference r, _, _) -> print_ref c indent (r :> Paths.Reference.t)
    | `Media (`Link _, _, _) -> ()

  and module_entry c indent { Comment.module_reference; module_synopsis } =
    Format.printf "%smodules: %a@." indent
      Odoc_xref2.Component.Fmt.(model_reference c)
      (module_reference :> Paths.Reference.t);
    match module_synopsis with
    | Some p -> List.iter (inline c (indent ^ "  ")) p
    | None -> ()

  let tag c indent (t : Comment.tag) =
    match t with
    | `Deprecated bs
    | `Param (_, bs)
    | `Return bs
    | `See (_, _, bs)
    | `Before (_, bs) ->
        List.iter (nestable c indent) bs
    | `Raise (`Reference (r, _), bs) ->
        print_ref c indent r;
        List.iter (nestable c indent) bs
    | `Raise (`Code_span _, bs) -> List.iter (nestable c indent) bs
    | `Author _ | `Since _ | `Version _ | `Alert _ -> ()

  let docs c (d : Comment.docs) =
    List.iter
      (fun (x : Comment.block_element Location_.with_location) ->
        match x.value with
        | #Comment.nestable_block_element as b ->
            nestable c "" { x with value = b }
        | `Heading (_, _, inls) -> List.iter (inline c "") inls
        | `Tag t -> tag c "" t)
      d.elements

  (* Only signatures written out in this unit are followed. A module type
     given by a path, [with] or [module type of], and the expansion of an
     include of a path, take their items from another signature, whose own
     references are printed when that signature is. *)
  let rec written_signature : ModuleType.U.expr -> bool = function
    | Signature _ -> true
    | With (_, e) | Strengthen (e, _, _) -> written_signature e
    | Path _ | TypeOf _ -> false

  let rec signature c (sg : Signature.t) =
    docs c sg.doc;
    List.iter (item c) sg.items

  and module_type_expr c (e : ModuleType.expr) =
    match e with
    | Signature sg -> signature c sg
    | Functor (p, body) ->
        functor_parameter c p;
        module_type_expr c body
    | Path _ | With _ | TypeOf _ | Strengthen _ -> ()

  and functor_parameter c (p : FunctorParameter.t) =
    match p with Named { expr; _ } -> module_type_expr c expr | Unit -> ()

  and constructor_argument c (a : TypeDecl.Constructor.argument) =
    match a with
    | Record fields ->
        List.iter (fun (f : TypeDecl.Field.t) -> docs c f.doc) fields
    | Tuple _ -> ()

  and type_decl c (t : TypeDecl.t) =
    docs c t.doc;
    (match t.representation with
    | Some (Variant cs) ->
        List.iter
          (fun (k : TypeDecl.Constructor.t) ->
            docs c k.doc;
            constructor_argument c k.args)
          cs
    | Some (Record fields) ->
        List.iter (fun (f : TypeDecl.Field.t) -> docs c f.doc) fields
    | Some (Record_unboxed_product fields) ->
        List.iter (fun (f : TypeDecl.UnboxedField.t) -> docs c f.doc) fields
    | Some Extensible | None -> ());
    (* Only a polymorphic variant at the top of a manifest has documented
       constructors. *)
    match t.equation.manifest with
    | Some (Polymorphic_variant { elements; _ }) ->
        List.iter
          (function
            | TypeExpr.Polymorphic_variant.Constructor k -> docs c k.doc
            | Type _ -> ())
          elements
    | Some _ | None -> ()

  and class_type_expr c (e : ClassType.expr) =
    match e with Signature cs -> class_signature c cs | Constr _ -> ()

  and class_decl c (d : Class.decl) =
    match d with
    | ClassType e -> class_type_expr c e
    | Arrow (_, _, d) -> class_decl c d

  and class_signature c (cs : ClassSignature.t) =
    docs c cs.doc;
    List.iter
      (fun (i : ClassSignature.item) ->
        match i with
        | Method m -> docs c m.doc
        | InstanceVariable v -> docs c v.doc
        | Constraint x -> docs c x.doc
        | Inherit x -> docs c x.doc
        | Comment (`Docs d) -> docs c d
        | Comment `Stop -> ())
      cs.items

  and item c (i : Signature.item) =
    match i with
    | Module (_, m) -> (
        docs c m.doc;
        match m.type_ with
        | ModuleType e -> module_type_expr c e
        | Alias _ -> ())
    | ModuleType mt -> (
        docs c mt.doc;
        match mt.expr with Some e -> module_type_expr c e | None -> ())
    | ModuleSubstitution x -> docs c x.doc
    | ModuleTypeSubstitution x ->
        docs c x.doc;
        module_type_expr c x.manifest
    | Open x -> docs c x.doc
    | Type (_, x) | TypeSubstitution x -> type_decl c x
    | TypExt x ->
        docs c x.doc;
        List.iter
          (fun (k : Extension.Constructor.t) ->
            docs c k.doc;
            constructor_argument c k.args)
          x.constructors
    | Exception x ->
        docs c x.doc;
        constructor_argument c x.args
    | Value x -> docs c x.doc
    | Class (_, x) ->
        docs c x.doc;
        class_decl c x.type_
    | ClassType (_, x) ->
        docs c x.doc;
        class_type_expr c x.expr
    | Include x -> (
        docs c x.doc;
        match x.decl with
        | ModuleType e when written_signature e ->
            (* Compiling empties a signature written here, so its items are
               read from the expansion. *)
            signature c x.expansion.content
        | ModuleType _ | Alias _ | Functor _ -> ())
    | Comment (`Docs d) -> docs c d
    | Comment `Stop -> ()
end

let run inp short long_paths show_canonical show_expansions
    show_include_expansions show_removed show_root_parents refs ref =
  let inp = Fpath.v inp in
  let c =
    {
      Odoc_xref2.Component.Fmt.short_paths = not long_paths;
      show_canonical;
      show_expansions;
      show_include_expansions;
      show_removed;
      identifier_name_only = false;
      show_root_parents;
    }
  in
  Odoc_file.load inp >>= fun unit ->
  if refs then
    if
      Option.is_some ref || short || show_canonical || show_expansions
      || show_include_expansions || show_removed
    then
      Error
        (`Msg
           "--refs can only be used with --long-paths and --show-root-parents")
    else
      match unit.content with
      | Page_content page ->
          Refs.docs c page.content;
          Ok ()
      | Unit_content { content = Module sg; _ } ->
          Refs.signature c sg;
          Ok ()
      | Unit_content { content = Pack _; _ } ->
          Error (`Msg "--refs can't print the references of a packed unit")
      | Impl_content _ | Asset_content _ ->
          Error
            (`Msg "--refs can only print the references of a unit or a page")
  else
    match unit.content with
    | Odoc_file.Page_content page ->
        print_json_desc Lang_desc.page_t page;
        Ok ()
    | Odoc_file.Impl_content impl ->
        print_json_desc Lang_desc.implementation_t impl;
        Ok ()
    | Unit_content u -> (
        match (short, ref, u.content) with
        | true, None, Module sg ->
            let sg' = Odoc_xref2.Component.Of_Lang.(signature (empty ()) sg) in
            Format.printf "%a\n%!" Odoc_xref2.Component.Fmt.(signature c) sg';
            Ok ()
        | _, Some r, Module sg -> (
            let r = Odoc_model.Semantics.parse_reference r in
            match Odoc_model.Error.raise_warnings r with
            | Ok r -> (
                match handle_ref sg r with
                | Some elt ->
                    if short then print_short c elt else print_element elt;
                    Ok ()
                | None -> Ok ())
            | _ -> Ok ())
        | true, None, _ -> Error (`Msg "Can't short-print packed modules")
        | _, Some _, _ -> Error (`Msg "Can't look up in packed modules")
        | false, None, _ ->
            print_json_desc Lang_desc.compilation_unit_t u;
            Ok ())
    | Asset_content a ->
        print_json_desc Lang_desc.asset_t a;
        Ok ()

open Cmdliner

let reference =
  let doc = "reference to print" in
  Arg.(value & opt (some string) None & info ~doc [ "r" ])

let a_inp =
  let doc = "Input file." in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PATH" [])

let a_short =
  let doc = "Short output." in
  Arg.(value & flag & info ~doc [ "short" ])

let a_show_expansions =
  let doc = "Show expansions in short output" in
  Arg.(value & flag & info ~doc [ "show-expansions" ])

let a_long_paths =
  let doc = "Show long paths in short output and with $(b,--refs)." in
  Arg.(value & flag & info ~doc [ "long-paths" ])

let a_show_canonical =
  let doc = "Show modules canonical reference in short output" in
  Arg.(value & flag & info ~doc [ "show-canonical" ])

let a_show_include_expansions =
  let doc = "Show include expansions in short output" in
  Arg.(value & flag & info ~doc [ "show-include-expansions" ])

let a_show_removed =
  let doc = "Show removed items in signature expansions in short output." in
  Arg.(value & flag & info ~doc [ "show-removed" ])

let a_show_root_parents =
  let doc =
    "Show the parent pages of root modules, in short output and with \
     $(b,--refs). In short output, use $(b,--long-paths) to see every root, \
     not only canonical paths."
  in
  Arg.(value & flag & info ~doc [ "show-root-parents" ])

let a_refs =
  let doc =
    "Print the references in the doc comments of a unit or a page, one per \
     line, instead of the content. Signatures written in the unit are \
     followed. A module type given by a path or by $(b,module type of) is not \
     followed. An include of such a module type is not followed either. Their \
     items come from another signature. References after a $(b,(**/**)) stop \
     comment are printed, although odoc does not render them. A resolved \
     reference can have hidden and canonical parts, and only $(b,--long-paths) \
     shows them. $(b,--show-root-parents) applies too. No other option can be \
     used with $(b,--refs)."
  in
  Arg.(value & flag & info ~doc [ "refs" ])

let cmd =
  let doc = "Print the content of .odoc files into a text format. For tests" in
  Cmd.v (Cmd.info "odoc_print" ~doc)
  @@ Term.(
       const run $ a_inp $ a_short $ a_long_paths $ a_show_canonical
       $ a_show_expansions $ a_show_include_expansions $ a_show_removed
       $ a_show_root_parents $ a_refs $ reference)

let () =
  match Cmd.eval_value' cmd with
  | `Ok (Ok ()) -> ()
  | `Ok (Error (`Msg msg)) ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | `Exit c -> exit c
