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
  | Strengthen { s_expansion = Some e; _ } -> Some (signature_of_simple_expansion e)
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

let run inp short long_paths show_canonical show_expansions
    show_include_expansions show_removed ref =
  let inp = Fpath.v inp in
  let c =
    {
      Odoc_xref2.Component.Fmt.short_paths = not long_paths;
      show_canonical;
      show_expansions;
      show_include_expansions;
      show_removed;
    }
  in
  Odoc_file.load inp >>= fun unit ->
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
  let doc = "Show long paths in short output" in
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

let cmd =
  let doc = "Print the content of .odoc files into a text format. For tests" in
  Cmd.v (Cmd.info "odoc_print" ~doc)
  @@ Term.(
       const run $ a_inp $ a_short $ a_long_paths $ a_show_canonical
       $ a_show_expansions $ a_show_include_expansions $ a_show_removed
       $ reference)

let () =
  match Cmd.eval_value' cmd with
  | `Ok (Ok ()) -> ()
  | `Ok (Error (`Msg msg)) ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | `Exit c -> exit c
