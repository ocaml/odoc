(** Print .odocl files. *)

open Odoc_odoc
open Odoc_odoc.Or_error
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

let find_map fn list =
  let rec inner = function
    | x :: xs -> ( match fn x with Some y -> Some y | None -> inner xs)
    | [] -> None
  in
  inner list

let find_module name sg =
  let open Odoc_model.Lang.Signature in
  find_map
    (function
      | Module (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.Module m)
      | _ -> None)
    sg.items

let find_module_type name sg =
  let open Odoc_model.Lang.Signature in
  find_map
    (function
      | ModuleType ({ id; _ } as m)
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.ModuleType m)
      | _ -> None)
    sg.items

let find_type name sg =
  let open Odoc_model.Lang.Signature in
  find_map
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
    sg.items

let find_value name sg =
  let open Odoc_model.Lang.Signature in
  find_map
    (function
      | Value ({ id; _ } as m) when Odoc_model.Paths.Identifier.name id = name
        ->
          Some (Element.Value m)
      | _ -> None)
    sg.items

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

let run inp ref =
  let inp = Fpath.v inp in
  Odoc_file.load inp >>= fun unit ->
  match unit.content with
  | Odoc_file.Source_tree_content tree ->
      print_json_desc Lang_desc.source_tree_page_t tree;
      Ok ()
  | Odoc_file.Page_content page ->
      print_json_desc Lang_desc.page_t page;
      Ok ()
  | Unit_content u -> (
      match ref with
      | None ->
          print_json_desc Lang_desc.compilation_unit_t u;
          Ok ()
      | Some r -> (
          let r = Odoc_model.Semantics.parse_reference r in
          let sg =
            match u.content with
            | Module m -> m
            | Pack _ -> failwith "Can't look up in packed modules"
          in
          match Odoc_model.Error.raise_warnings r with
          | Ok r -> (
              match handle_ref sg r with
              | Some elt ->
                  print_element elt;
                  Ok ()
              | None -> Ok ())
          | _ -> Ok ()))

open Compatcmdliner

let reference =
  let doc = "reference to print" in
  Arg.(value & opt (some string) None & info ~doc [ "r" ])

let a_inp =
  let doc = "Input file." in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PATH" [])

let term =
  let doc = "Print the content of .odoc files into a text format. For tests" in
  Term.(const run $ a_inp $ reference, info "odoc_print" ~doc)

let () =
  match Term.eval term with
  | `Ok (Ok ()) -> ()
  | `Ok (Error (`Msg msg)) ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | (`Version | `Help | `Error _) as x -> Term.exit x
