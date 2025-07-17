open Odoc_model
open Names
module Tools_error = struct
  open Paths
  (** Errors raised by Tools *)

  type handle_subs_error =
    [ `UnresolvedPath of [ `Module of Cpath.module_ ]
      (* Failed to resolve a module path when applying a fragment item *) ]

  type reference_kind =
    [ `S
    | `T
    | `C
    | `CT
    | `Page
    | `Cons
    | `Field
    | `UnboxedField
    | `Label
    | `Page_path
    | `Module_path
    | `Asset_path
    | `Any_path ]

  type path_kind = [ `Page | `Unit ]

  type expansion_of_module_error =
    [ `OpaqueModule (* The module does not have an expansion *)
    | `UnresolvedForwardPath
      (* The module signature depends upon a forward path *)
    | `UnresolvedPath of
      [ `Module of Cpath.module_ * simple_module_lookup_error
      | `ModuleType of Cpath.module_type * simple_module_type_lookup_error ]
      (* The path to the module or module type could not be resolved *)
    | `UnresolvedOriginalPath of Cpath.module_ * simple_module_lookup_error ]

  and simple_module_lookup_error =
    [ `Local of Env.t * Ident.module_
      (* Internal error: Found local path during lookup *)
    | `Find_failure
    | (* Internal error: the module was not found in the parent signature *)
      `Lookup_failure of Identifier.Path.Module.t
      (* Could not find the module in the environment *)
    | `Lookup_failure_root of ModuleName.t (* Could not find the root module *)
    | `Parent of parent_lookup_error ]

  and simple_module_type_expr_of_module_error =
    [ `ApplyNotFunctor
      (* Internal error: attempt made to apply a module that's not a functor *)
    | `OpaqueModule (* The module does not have an expansion *)
    | `UnresolvedForwardPath
      (* The module signature depends upon a forward path *)
    | `UnresolvedPath of
      [ `Module of Cpath.module_ * simple_module_lookup_error
      | `ModuleType of Cpath.module_type * simple_module_type_lookup_error ]
    | `Parent of parent_lookup_error ]

  and simple_module_type_lookup_error =
    [ `LocalMT of Env.t * Ident.module_type
      (* Internal error: Found local path during lookup *)
    | `Find_failure
      (* Internal error: the module was not found in the parent signature *)
    | `Lookup_failureMT of Identifier.ModuleType.t
      (* Could not find the module in the environment *)
    | `Parent of parent_lookup_error ]

  and simple_type_lookup_error =
    [ `LocalType of Env.t * Ident.type_
      (* Internal error: Found local path during lookup *)
    | `Class_replaced
      (* Class was replaced with a destructive substitution and we're not sure
          what to do now *)
    | `OpaqueClass (* Couldn't resolve class signature. *)
    | `Find_failure
      (* Internal error: the type was not found in the parent signature *)
    | `Lookup_failureT of Identifier.Path.Type.t
      (* Could not find the module in the environment *)
    | `Parent of parent_lookup_error ]

  and simple_value_lookup_error =
    [ `LocalValue of Env.t * Ident.value
      (* Internal error: Found local path during lookup *)
    | `Find_failure
      (* Internal error: the type was not found in the parent signature *)
    | `Lookup_failureV of Identifier.Path.Value.t
      (* Could not find the module in the environment *)
    | `Parent of parent_lookup_error ]

  and parent_lookup_error =
    [ `Parent_sig of expansion_of_module_error
      (* Error found while calculating the parent signature *)
    | `Parent_module_type of simple_module_type_lookup_error
      (* Error found while looking up parent module type *)
    | `Parent_expr of simple_module_type_expr_of_module_error
      (* Error found while evaluating parent module expression *)
    | `Parent_module of simple_module_lookup_error
      (* Error found while looking up parent module *)
    | `Parent_type of simple_type_lookup_error
    | `Fragment_root (* Encountered unexpected fragment root *)
    | `Parent of parent_lookup_error
    | `Reference of reference_lookup_error ]

  and reference_lookup_error =
    [ `Wrong_kind of reference_kind list * reference_kind (* Expected, got *)
    | `Lookup_by_name of [ reference_kind | `Any ] * string
    | `Find_by_name of [ reference_kind | `Any ] * string
    | `Path_error of
      [ `Not_found | `Is_directory | `Wrong_kind of path_kind list * path_kind ]
      * Reference.tag_hierarchy
      * string list
    | `Parent of parent_lookup_error ]

  type any =
    [ simple_type_lookup_error
    | simple_value_lookup_error
    | simple_module_type_lookup_error
    | simple_module_type_expr_of_module_error
    | simple_module_lookup_error
    | expansion_of_module_error
    | parent_lookup_error ]

  let pp_reference_kind fmt k =
    let k =
      match k with
      | `S -> "signature"
      | `T -> "type"
      | `C -> "class"
      | `CT -> "class type"
      | `Page -> "page"
      | `Cons -> "constructor"
      | `Field -> "field"
      | `UnboxedField -> "unboxed field"
      | `Label -> "label"
      | `Page_path -> "path to a page"
      | `Module_path -> "path to a module"
      | `Asset_path -> "path to an asset"
      | `Any_path -> "path"
    in
    Format.pp_print_string fmt k

  let fpf = Format.fprintf

  let pp_human_list pp_a fmt lst =
    match List.rev lst with
    | [] -> ()
    | [ one ] -> pp_a fmt one
    | last :: rev_tl ->
        let pp_sep fmt () = fpf fmt ", " in
        fpf fmt "%a and %a" (Format.pp_print_list ~pp_sep pp_a) rev_tl pp_a last

  let pp_path fmt (tag, p) =
    let tag =
      match tag with
      | `TRelativePath -> ""
      | `TAbsolutePath -> "/"
      | `TCurrentPackage -> "//"
    in
    let pp_sep fmt () = fpf fmt "/" in
    fpf fmt "%s%a" tag (Format.pp_print_list ~pp_sep Format.pp_print_string) p

  let pp_path_kind fmt = function
    | `Unit -> fpf fmt "module"
    | `Page -> fpf fmt "page"

  let pp_path_error fmt err tag path =
    match err with
    | `Not_found -> fpf fmt "Path '%a' not found" pp_path (tag, path)
    | `Is_directory ->
        fpf fmt "Path '%a' points to directory" pp_path (tag, path)
    | `Wrong_kind (expected, got) ->
        fpf fmt "Path '%a' is of kind %a but was expected %a" pp_path
          (tag, path) pp_path_kind got
          (pp_human_list pp_path_kind)
          expected

  let rec pp : Format.formatter -> any -> unit =
   fun fmt err ->
    let open Component.Fmt in
    let c = default in
    match err with
    | `OpaqueModule -> Format.fprintf fmt "OpaqueModule"
    | `OpaqueClass -> Format.fprintf fmt "Class is abstract"
    | `UnresolvedForwardPath -> Format.fprintf fmt "Unresolved forward path"
    | `UnresolvedPath (`Module (p, e)) ->
        Format.fprintf fmt "Unresolved module path %a (%a)" (module_path c) p pp
          (e :> any)
    | `UnresolvedPath (`ModuleType (p, e)) ->
        Format.fprintf fmt "Unresolved module type path %a (%a)"
          (module_type_path c) p pp
          (e :> any)
    | `UnresolvedOriginalPath (p, e) ->
        Format.fprintf fmt "Unresolved original module path %a (%a)"
          Component.Fmt.(module_path default)
          p pp
          (e :> any)
    | `LocalMT (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `Local (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `LocalType (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `LocalValue (_, id) ->
        Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `Find_failure -> Format.fprintf fmt "Find failure"
    | `Lookup_failure m ->
        Format.fprintf fmt "Lookup failure (module): %a" (model_identifier c)
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failure_root r ->
        Format.fprintf fmt "Lookup failure (root module): %a" ModuleName.fmt r
    | `Lookup_failureMT m ->
        Format.fprintf fmt "Lookup failure (module type): %a"
          (model_identifier c)
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failureT m ->
        Format.fprintf fmt "Lookup failure (type): %a" (model_identifier c)
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failureV m ->
        Format.fprintf fmt "Lookup failure (value): %a" (model_identifier c)
          (m :> Odoc_model.Paths.Identifier.t)
    | `ApplyNotFunctor -> Format.fprintf fmt "Apply module is not a functor"
    | `Class_replaced -> Format.fprintf fmt "Class replaced"
    | `Parent p -> pp fmt (p :> any)
    | `Parent_sig e -> Format.fprintf fmt "Parent_sig: %a" pp (e :> any)
    | `Parent_module_type e ->
        Format.fprintf fmt "Parent_module_type: %a" pp (e :> any)
    | `Parent_expr e -> Format.fprintf fmt "Parent_expr: %a" pp (e :> any)
    | `Parent_module e -> Format.fprintf fmt "Parent_module: %a" pp (e :> any)
    | `Fragment_root -> Format.fprintf fmt "Fragment root"
    | `Parent_type e -> Format.fprintf fmt "Parent_type: %a" pp (e :> any)
    | `Reference e -> pp_reference_lookup_error fmt e

  and pp_reference_lookup_error fmt = function
    | `Wrong_kind (expected, got) ->
        let pp_sep fmt () = Format.fprintf fmt " or " in
        Format.fprintf fmt "is of kind %a but expected %a" pp_reference_kind got
          (Format.pp_print_list ~pp_sep pp_reference_kind)
          expected
    | `Lookup_by_name (kind, name) | `Find_by_name (kind, name) -> (
        match kind with
        | `Any -> Format.fprintf fmt "Couldn't find %S" name
        | #reference_kind as kind ->
            Format.fprintf fmt "Couldn't find %a %S" pp_reference_kind kind name
        )
    | `Path_error (err, tag, path) -> pp_path_error fmt err tag path
    | `Parent e -> pp fmt (e :> any)
end

type kind = [ `OpaqueModule | `Root of string ]

let rec kind_of_module_cpath = function
  | `Root name -> Some (`Root (ModuleName.to_string name))
  | `Substituted p' | `Dot (p', _) -> kind_of_module_cpath p'
  | `Apply (a, b) -> (
      match kind_of_module_cpath a with
      | Some _ as a -> a
      | None -> kind_of_module_cpath b)
  | _ -> None

let rec kind_of_module_type_cpath = function
  | `Substituted p' -> kind_of_module_type_cpath p'
  | `DotMT (p', _) -> kind_of_module_cpath p'
  | _ -> None

(** [Some (`Root _)] for errors during lookup of root modules or [None] for
    other errors. *)
let rec kind_of_error : Tools_error.any -> kind option = function
  | `UnresolvedPath (`Module (cp, e)) -> (
      match kind_of_module_cpath cp with
      | None -> kind_of_error (e :> Tools_error.any)
      | x -> x)
  | `UnresolvedPath (`ModuleType (cp, e)) -> (
      match kind_of_module_type_cpath cp with
      | None -> kind_of_error (e :> Tools_error.any)
      | x -> x)
  | `Lookup_failure { iv = `Root (_, name); _ } ->
      Some (`Root (ModuleName.to_string name))
  | `Lookup_failure_root name -> Some (`Root (ModuleName.to_string name))
  | `Parent (`Parent_sig e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_module_type e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_expr e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_module e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent _ as e) -> kind_of_error (e :> Tools_error.any)
  | `OpaqueModule ->
      (* Don't turn OpaqueModule warnings into errors *)
      Some `OpaqueModule
  | _ -> None

and kind_of_type_of_desc : Component.ModuleType.type_of_desc -> kind option =
  function
  | ModPath cp -> kind_of_module_cpath cp
  | StructInclude cp -> kind_of_module_cpath cp

let kind_of_error ~what = function
  | Some e -> kind_of_error (e :> Tools_error.any)
  | None -> (
      match what with
      | `Include (Component.Include.Alias cp) -> kind_of_module_cpath cp
      | `Module { Odoc_model.Paths.Identifier.iv = `Root (_, name); _ } ->
          Some (`Root (ModuleName.to_string name))
      | _ -> None)

open Paths

type what =
  [ `Functor_parameter of Identifier.FunctorParameter.t
  | `Value of Identifier.Value.t
  | `Value_path of Cpath.value
  | `Class of Identifier.Class.t
  | `Class_type of Identifier.ClassType.t
  | `Module of Identifier.Module.t
  | `Module_type of Identifier.Signature.t
  | `Module_path of Cpath.module_
  | `Module_type_path of Cpath.module_type
  | `Module_type_U of Component.ModuleType.U.expr
  | `Include of Component.Include.decl
  | `Package of Cpath.module_type
  | `Type of Cfrag.type_
  | `Type_path of Cpath.type_
  | `Class_type_path of Cpath.class_type
  | `With_module of Cfrag.module_
  | `With_module_type of Cfrag.module_type
  | `With_type of Cfrag.type_
  | `Module_type_expr of Component.ModuleType.expr
  | `Module_type_u_expr of Component.ModuleType.U.expr
  | `Child_module of string
  | `Child_page of string
  | `Reference of Reference.t ]

let report ~(what : what) ?tools_error action =
  let action =
    match action with
    | `Lookup -> "lookup"
    | `Expand -> "compile expansion for"
    | `Resolve_module_type -> "resolve type of"
    | `Resolve -> "resolve"
    | `Compile -> "compile"
  in
  let pp_tools_error fmt = function
    | Some e -> Format.fprintf fmt " %a" Tools_error.pp (e :> Tools_error.any)
    | None -> ()
  in
  let open Component.Fmt in
  let report_internal_error () =
    let r subject pp_a a =
      Lookup_failures.report_internal "Failed to %s %s %a%a" action subject pp_a
        a pp_tools_error tools_error
    in
    let c = default in
    let fmt_id fmt id = model_identifier c fmt (id :> Paths.Identifier.t) in
    match what with
    | `Functor_parameter id -> r "functor parameter" fmt_id id
    | `Value id -> r "value" fmt_id id
    | `Class id -> r "class" fmt_id id
    | `Class_type id -> r "class type" fmt_id id
    | `Module id -> r "module" fmt_id id
    | `Module_type id -> r "module type" fmt_id id
    | `Module_path path -> r "module path" (module_path c) path
    | `Module_type_path path -> r "module type path" (module_type_path c) path
    | `Module_type_U expr -> r "module type expr" (u_module_type_expr c) expr
    | `Include decl -> r "include" (include_decl c) decl
    | `Package path ->
        r "module package" (module_type_path c) (path :> Cpath.module_type)
    | `Type cfrag -> r "type" (type_fragment c) cfrag
    | `Type_path path -> r "type" (type_path c) path
    | `Value_path path -> r "value" (value_path c) path
    | `Class_type_path path -> r "class_type" (class_type_path c) path
    | `With_module frag -> r "module substitution" (module_fragment c) frag
    | `With_module_type frag ->
        r "module type substitution" (module_type_fragment c) frag
    | `With_type frag -> r "type substitution" (type_fragment c) frag
    | `Module_type_expr cexpr ->
        r "module type expression" (module_type_expr c) cexpr
    | `Module_type_u_expr cexpr ->
        r "module type u expression" (u_module_type_expr c) cexpr
    | `Child_module rf -> r "child module" Astring.String.pp rf
    | `Child_page rf -> r "child page" Astring.String.pp rf
    | `Reference ref -> r "reference" (model_reference c) ref
  in
  match kind_of_error ~what tools_error with
  | Some (`Root name) -> Lookup_failures.report_root ~name
  | Some `OpaqueModule -> report_internal_error ()
  | None -> report_internal_error ()
