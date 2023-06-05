open Type_desc
open Odoc_model
open Paths_desc
open Comment_desc
module T = Type_desc

module Digest = struct
  let t : Digest.t t = To_string (fun _ -> "<digest>")
end

let inline_status =
  Variant
    (function
    | `Default -> C0 "`Default"
    | `Open -> C0 "`Open"
    | `Closed -> C0 "`Closed"
    | `Inline -> C0 "`Inline")

let source_info =
  let open Lang.Source_info in
  Record [ F ("id", (fun t -> t.id), identifier) ]

(** {3 Module} *)

let rec module_decl =
  let open Lang.Module in
  Variant
    (function
    | Alias (x, y) ->
        C
          ( "Alias",
            ((x :> Paths.Path.t), y),
            Pair (path, Option simple_expansion) )
    | ModuleType x -> C ("ModuleType", x, moduletype_expr))

and module_t =
  let open Lang.Module in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("type_", (fun t -> t.type_), module_decl);
      F
        ( "canonical",
          (fun t -> (t.canonical :> Paths.Path.t option)),
          Option path );
      F ("hidden", (fun t -> t.hidden), bool);
    ]

(** {3 FunctorParameter} *)
and functorparameter_parameter =
  let open Lang.FunctorParameter in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("expr", (fun t -> t.expr), moduletype_expr);
    ]

and functorparameter_t =
  let open Lang.FunctorParameter in
  Variant
    (function
    | Unit -> C0 "Unit" | Named x -> C ("Named", x, functorparameter_parameter))

(** {3 ModuleType} *)

and moduletype_substitution =
  let open Lang.ModuleType in
  Variant
    (function
    | ModuleEq (x1, x2) ->
        C
          ( "ModuleEq",
            ((x1 :> Paths.Fragment.t), x2),
            Pair (fragment, module_decl) )
    | ModuleTypeEq (x1, x2) ->
        C
          ( "ModuleTypeEq",
            ((x1 :> Paths.Fragment.t), x2),
            Pair (fragment, moduletype_expr) )
    | TypeEq (x1, x2) ->
        C
          ( "TypeEq",
            ((x1 :> Paths.Fragment.t), x2),
            Pair (fragment, typedecl_equation) )
    | ModuleSubst (x1, x2) ->
        C
          ( "ModuleSubst",
            ((x1 :> Paths.Fragment.t), (x2 :> Paths.Path.t)),
            Pair (fragment, path) )
    | ModuleTypeSubst (x1, x2) ->
        C
          ( "ModuleTypeSubst",
            ((x1 :> Paths.Fragment.t), x2),
            Pair (fragment, moduletype_expr) )
    | TypeSubst (x1, x2) ->
        C
          ( "TypeSubst",
            ((x1 :> Paths.Fragment.t), x2),
            Pair (fragment, typedecl_equation) ))

and moduletype_type_of_desc =
  let open Lang.ModuleType in
  Variant
    (function
    | ModPath x -> C ("ModPath", (x :> Paths.Path.t), path)
    | StructInclude x -> C ("StructInclude", (x :> Paths.Path.t), path))

and simple_expansion : Lang.ModuleType.simple_expansion T.t =
  let open Lang.ModuleType in
  Variant
    (function
    | Signature sg -> C ("Signature", sg, signature_t)
    | Functor (p, e) ->
        C ("Functor", (p, e), Pair (functorparameter_t, simple_expansion)))

and moduletype_path_t =
  let open Lang.ModuleType in
  Record
    [
      F ("p_expansion", (fun t -> t.p_expansion), Option simple_expansion);
      F ("p_path", (fun t -> (t.p_path :> Paths.Path.t)), path);
    ]

and moduletype_with_t =
  let open Lang.ModuleType in
  Record
    [
      F
        ( "w_substitutions",
          (fun t -> t.w_substitutions),
          List moduletype_substitution );
      F ("w_expansion", (fun t -> t.w_expansion), Option simple_expansion);
      F ("w_expr", (fun t -> t.w_expr), moduletype_u_expr);
    ]

and moduletype_typeof_t =
  let open Lang.ModuleType in
  Record
    [
      F ("t_desc", (fun t -> t.t_desc), moduletype_type_of_desc);
      F ("t_expansion", (fun t -> t.t_expansion), Option simple_expansion);
    ]

and moduletype_expr =
  let open Lang.ModuleType in
  Variant
    (function
    | Path x -> C ("Path", x, moduletype_path_t)
    | Signature x -> C ("Signature", x, signature_t)
    | Functor (x1, x2) ->
        C ("Functor", (x1, x2), Pair (functorparameter_t, moduletype_expr))
    | With t -> C ("With", t, moduletype_with_t)
    | TypeOf x -> C ("TypeOf", x, moduletype_typeof_t))

and moduletype_u_expr =
  let open Lang.ModuleType.U in
  Variant
    (function
    | Path x -> C ("Path", (x :> Paths.Path.t), path)
    | Signature x -> C ("Signature", x, signature_t)
    | With (t, e) ->
        C
          ( "With",
            (t, e),
            Pair (List moduletype_substitution, moduletype_u_expr) )
    | TypeOf x -> C ("TypeOf", x, moduletype_typeof_t))

and moduletype_t =
  let open Lang.ModuleType in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F
        ( "canonical",
          (fun t -> (t.canonical :> Paths.Path.t option)),
          Option path );
      F ("expr", (fun t -> t.expr), Option moduletype_expr);
    ]

(** {3 ModuleSubstitution} *)

and modulesubstitution_t =
  let open Lang.ModuleSubstitution in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("manifest", (fun t -> (t.manifest :> Paths.Path.t)), path);
    ]

(** {3 ModuleTypeSubstitution} *)

and moduletypesubstitution_t =
  let open Lang.ModuleTypeSubstitution in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("manifest", (fun t -> t.manifest), moduletype_expr);
    ]

(** {3 Signature} *)

and signature_recursive =
  let open Lang.Signature in
  Variant
    (function
    | Ordinary -> C0 "Ordinary"
    | And -> C0 "And"
    | Nonrec -> C0 "Nonrec"
    | Rec -> C0 "Rec")

and signature_item =
  let open Lang.Signature in
  Variant
    (function
    | Module (x1, x2) ->
        C ("Module", (x1, x2), Pair (signature_recursive, module_t))
    | ModuleType x -> C ("ModuleType", x, moduletype_t)
    | ModuleSubstitution x -> C ("ModuleSubstitution", x, modulesubstitution_t)
    | ModuleTypeSubstitution x ->
        C ("ModuleTypeSubstitution", x, moduletypesubstitution_t)
    | Open x -> C ("Open", x, open_t)
    | Type (x1, x2) ->
        C ("Type", (x1, x2), Pair (signature_recursive, typedecl_t))
    | TypeSubstitution x -> C ("TypeSubstitution", x, typedecl_t)
    | TypExt x -> C ("TypExt", x, extension_t)
    | Exception x -> C ("Exception", x, exception_t)
    | Value x -> C ("Value", x, value_t)
    | Class (x1, x2) ->
        C ("Class", (x1, x2), Pair (signature_recursive, class_t))
    | ClassType (x1, x2) ->
        C ("ClassType", (x1, x2), Pair (signature_recursive, classtype_t))
    | Include x -> C ("Include", x, include_t)
    | Comment x -> C ("Comment", x, docs_or_stop))

and signature_t : Lang.Signature.t Type_desc.t =
  Record
    [
      F ("items", (fun t -> t.items), List signature_item);
      F ("compiled", (fun t -> t.compiled), bool);
      F ("doc", (fun t -> t.doc), docs);
    ]

(** {3 Open} *)
and open_t =
  let open Lang.Open in
  Record [ F ("expansion", (fun t -> t.expansion), signature_t) ]

(** {3 Include} *)

and include_shadowed =
  let open Lang.Include in
  Record
    [
      F ("s_modules", (fun t -> t.s_modules), List string);
      F ("s_module_types", (fun t -> t.s_module_types), List string);
      F ("s_values", (fun t -> t.s_values), List string);
      F ("s_types", (fun t -> t.s_types), List string);
      F ("s_classes", (fun t -> t.s_classes), List string);
      F ("s_class_types", (fun t -> t.s_class_types), List string);
    ]

and include_expansion =
  let open Lang.Include in
  Record
    [
      F ("shadowed", (fun t -> t.shadowed), include_shadowed);
      F ("content", (fun t -> t.content), signature_t);
    ]

and include_decl =
  let open Lang.Include in
  Variant
    (function
    | Alias p -> C ("Alias", (p :> Paths.Path.t), path)
    | ModuleType e -> C ("ModuleType", e, moduletype_u_expr))

and include_t =
  let open Lang.Include in
  Record
    [
      F ("parent", (fun t -> t.parent), identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("decl", (fun t -> t.decl), include_decl);
      F ("status", (fun t -> t.status), inline_status);
      F ("expansion", (fun t -> t.expansion), include_expansion);
    ]

(** {3 TypeDecl} *)
and typedecl_field =
  let open Lang.TypeDecl.Field in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("mutable_", (fun t -> t.mutable_), bool);
      F ("type_", (fun t -> t.type_), typeexpr_t);
    ]

and typedecl_constructor_argument =
  let open Lang.TypeDecl.Constructor in
  T.Variant
    (function
    | Tuple x -> C ("Tuple", x, List typeexpr_t)
    | Record x -> C ("Record", x, List typedecl_field))

and typedecl_constructor =
  let open Lang.TypeDecl.Constructor in
  T.Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("args", (fun t -> t.args), typedecl_constructor_argument);
      F ("res", (fun t -> t.res), Option typeexpr_t);
    ]

and typedecl_representation =
  let open Lang.TypeDecl.Representation in
  T.Variant
    (function
    | Variant x -> C ("Variant", x, List typedecl_constructor)
    | Record x -> C ("Record", x, List typedecl_field)
    | Extensible -> C0 "Extensible")

and typedecl_variance =
  let open Lang.TypeDecl in
  Variant (function Pos -> C0 "Pos" | Neg -> C0 "Neg")

and typedecl_param_desc =
  let open Lang.TypeDecl in
  Variant (function Any -> C0 "Any" | Var x -> C ("Var", x, string))

and typedecl_param =
  let open Lang.TypeDecl in
  Record
    [
      F ("desc", (fun t -> t.desc), typedecl_param_desc);
      F ("variance", (fun t -> t.variance), Option typedecl_variance);
      F ("injectivity", (fun t -> t.injectivity), bool);
    ]

and typedecl_equation =
  let open Lang.TypeDecl.Equation in
  Record
    [
      F ("params", (fun t -> t.params), List typedecl_param);
      F ("private_", (fun t -> t.private_), bool);
      F ("manifest", (fun t -> t.manifest), Option typeexpr_t);
      F
        ( "constraints",
          (fun t -> t.constraints),
          List (Pair (typeexpr_t, typeexpr_t)) );
    ]

and typedecl_t =
  let open Lang.TypeDecl in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("equation", (fun t -> t.equation), typedecl_equation);
      F
        ( "representation",
          (fun t -> t.representation),
          Option typedecl_representation );
    ]

(** {3 Extension} *)
and extension_constructor =
  let open Lang.Extension.Constructor in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("args", (fun t -> t.args), typedecl_constructor_argument);
      F ("res", (fun t -> t.res), Option typeexpr_t);
    ]

and extension_t =
  let open Lang.Extension in
  Record
    [
      F ("type_path", (fun t -> (t.type_path :> Paths.Path.t)), path);
      F ("doc", (fun t -> t.doc), docs);
      F ("type_params", (fun t -> t.type_params), List typedecl_param);
      F ("private_", (fun t -> t.private_), bool);
      F ("constructors", (fun t -> t.constructors), List extension_constructor);
    ]

(** {3 Exception} *)

and exception_t =
  let open Lang.Exception in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("args", (fun t -> t.args), typedecl_constructor_argument);
      F ("res", (fun t -> t.res), Option typeexpr_t);
    ]

(** {3 Value} *)

and value_t =
  let open Lang.Value in
  let value_value_t =
    Variant
      (function
      | Abstract -> C0 "Abstract" | External x -> C ("External", x, List string))
  in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("type_", (fun t -> t.type_), typeexpr_t);
      F ("value", (fun t -> t.value), value_value_t);
    ]

(** {3 Class} *)

and class_decl =
  let open Lang.Class in
  Variant
    (function
    | ClassType x -> C ("ClassType", x, classtype_expr)
    | Arrow (x1, x2, x3) ->
        C
          ( "Arrow",
            (x1, x2, x3),
            Triple (Option typeexpr_label, typeexpr_t, class_decl) ))

and class_t =
  let open Lang.Class in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("virtual_", (fun t -> t.virtual_), bool);
      F ("params", (fun t -> t.params), List typedecl_param);
      F ("type_", (fun t -> t.type_), class_decl);
      F ("expansion", (fun t -> t.expansion), Option classsignature_t);
    ]

(** {3 ClassType} *)

and classtype_expr =
  let open Lang.ClassType in
  Variant
    (function
    | Constr (x1, x2) ->
        C ("Constr", ((x1 :> Paths.Path.t), x2), Pair (path, List typeexpr_t))
    | Signature x -> C ("Signature", x, classsignature_t))

and classtype_t =
  let open Lang.ClassType in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("locs", (fun t -> t.locs), Option identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("virtual_", (fun t -> t.virtual_), bool);
      F ("params", (fun t -> t.params), List typedecl_param);
      F ("expr", (fun t -> t.expr), classtype_expr);
      F ("expansion", (fun t -> t.expansion), Option classsignature_t);
    ]

(** {3 ClassSignature} *)

and classsignature_item =
  let open Lang.ClassSignature in
  Variant
    (function
    | Method x -> C ("Method", x, method_t)
    | InstanceVariable x -> C ("InstanceVariable", x, instancevariable_t)
    | Constraint cst ->
        C
          ( "Constraint",
            (cst.left, cst.right, cst.doc),
            Triple (typeexpr_t, typeexpr_t, docs) )
    | Inherit ih -> C ("Inherit", (ih.expr, ih.doc), Pair (classtype_expr, docs))
    | Comment x -> C ("Comment", x, docs_or_stop))

and classsignature_t =
  let open Lang.ClassSignature in
  Record
    [
      F ("self", (fun t -> t.self), Option typeexpr_t);
      F ("items", (fun t -> t.items), List classsignature_item);
    ]

(** {3 Method} *)

and method_t =
  let open Lang.Method in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("private_", (fun t -> t.private_), bool);
      F ("virtual_", (fun t -> t.virtual_), bool);
      F ("type_", (fun t -> t.type_), typeexpr_t);
    ]

(** {3 InstanceVariable} *)

and instancevariable_t =
  let open Lang.InstanceVariable in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("doc", (fun t -> t.doc), docs);
      F ("mutable_", (fun t -> t.mutable_), bool);
      F ("virtual_", (fun t -> t.virtual_), bool);
      F ("type_", (fun t -> t.type_), typeexpr_t);
    ]

(** {3 TypeExpr} *)

and typeexpr_polymorphic_variant_kind =
  let open Lang.TypeExpr.Polymorphic_variant in
  Variant
    (function
    | Fixed -> C0 "Fixed"
    | Closed x -> C ("Closed", x, List string)
    | Open -> C0 "Open")

and typeexpr_polymorphic_variant_constructor =
  let open Lang.TypeExpr.Polymorphic_variant.Constructor in
  Record
    [
      F ("name", (fun t -> t.name), string);
      F ("constant", (fun t -> t.constant), bool);
      F ("arguments", (fun t -> t.arguments), List typeexpr_t);
      F ("doc", (fun t -> t.doc), docs);
    ]

and typeexpr_polymorphic_variant_element =
  let open Lang.TypeExpr.Polymorphic_variant in
  Variant
    (function
    | Type x -> C ("Type", x, typeexpr_t)
    | Constructor x ->
        C ("Constructor", x, typeexpr_polymorphic_variant_constructor))

and typeexpr_polymorphic_variant =
  let open Lang.TypeExpr.Polymorphic_variant in
  Record
    [
      F ("kind", (fun t -> t.kind), typeexpr_polymorphic_variant_kind);
      F
        ( "elements",
          (fun t -> t.elements),
          List typeexpr_polymorphic_variant_element );
    ]

and typeexpr_object_method_ =
  let open Lang.TypeExpr.Object in
  Record
    [
      F ("name", (fun t -> t.name), string);
      F ("type_", (fun t -> t.type_), typeexpr_t);
    ]

and typeexpr_object_field =
  let open Lang.TypeExpr.Object in
  Variant
    (function
    | Method x -> C ("Method", x, typeexpr_object_method_)
    | Inherit x -> C ("Inherit", x, typeexpr_t))

and typeexpr_object =
  let open Lang.TypeExpr.Object in
  Record
    [
      F ("fields", (fun t -> t.fields), List typeexpr_object_field);
      F ("open_", (fun t -> t.open_), bool);
    ]

and typeexpr_package_substitution = Pair (fragment, typeexpr_t)

and typeexpr_package =
  let open Lang.TypeExpr.Package in
  Record
    [
      F ("path", (fun t -> (t.path :> Paths.Path.t)), path);
      F
        ( "substitutions",
          (fun t ->
            (t.substitutions :> (Paths.Fragment.t * Lang.TypeExpr.t) list)),
          List typeexpr_package_substitution );
    ]

and typeexpr_label =
  let open Lang.TypeExpr in
  Variant
    (function
    | Label x -> C ("Label", x, string) | Optional x -> C ("Optional", x, string))

and typeexpr_t =
  let open Lang.TypeExpr in
  Variant
    (function
    | Var x -> C ("Var", x, string)
    | Any -> C0 "Any"
    | Alias (x1, x2) -> C ("Alias", (x1, x2), Pair (typeexpr_t, string))
    | Arrow (x1, x2, x3) ->
        C
          ( "Arrow",
            (x1, x2, x3),
            Triple (Option typeexpr_label, typeexpr_t, typeexpr_t) )
    | Tuple x -> C ("Tuple", x, List typeexpr_t)
    | Constr (x1, x2) ->
        C ("Constr", ((x1 :> Paths.Path.t), x2), Pair (path, List typeexpr_t))
    | Polymorphic_variant x ->
        C ("Polymorphic_variant", x, typeexpr_polymorphic_variant)
    | Object x -> C ("Object", x, typeexpr_object)
    | Class (x1, x2) ->
        C ("Class", ((x1 :> Paths.Path.t), x2), Pair (path, List typeexpr_t))
    | Poly (x1, x2) -> C ("Poly", (x1, x2), Pair (List string, typeexpr_t))
    | Package x -> C ("Package", x, typeexpr_package))

(** {3 Compilation_unit} *)

and compilation_unit_import =
  let open Lang.Compilation_unit.Import in
  Variant
    (function
    | Unresolved (x1, x2) ->
        C ("Unresolved", (x1, x2), Pair (string, Option Digest.t))
    | Resolved (x1, x2) -> C ("Resolved", (x1, x2), Pair (root, modulename)))

and compilation_unit_source =
  let open Lang.Compilation_unit.Source in
  Record
    [
      F ("file", (fun t -> t.file), string);
      F ("build_dir", (fun t -> t.build_dir), string);
      F ("digest", (fun t -> t.digest), Digest.t);
    ]

and compilation_unit_packed_item =
  let open Lang.Compilation_unit.Packed in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("path", (fun t -> (t.path :> Paths.Path.t)), path);
    ]

and compilation_unit_packed = List compilation_unit_packed_item

and compilation_unit_content =
  let open Lang.Compilation_unit in
  Variant
    (function
    | Module x -> C ("Module", x, signature_t)
    | Pack x -> C ("Pack", x, compilation_unit_packed))

and compilation_unit_t =
  let open Lang.Compilation_unit in
  Record
    [
      F ("id", (fun t -> t.id), identifier);
      F ("root", (fun t -> t.root), root);
      F ("digest", (fun t -> t.digest), Digest.t);
      F ("imports", (fun t -> t.imports), List compilation_unit_import);
      F ("source", (fun t -> t.source), Option compilation_unit_source);
      F ("interface", (fun t -> t.interface), bool);
      F ("hidden", (fun t -> t.hidden), bool);
      F ("content", (fun t -> t.content), compilation_unit_content);
      F ("expansion", (fun t -> t.expansion), Option signature_t);
      F
        ( "canonical",
          (fun t -> (t.canonical :> Paths.Path.t option)),
          Option path );
      F ("sources", (fun t -> t.source_info), Option source_info);
    ]

(** {3 Page} *)

and page_t =
  let open Lang.Page in
  Record
    [
      F ("name", (fun t -> t.name), identifier);
      F ("root", (fun t -> t.root), root);
      F ("content", (fun t -> t.content), docs);
      F ("digest", (fun t -> t.digest), Digest.t);
    ]

and source_tree_page_t =
  let open Lang.SourceTree in
  Record
    [
      F ("name", (fun t -> t.name), identifier);
      F ("root", (fun t -> t.root), root);
      F ("digest", (fun t -> t.digest), Digest.t);
      F ("source_children", (fun t -> t.source_children), List identifier);
    ]
