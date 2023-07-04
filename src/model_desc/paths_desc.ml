open Type_desc
open Odoc_model

module Root = struct
  let t : Root.t t = To_string (fun _ -> "<root>")
end

module Names = struct
  include Names

  let modulename = To_string ModuleName.to_string

  let moduletypename = To_string ModuleTypeName.to_string

  let typename = To_string TypeName.to_string

  let classname = To_string ClassName.to_string

  let classtypename = To_string ClassTypeName.to_string

  let constructorname = To_string ConstructorName.to_string

  let fieldname = To_string FieldName.to_string

  let exceptionname = To_string ExceptionName.to_string

  let extensionname = To_string ExtensionName.to_string

  let valuename = To_string ValueName.to_string

  let methodname = To_string MethodName.to_string

  let instancevariablename = To_string InstanceVariableName.to_string

  let labelname = To_string LabelName.to_string

  let pagename = To_string PageName.to_string

  let parametername = To_string ModuleName.to_string

  let defname = To_string DefName.to_string

  let localname = To_string LocalName.to_string
end

module General_paths = struct
  (** Simplified paths types that can be coerced to. *)

  type p = Paths.Path.t

  type rp = Paths.Path.Resolved.t

  type f = Paths.Fragment.t

  type rf = Paths.Fragment.Resolved.t

  type r = Paths.Reference.t

  type rr = Paths.Reference.Resolved.t

  type id_t = Paths.Identifier.t

  type tag = Paths.Reference.tag_any

  let rec identifier : Paths.Identifier.t t =
    Variant
      (fun x ->
        match x.iv with
        | `Page (parent, name) ->
            C
              ( "`Page",
                ((parent :> id_t option), name),
                Pair (Option identifier, Names.pagename) )
        | `LeafPage (parent, name) ->
            C
              ( "`LeafPage",
                ((parent :> id_t option), name),
                Pair (Option identifier, Names.pagename) )
        | `AssetFile (parent, name) ->
            C ("`AssetFile", ((parent :> id_t), name), Pair (identifier, string))
        | `Root (parent, name) ->
            C
              ( "`Root",
                ((parent :> id_t option), name),
                Pair (Option identifier, Names.modulename) )
        | `Module (parent, name) ->
            C
              ( "`Module",
                ((parent :> id_t), name),
                Pair (identifier, Names.modulename) )
        | `Parameter (parent, name) ->
            C
              ( "`Parameter",
                ((parent :> id_t), name),
                Pair (identifier, Names.parametername) )
        | `Result r -> C ("`Result", (r :> id_t), identifier)
        | `ModuleType (parent, name) ->
            C
              ( "`ModuleType",
                ((parent :> id_t), name),
                Pair (identifier, Names.moduletypename) )
        | `Class (parent, name) ->
            C
              ( "`Class",
                ((parent :> id_t), name),
                Pair (identifier, Names.classname) )
        | `ClassType (parent, name) ->
            C
              ( "`ClassType",
                ((parent :> id_t), name),
                Pair (identifier, Names.classtypename) )
        | `Type (parent, name) ->
            C
              ( "`Type",
                ((parent :> id_t), name),
                Pair (identifier, Names.typename) )
        | `CoreType name -> C ("`CoreType", name, Names.typename)
        | `Constructor (parent, name) ->
            C
              ( "`Constructor",
                ((parent :> id_t), name),
                Pair (identifier, Names.constructorname) )
        | `Field (parent, name) ->
            C
              ( "`Field",
                ((parent :> id_t), name),
                Pair (identifier, Names.fieldname) )
        | `Extension (parent, name) ->
            C
              ( "`Extension",
                ((parent :> id_t), name),
                Pair (identifier, Names.extensionname) )
        | `ExtensionDecl (parent, name, name') ->
            C
              ( "`ExtensionDecl",
                ((parent :> id_t), name, name'),
                Triple (identifier, Names.extensionname, Names.extensionname) )
        | `Exception (parent, name) ->
            C
              ( "`Exception",
                ((parent :> id_t), name),
                Pair (identifier, Names.exceptionname) )
        | `CoreException name -> C ("`CoreException", name, Names.exceptionname)
        | `Value (parent, name) ->
            C
              ( "`Value",
                ((parent :> id_t), name),
                Pair (identifier, Names.valuename) )
        | `Method (parent, name) ->
            C
              ( "`Method",
                ((parent :> id_t), name),
                Pair (identifier, Names.methodname) )
        | `InstanceVariable (parent, name) ->
            C
              ( "`InstanceVariable",
                ((parent :> id_t), name),
                Pair (identifier, Names.instancevariablename) )
        | `Label (parent, name) ->
            C
              ( "`Label",
                ((parent :> id_t), name),
                Pair (identifier, Names.labelname) )
        | `SourceDir (parent, name) ->
            C ("`SourceDir", ((parent :> id_t), name), Pair (identifier, string))
        | `SourcePage (parent, name) ->
            C
              ( "`SourcePage",
                ((parent :> id_t), name),
                Pair (identifier, string) )
        | `SourceLocation (parent, name) ->
            C
              ( "`SourceLocation",
                ((parent :> id_t), name),
                Pair (identifier, Names.defname) )
        | `SourceLocationInternal (parent, name) ->
            C
              ( "`SourceLocationInternal",
                ((parent :> id_t), name),
                Pair (identifier, Names.localname) )
        | `SourceLocationMod parent ->
            C ("`SourceLocationMod", (parent :> id_t), identifier))

  let reference_tag : tag t =
    Variant
      (function
      | `TClass -> C0 "`TClass"
      | `TClassType -> C0 "`TClassType"
      | `TConstructor -> C0 "`TConstructor"
      | `TException -> C0 "`TException"
      | `TExtension -> C0 "`TExtension"
      | `TExtensionDecl -> C0 "`TExtensionDecl"
      | `TField -> C0 "`TField"
      | `TInstanceVariable -> C0 "`TInstanceVariable"
      | `TLabel -> C0 "`TLabel"
      | `TMethod -> C0 "`TMethod"
      | `TModule -> C0 "`TModule"
      | `TModuleType -> C0 "`TModuleType"
      | `TPage -> C0 "`TPage"
      | `TType -> C0 "`TType"
      | `TUnknown -> C0 "`TUnknown"
      | `TValue -> C0 "`TValue"
      | `TChildPage -> C0 "`TChildPage"
      | `TChildModule -> C0 "`TChildModule")

  let rec path : p t =
    Variant
      (function
      | `Resolved x -> C ("`Resolved", x, resolved_path)
      | `Identifier (x1, x2) ->
          C ("`Identifier", ((x1 :> id_t), x2), Pair (identifier, bool))
      | `Root x -> C ("`Root", x, string)
      | `Forward x -> C ("`Forward", x, string)
      | `Dot (x1, x2) -> C ("`Dot", ((x1 :> p), x2), Pair (path, string))
      | `Apply (x1, x2) ->
          C ("`Apply", ((x1 :> p), (x2 :> p)), Pair (path, path)))

  and resolved_path : rp t =
    Variant
      (function
      | `Identifier x -> C ("`Identifier", x, identifier)
      | `Subst (x1, x2) ->
          C
            ( "`Subst",
              ((x1 :> rp), (x2 :> rp)),
              Pair (resolved_path, resolved_path) )
      | `Hidden x -> C ("`Hidden", (x :> rp), resolved_path)
      | `Module (x1, x2) ->
          C ("`Module", ((x1 :> rp), x2), Pair (resolved_path, Names.modulename))
      | `Canonical (x1, x2) ->
          C ("`Canonical", ((x1 :> rp), (x2 :> p)), Pair (resolved_path, path))
      | `Apply (x1, x2) ->
          C
            ( "`Apply",
              ((x1 :> rp), (x2 :> rp)),
              Pair (resolved_path, resolved_path) )
      | `Alias (dest, src) ->
          C ("`Alias", ((dest :> rp), (src :> p)), Pair (resolved_path, path))
      | `AliasModuleType (x1, x2) ->
          C
            ( "`AliasModuleType",
              ((x1 :> rp), (x2 :> rp)),
              Pair (resolved_path, resolved_path) )
      | `OpaqueModule x -> C ("`OpaqueModule", (x :> rp), resolved_path)
      | `ModuleType (x1, x2) ->
          C
            ( "`ModuleType",
              ((x1 :> rp), x2),
              Pair (resolved_path, Names.moduletypename) )
      | `SubstT (x1, x2) ->
          C
            ( "`SubstT",
              ((x1 :> rp), (x2 :> rp)),
              Pair (resolved_path, resolved_path) )
      | `CanonicalModuleType (x1, x2) ->
          C
            ( "`CanonicalModuleType",
              ((x1 :> rp), (x2 :> p)),
              Pair (resolved_path, path) )
      | `CanonicalType (x1, x2) ->
          C
            ( "`CanonicalType",
              ((x1 :> rp), (x2 :> p)),
              Pair (resolved_path, path) )
      | `CanonicalDataType (x1, x2) ->
          C
            ( "`CanonicalDataType",
              ((x1 :> rp), (x2 :> p)),
              Pair (resolved_path, path) )
      | `OpaqueModuleType x -> C ("`OpaqueModuleType", (x :> rp), resolved_path)
      | `Type (x1, x2) ->
          C ("`Type", ((x1 :> rp), x2), Pair (resolved_path, Names.typename))
      | `Value (x1, x2) ->
          C ("`Value", ((x1 :> rp), x2), Pair (resolved_path, Names.valuename))
      | `Constructor (x1, x2) ->
          C
            ( "`Constructor",
              ((x1 :> rp), x2),
              Pair (resolved_path, Names.constructorname) )
      | `Class (x1, x2) ->
          C ("`Class", ((x1 :> rp), x2), Pair (resolved_path, Names.classname))
      | `ClassType (x1, x2) ->
          C
            ( "`ClassType",
              ((x1 :> rp), x2),
              Pair (resolved_path, Names.classtypename) ))

  and reference : r t =
    Variant
      (function
      | `Resolved x -> C ("`Resolved", x, resolved_reference)
      | `Root (x1, x2) -> C ("`Root", (x1, x2), Pair (string, reference_tag))
      | `Dot (x1, x2) -> C ("`Dot", ((x1 :> r), x2), Pair (reference, string))
      | `Module (x1, x2) ->
          C ("`Module", ((x1 :> r), x2), Pair (reference, Names.modulename))
      | `ModuleType (x1, x2) ->
          C
            ( "`ModuleType",
              ((x1 :> r), x2),
              Pair (reference, Names.moduletypename) )
      | `Type (x1, x2) ->
          C ("`Type", ((x1 :> r), x2), Pair (reference, Names.typename))
      | `Constructor (x1, x2) ->
          C
            ( "`Constructor",
              ((x1 :> r), x2),
              Pair (reference, Names.constructorname) )
      | `Field (x1, x2) ->
          C ("`Field", ((x1 :> r), x2), Pair (reference, Names.fieldname))
      | `Extension (x1, x2) ->
          C
            ( "`Extension",
              ((x1 :> r), x2),
              Pair (reference, Names.extensionname) )
      | `ExtensionDecl (x1, x2) ->
          C
            ( "`ExtensionDecl",
              ((x1 :> r), x2),
              Pair (reference, Names.extensionname) )
      | `Exception (x1, x2) ->
          C
            ( "`Exception",
              ((x1 :> r), x2),
              Pair (reference, Names.exceptionname) )
      | `Value (x1, x2) ->
          C ("`Value", ((x1 :> r), x2), Pair (reference, Names.valuename))
      | `Class (x1, x2) ->
          C ("`Class", ((x1 :> r), x2), Pair (reference, Names.classname))
      | `ClassType (x1, x2) ->
          C
            ( "`ClassType",
              ((x1 :> r), x2),
              Pair (reference, Names.classtypename) )
      | `Method (x1, x2) ->
          C ("`Method", ((x1 :> r), x2), Pair (reference, Names.methodname))
      | `InstanceVariable (x1, x2) ->
          C
            ( "`InstanceVariable",
              ((x1 :> r), x2),
              Pair (reference, Names.instancevariablename) )
      | `Label (x1, x2) ->
          C ("`Label", ((x1 :> r), x2), Pair (reference, Names.labelname)))

  and resolved_reference : rr t =
    Variant
      (function
      | `Class (x1, x2) ->
          C
            ( "`Class",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.classname) )
      | `ClassType (x1, x2) ->
          C
            ( "`ClassType",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.classtypename) )
      | `Constructor (x1, x2) ->
          C
            ( "`Constructor",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.constructorname) )
      | `Exception (x1, x2) ->
          C
            ( "`Exception",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.exceptionname) )
      | `Extension (x1, x2) ->
          C
            ( "`Extension",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.extensionname) )
      | `ExtensionDecl (x1, x2, x3) ->
          C
            ( "`ExtensionDecl",
              ((x1 :> rr), x2, x3),
              Triple
                (resolved_reference, Names.extensionname, Names.extensionname)
            )
      | `Field (x1, x2) ->
          C
            ( "`Field",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.fieldname) )
      | `Hidden x -> C ("`Hidden", (x :> rr), resolved_reference)
      | `Identifier x -> C ("`Identifier", (x :> id_t), identifier)
      | `InstanceVariable (x1, x2) ->
          C
            ( "`InstanceVariable",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.instancevariablename) )
      | `Label (x1, x2) ->
          C
            ( "`Label",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.labelname) )
      | `Method (x1, x2) ->
          C
            ( "`Method",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.methodname) )
      | `Module (x1, x2) ->
          C
            ( "`Module",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.modulename) )
      | `ModuleType (x1, x2) ->
          C
            ( "`ModuleType",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.moduletypename) )
      | `Alias (x1, x2) ->
          C
            ( "`Alias",
              ((x1 :> rp), (x2 :> rr)),
              Pair (resolved_path, resolved_reference) )
      | `AliasModuleType (x1, x2) ->
          C
            ( "`AliasModuleType",
              ((x1 :> rp), (x2 :> rr)),
              Pair (resolved_path, resolved_reference) )
      | `Type (x1, x2) ->
          C
            ( "`Type",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.typename) )
      | `Value (x1, x2) ->
          C
            ( "`Value",
              ((x1 :> rr), x2),
              Pair (resolved_reference, Names.valuename) ))

  let resolved_fragment_root : Paths.Fragment.Resolved.root t =
    Variant
      (function
      | `ModuleType x -> C ("`ModuleType", (x :> rp), resolved_path)
      | `Module x -> C ("`Module", (x :> rp), resolved_path))

  let rec resolved_fragment : rf t =
    Variant
      (function
      | `Root x -> C ("`Root", x, resolved_fragment_root)
      | `Subst (x1, x2) ->
          C
            ( "`Subst",
              ((x1 :> rp), (x2 :> rf)),
              Pair (resolved_path, resolved_fragment) )
      | `Alias (x1, x2) ->
          C
            ( "`Alias",
              ((x1 :> rp), (x2 :> rf)),
              Pair (resolved_path, resolved_fragment) )
      | `Module (x1, x2) ->
          C
            ( "`Module",
              ((x1 :> rf), x2),
              Pair (resolved_fragment, Names.modulename) )
      | `Module_type (x1, x2) ->
          C
            ( "`Module_type",
              ((x1 :> rf), x2),
              Pair (resolved_fragment, Names.moduletypename) )
      | `Type (x1, x2) ->
          C ("`Type", ((x1 :> rf), x2), Pair (resolved_fragment, Names.typename))
      | `Class (x1, x2) ->
          C
            ( "`Class",
              ((x1 :> rf), x2),
              Pair (resolved_fragment, Names.classname) )
      | `ClassType (x1, x2) ->
          C
            ( "`ClassType",
              ((x1 :> rf), x2),
              Pair (resolved_fragment, Names.classtypename) )
      | `OpaqueModule x -> C ("`OpaqueModule", (x :> rf), resolved_fragment))

  let rec fragment : f t =
    Variant
      (function
      | `Resolved x -> C ("`Resolved", (x :> rf), resolved_fragment)
      | `Dot (x1, x2) -> C ("`Dot", ((x1 :> f), x2), Pair (fragment, string))
      | `Root -> C0 "`Root")
end

let root = Root.t

let modulename = Names.modulename

(* Indirection seems to be required to make the type open. *)
let identifier : [< Paths.Identifier.t_pv ] Paths.Identifier.id Type_desc.t =
  Indirect ((fun n -> (n :> Paths.Identifier.t)), General_paths.identifier)

let resolved_path : [< Paths.Path.Resolved.t ] Type_desc.t =
  Indirect ((fun n -> (n :> General_paths.rp)), General_paths.resolved_path)

let path : [< Paths.Path.t ] Type_desc.t =
  Indirect ((fun n -> (n :> General_paths.p)), General_paths.path)

let resolved_fragment =
  Indirect ((fun n -> (n :> General_paths.rf)), General_paths.resolved_fragment)

let fragment =
  Indirect ((fun n -> (n :> General_paths.f)), General_paths.fragment)

let reference =
  Indirect ((fun n -> (n :> General_paths.r)), General_paths.reference)
