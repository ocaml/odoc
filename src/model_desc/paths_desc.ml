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
end

let rec identifier =
  Indirect
    ( Paths.Identifier.split,
      Pair
        ( Variant
            (function
            | `Root x -> C ("`Root", x, Option Root.t)
            | `Parent x -> C ("`Parent", (x :> Paths.Identifier.t), identifier)),
          string ) )

module General_paths = struct
  (** Simplified paths types that can be coerced to. *)

  type reference_tag =
    [ `TClass
    | `TClassType
    | `TConstructor
    | `TException
    | `TExtension
    | `TField
    | `TInstanceVariable
    | `TLabel
    | `TMethod
    | `TModule
    | `TModuleType
    | `TPage
    | `TType
    | `TUnknown
    | `TValue ]

  type path =
    [ `Resolved of resolved_path
    | `Identifier of Paths.Identifier.t * bool
    | `Root of string
    | `Forward of string
    | `Dot of path * string
    | `Apply of path * path ]

  and resolved_path =
    [ `Identifier of Paths.Identifier.t
    | `Subst of resolved_path * resolved_path
    | `SubstAlias of resolved_path * resolved_path
    | `Hidden of resolved_path
    | `Module of resolved_path * Names.ModuleName.t
    | `Canonical of resolved_path * path
    | `Apply of resolved_path * resolved_path
    | `Alias of resolved_path * resolved_path
    | `OpaqueModule of resolved_path
    | `ModuleType of resolved_path * Names.ModuleTypeName.t
    | `SubstT of resolved_path * resolved_path
    | `OpaqueModuleType of resolved_path
    | `Type of resolved_path * Names.TypeName.t
    | `Class of resolved_path * Names.ClassName.t
    | `ClassType of resolved_path * Names.ClassTypeName.t
    | `Class of resolved_path * Names.ClassName.t
    | `ClassType of resolved_path * Names.ClassTypeName.t ]

  and reference =
    [ `Resolved of resolved_reference
    | `Root of string * reference_tag
    | `Dot of reference * string
    | `Module of reference * Names.ModuleName.t
    | `ModuleType of reference * Names.ModuleTypeName.t
    | `Type of reference * Names.TypeName.t
    | `Constructor of reference * Names.ConstructorName.t
    | `Field of reference * Names.FieldName.t
    | `Extension of reference * Names.ExtensionName.t
    | `Exception of reference * Names.ExceptionName.t
    | `Value of reference * Names.ValueName.t
    | `Class of reference * Names.ClassName.t
    | `ClassType of reference * Names.ClassTypeName.t
    | `Method of reference * Names.MethodName.t
    | `InstanceVariable of reference * Names.InstanceVariableName.t
    | `Label of reference * Names.LabelName.t ]

  and resolved_reference =
    [ `Canonical of resolved_reference * reference
    | `Class of resolved_reference * Names.ClassName.t
    | `ClassType of resolved_reference * Names.ClassTypeName.t
    | `Constructor of resolved_reference * Names.ConstructorName.t
    | `Exception of resolved_reference * Names.ExceptionName.t
    | `Extension of resolved_reference * Names.ExtensionName.t
    | `Field of resolved_reference * Names.FieldName.t
    | `Hidden of resolved_reference
    | `Identifier of Paths.Identifier.t
    | `InstanceVariable of resolved_reference * Names.InstanceVariableName.t
    | `Label of resolved_reference * Names.LabelName.t
    | `Method of resolved_reference * Names.MethodName.t
    | `Module of resolved_reference * Names.ModuleName.t
    | `ModuleType of resolved_reference * Names.ModuleTypeName.t
    | `SubstAlias of resolved_path * resolved_reference
    | `Type of resolved_reference * Names.TypeName.t
    | `Value of resolved_reference * Names.ValueName.t ]

  type resolved_fragment_root =
    [ `ModuleType of resolved_path | `Module of resolved_path ]

  type resolved_fragment =
    [ `Root of resolved_fragment_root
    | `Subst of resolved_path * resolved_fragment
    | `SubstAlias of resolved_path * resolved_fragment
    | `Module of resolved_fragment * Names.ModuleName.t
    | `Type of resolved_fragment * Names.TypeName.t
    | `Class of resolved_fragment * Names.ClassName.t
    | `ClassType of resolved_fragment * Names.ClassTypeName.t
    | `OpaqueModule of resolved_fragment ]

  type fragment =
    [ `Resolved of resolved_fragment | `Dot of fragment * string | `Root ]

  let reference_tag : reference_tag t =
    Variant
      (function
      | `TClass -> C0 "`TClass"
      | `TClassType -> C0 "`TClassType"
      | `TConstructor -> C0 "`TConstructor"
      | `TException -> C0 "`TException"
      | `TExtension -> C0 "`TExtension"
      | `TField -> C0 "`TField"
      | `TInstanceVariable -> C0 "`TInstanceVariable"
      | `TLabel -> C0 "`TLabel"
      | `TMethod -> C0 "`TMethod"
      | `TModule -> C0 "`TModule"
      | `TModuleType -> C0 "`TModuleType"
      | `TPage -> C0 "`TPage"
      | `TType -> C0 "`TType"
      | `TUnknown -> C0 "`TUnknown"
      | `TValue -> C0 "`TValue")

  let rec path : path t =
    Variant
      (function
      | `Resolved x -> C ("`Resolved", x, resolved_path)
      | `Identifier (x1, x2) ->
          C ("`Identifier", (x1, x2), Pair (identifier, bool))
      | `Root x -> C ("`Root", x, string)
      | `Forward x -> C ("`Forward", x, string)
      | `Dot (x1, x2) -> C ("`Dot", (x1, x2), Pair (path, string))
      | `Apply (x1, x2) -> C ("`Apply", (x1, x2), Pair (path, path)))

  and resolved_path : resolved_path t =
    Variant
      (function
      | `Identifier x -> C ("`Identifier", x, identifier)
      | `Subst (x1, x2) ->
          C ("`Subst", (x1, x2), Pair (resolved_path, resolved_path))
      | `SubstAlias (x1, x2) ->
          C ("`SubstAlias", (x1, x2), Pair (resolved_path, resolved_path))
      | `Hidden x -> C ("`Hidden", x, resolved_path)
      | `Module (x1, x2) ->
          C ("`Module", (x1, x2), Pair (resolved_path, Names.modulename))
      | `Canonical (x1, x2) ->
          C ("`Canonical", (x1, x2), Pair (resolved_path, path))
      | `Apply (x1, x2) ->
          C ("`Apply", (x1, x2), Pair (resolved_path, resolved_path))
      | `Alias (x1, x2) ->
          C ("`Alias", (x1, x2), Pair (resolved_path, resolved_path))
      | `OpaqueModule x -> C ("`OpaqueModule", x, resolved_path)
      | `ModuleType (x1, x2) ->
          C ("`ModuleType", (x1, x2), Pair (resolved_path, Names.moduletypename))
      | `SubstT (x1, x2) ->
          C ("`SubstT", (x1, x2), Pair (resolved_path, resolved_path))
      | `OpaqueModuleType x -> C ("`OpaqueModuleType", x, resolved_path)
      | `Type (x1, x2) ->
          C ("`Type", (x1, x2), Pair (resolved_path, Names.typename))
      | `Class (x1, x2) ->
          C ("`Class", (x1, x2), Pair (resolved_path, Names.classname))
      | `ClassType (x1, x2) ->
          C ("`ClassType", (x1, x2), Pair (resolved_path, Names.classtypename)))

  and reference : reference t =
    Variant
      (function
      | `Resolved x -> C ("`Resolved", x, resolved_reference)
      | `Root (x1, x2) -> C ("`Root", (x1, x2), Pair (string, reference_tag))
      | `Dot (x1, x2) -> C ("`Dot", (x1, x2), Pair (reference, string))
      | `Module (x1, x2) ->
          C ("`Module", (x1, x2), Pair (reference, Names.modulename))
      | `ModuleType (x1, x2) ->
          C ("`ModuleType", (x1, x2), Pair (reference, Names.moduletypename))
      | `Type (x1, x2) -> C ("`Type", (x1, x2), Pair (reference, Names.typename))
      | `Constructor (x1, x2) ->
          C ("`Constructor", (x1, x2), Pair (reference, Names.constructorname))
      | `Field (x1, x2) ->
          C ("`Field", (x1, x2), Pair (reference, Names.fieldname))
      | `Extension (x1, x2) ->
          C ("`Extension", (x1, x2), Pair (reference, Names.extensionname))
      | `Exception (x1, x2) ->
          C ("`Exception", (x1, x2), Pair (reference, Names.exceptionname))
      | `Value (x1, x2) ->
          C ("`Value", (x1, x2), Pair (reference, Names.valuename))
      | `Class (x1, x2) ->
          C ("`Class", (x1, x2), Pair (reference, Names.classname))
      | `ClassType (x1, x2) ->
          C ("`ClassType", (x1, x2), Pair (reference, Names.classtypename))
      | `Method (x1, x2) ->
          C ("`Method", (x1, x2), Pair (reference, Names.methodname))
      | `InstanceVariable (x1, x2) ->
          C
            ( "`InstanceVariable",
              (x1, x2),
              Pair (reference, Names.instancevariablename) )
      | `Label (x1, x2) ->
          C ("`Label", (x1, x2), Pair (reference, Names.labelname)))

  and resolved_reference : resolved_reference t =
    Variant
      (function
      | `Canonical (x1, x2) ->
          C ("`Canonical", (x1, x2), Pair (resolved_reference, reference))
      | `Class (x1, x2) ->
          C ("`Class", (x1, x2), Pair (resolved_reference, Names.classname))
      | `ClassType (x1, x2) ->
          C
            ( "`ClassType",
              (x1, x2),
              Pair (resolved_reference, Names.classtypename) )
      | `Constructor (x1, x2) ->
          C
            ( "`Constructor",
              (x1, x2),
              Pair (resolved_reference, Names.constructorname) )
      | `Exception (x1, x2) ->
          C
            ( "`Exception",
              (x1, x2),
              Pair (resolved_reference, Names.exceptionname) )
      | `Extension (x1, x2) ->
          C
            ( "`Extension",
              (x1, x2),
              Pair (resolved_reference, Names.extensionname) )
      | `Field (x1, x2) ->
          C ("`Field", (x1, x2), Pair (resolved_reference, Names.fieldname))
      | `Hidden x -> C ("`Hidden", x, resolved_reference)
      | `Identifier x -> C ("`Identifier", x, identifier)
      | `InstanceVariable (x1, x2) ->
          C
            ( "`InstanceVariable",
              (x1, x2),
              Pair (resolved_reference, Names.instancevariablename) )
      | `Label (x1, x2) ->
          C ("`Label", (x1, x2), Pair (resolved_reference, Names.labelname))
      | `Method (x1, x2) ->
          C ("`Method", (x1, x2), Pair (resolved_reference, Names.methodname))
      | `Module (x1, x2) ->
          C ("`Module", (x1, x2), Pair (resolved_reference, Names.modulename))
      | `ModuleType (x1, x2) ->
          C
            ( "`ModuleType",
              (x1, x2),
              Pair (resolved_reference, Names.moduletypename) )
      | `SubstAlias (x1, x2) ->
          C ("`SubstAlias", (x1, x2), Pair (resolved_path, resolved_reference))
      | `Type (x1, x2) ->
          C ("`Type", (x1, x2), Pair (resolved_reference, Names.typename))
      | `Value (x1, x2) ->
          C ("`Value", (x1, x2), Pair (resolved_reference, Names.valuename)))

  let resolved_fragment_root : resolved_fragment_root t =
    Variant
      (function
      | `ModuleType x -> C ("`ModuleType", x, resolved_path)
      | `Module x -> C ("`Module", x, resolved_path))

  let rec resolved_fragment : resolved_fragment t =
    Variant
      (function
      | `Root x -> C ("`Root", x, resolved_fragment_root)
      | `Subst (x1, x2) ->
          C ("`Subst", (x1, x2), Pair (resolved_path, resolved_fragment))
      | `SubstAlias (x1, x2) ->
          C ("`SubstAlias", (x1, x2), Pair (resolved_path, resolved_fragment))
      | `Module (x1, x2) ->
          C ("`Module", (x1, x2), Pair (resolved_fragment, Names.modulename))
      | `Type (x1, x2) ->
          C ("`Type", (x1, x2), Pair (resolved_fragment, Names.typename))
      | `Class (x1, x2) ->
          C ("`Class", (x1, x2), Pair (resolved_fragment, Names.classname))
      | `ClassType (x1, x2) ->
          C
            ( "`ClassType",
              (x1, x2),
              Pair (resolved_fragment, Names.classtypename) )
      | `OpaqueModule x -> C ("`OpaqueModule", x, resolved_fragment))

  let rec fragment : fragment t =
    Variant
      (function
      | `Resolved x -> C ("`Resolved", x, resolved_fragment)
      | `Dot (x1, x2) -> C ("`Dot", (x1, x2), Pair (fragment, string))
      | `Root -> C0 "`Root")
end

(* Indirection seems to be required to make the type open. *)
let identifier = Indirect ((fun n -> (n :> Paths.Identifier.t)), identifier)

let resolved_path =
  Indirect
    ( (fun n -> ((n :> Paths.Path.Resolved.t) :> General_paths.resolved_path)),
      General_paths.resolved_path )

let path =
  Indirect
    ((fun n -> ((n :> Paths.Path.t) :> General_paths.path)), General_paths.path)

let resolved_fragment =
  Indirect
    ( (fun n ->
        ((n :> Paths.Fragment.Resolved.t) :> General_paths.resolved_fragment)),
      General_paths.resolved_fragment )

let fragment =
  Indirect
    ( (fun n -> ((n :> Paths.Fragment.t) :> General_paths.fragment)),
      General_paths.fragment )

let reference =
  Indirect
    ( (fun n -> ((n :> Paths.Reference.t) :> General_paths.reference)),
      General_paths.reference )
