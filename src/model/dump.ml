module Identifier =struct
    module Signature = struct
        let rec dump : Paths.Identifier.Signature.t -> string = function
            | `Root(_, n) -> (Names.UnitName.to_string n)
            | `Module(s, n) -> Printf.sprintf "%s.%s" (dump s) (Names.ModuleName.to_string n)
            | `Argument(s, i, n) -> Printf.sprintf "(argument %s %d %s)" (dump s) i (Names.ArgumentName.to_string n)
            | `ModuleType(s, n) -> Printf.sprintf "%s.%s" (dump s) (Names.ModuleTypeName.to_string n)
    end
    module ClassSignature = struct
        let dump : Paths.Identifier.ClassSignature.t -> string = function
            | `Class(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassName.to_string n)
            | `ClassType(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassTypeName.to_string n)
    end
    module DataType = struct
        let dump : Paths.Identifier.DataType.t -> string = function
            | `Type(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.TypeName.to_string n)
            | `CoreType n -> Names.TypeName.to_string n
    end
    module Parent = struct
        let dump : Paths.Identifier.Parent.t -> string = function
            | #Paths_types.Identifier.signature as s -> Signature.dump s
            | #Paths_types.Identifier.datatype as t -> DataType.dump t
            | #Paths_types.Identifier.class_signature as s -> ClassSignature.dump s
    end
    module LabelParent = struct
        let dump : Paths.Identifier.LabelParent.t -> string = function
            | #Paths_types.Identifier.parent as p -> Parent.dump p
            | `Page(_, n) -> Printf.sprintf "(page %s)" (Names.PageName.to_string n)
    end
    module Module = struct
        let dump : Paths.Identifier.Module.t -> string = function
            | `Root(_, n) -> (Names.UnitName.to_string n)
            | `Module(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ModuleName.to_string n)
            | `Argument(s, i, n) -> Printf.sprintf "(argument %s %d %s)" (Signature.dump s) i (Names.ArgumentName.to_string n)
    end
    module ModuleType = struct
        let dump : Paths.Identifier.ModuleType.t -> string = function
            | `ModuleType(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ModuleTypeName.to_string n)
    end
    module Type = struct
        let dump : Paths.Identifier.Type.t -> string = function
            | `Type(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.TypeName.to_string n)
            | `CoreType n -> Names.TypeName.to_string n
    end
    module Constructor = struct
        let dump : Paths.Identifier.Constructor.t -> string = function
            | `Constructor(p, n) -> Printf.sprintf "%s.%s" (Type.dump p) (Names.ConstructorName.to_string n)
    end
    module Field = struct
        let dump : Paths.Identifier.Field.t -> string = function
            | `Field(p, n) -> Printf.sprintf "%s.%s" (Parent.dump p) (Names.FieldName.to_string n)
    end
    module Extension = struct
        let dump : Paths.Identifier.Extension.t -> string = function
            | `Extension(p, n) -> Printf.sprintf "%s.%s" (Signature.dump p) (Names.ExtensionName.to_string n)
    end
    module Exception = struct
        let dump : Paths.Identifier.Exception.t -> string = function
            | `Exception(p, n) -> Printf.sprintf "%s.%s" (Signature.dump p) (Names.ExceptionName.to_string n)
            | `CoreException n -> (Names.ExceptionName.to_string n)
    end
    module Value = struct
        let dump : Paths.Identifier.Value.t -> string = function
            | `Value(p, n) -> Printf.sprintf "%s.%s" (Signature.dump p) (Names.ValueName.to_string n)
    end
    module Class = struct
        let dump : Paths.Identifier.Class.t -> string = function
            | `Class(p, n) -> Printf.sprintf "%s.%s" (Signature.dump p) (Names.ClassName.to_string n)
    end
    module ClassType = struct
        let dump : Paths.Identifier.ClassType.t -> string = function
            | `ClassType(p, n) -> Printf.sprintf "%s.%s" (Signature.dump p) (Names.ClassTypeName.to_string n)
    end
    module Method = struct
        let dump : Paths.Identifier.Method.t -> string = function
            | `Method(p, n) -> Printf.sprintf "%s.%s" (ClassSignature.dump p) (Names.MethodName.to_string n)
    end
    module InstanceVariable = struct
        let dump : Paths.Identifier.InstanceVariable.t -> string = function
            | `InstanceVariable(p, n) -> Printf.sprintf "%s.%s" (ClassSignature.dump p) (Names.InstanceVariableName.to_string n)
    end
    module Label = struct
        let dump : Paths.Identifier.Label.t -> string = function
            | `Label(p, n) -> Printf.sprintf "%s.%s" (LabelParent.dump p) (Names.LabelName.to_string n)
    end
    module Page = struct
        let dump : Paths.Identifier.Page.t -> string = function
            | `Page(_, n) -> Printf.sprintf "(page %s)" (Names.PageName.to_string n)
    end

    module Path = struct
        module Module = struct
            let dump : Paths.Identifier.Path.Module.t -> string = function
                | `Root(_, n) -> (Names.UnitName.to_string n)
                | `Module(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ModuleName.to_string n)
                | `Argument(s, i, n) -> Printf.sprintf "(argument %s %d %s)" (Signature.dump s) i (Names.ArgumentName.to_string n)
    end
        module ModuleType = struct
            let dump : Paths.Identifier.Path.ModuleType.t -> string = function
                | `ModuleType(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ModuleTypeName.to_string n)
        end
        module Type = struct
            let dump : Paths.Identifier.Path.Type.t -> string = function
                | `Type(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.TypeName.to_string n)
                | `CoreType n -> Names.TypeName.to_string n
                | `Class(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassName.to_string n)
                | `ClassType(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassTypeName.to_string n)
        end
        module ClassType = struct
            let dump : Paths.Identifier.Path.ClassType.t -> string = function
                | `Class(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassName.to_string n)
                | `ClassType(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassTypeName.to_string n)
        end

            let dump : Paths.Identifier.Path.t -> string = function
                | #Paths_types.Identifier.path_module as p -> Module.dump p
                | #Paths_types.Identifier.path_module_type as p -> ModuleType.dump p
                | #Paths_types.Identifier.path_type as p -> Type.dump p
                (* | #Paths_types.Identifier.path_class_type as p -> ClassType.dump p *)
        end

            let dump : Paths.Identifier.t -> string = function
                | #Paths_types.Identifier.signature as s -> Signature.dump s
                | #Paths_types.Identifier.class_signature as s -> ClassSignature.dump s
                | #Paths_types.Identifier.datatype as t -> DataType.dump t
                (* | #Paths_types.Identifier.parent as p -> Parent.dump p *)
                | #Paths_types.Identifier.label_parent as p -> LabelParent.dump p
                (* | #Paths_types.Identifier.module_ as m -> Module.dump m *)
                (* | #Paths_types.Identifier.module_type as m -> ModuleType.dump m *)
                (* | #Paths_types.Identifier.type_ as t -> Type.dump t *)
                | #Paths_types.Identifier.constructor as c -> Constructor.dump c
                | #Paths_types.Identifier.field as f -> Field.dump f
                | #Paths_types.Identifier.extension as e -> Extension.dump e
                | #Paths_types.Identifier.exception_ as e -> Exception.dump e
                | #Paths_types.Identifier.value as v -> Value.dump v
                (* | #Paths_types.Identifier.class_ as c -> Class.dump c *)
                (* | #Paths_types.Identifier.class_type as c -> ClassType.dump c *)
                | #Paths_types.Identifier.method_ as m -> Method.dump m
                | #Paths_types.Identifier.instance_variable as v -> InstanceVariable.dump v
                | #Paths_types.Identifier.label as l -> Label.dump l
                (*  | #Paths_types.Identifier.page as p -> Page.dump p *)
    end
module Path = struct
        let rec dump_resolved_module : Paths.Path.Resolved.Module.t -> string = function
            | `Identifier id -> Printf.sprintf "(ident %s)" (Identifier.Path.Module.dump id)
                | `Subst(mt, m) -> Printf.sprintf "(subst %s %s)" (dump_resolved_module_type mt) (dump_resolved_module m)
                | `SubstAlias(m, n) -> Printf.sprintf "(substalias %s %s)" (dump_resolved_module m) (dump_resolved_module n)
                | `Hidden m -> Printf.sprintf "(hidden %s)" (dump_resolved_module m)
                | `Module(m, n) -> Printf.sprintf "%s.%s" (dump_resolved_module m) (Names.ModuleName.to_string n)
                | `Canonical(m, p) -> Printf.sprintf "(canonical %s %s)" (dump_resolved_module m) (dump_module p)
                | `Apply(m, n) -> Printf.sprintf "%s (%s)" (dump_resolved_module m) (dump_module n)
            and dump_resolved_module_type : Paths.Path.Resolved.ModuleType.t -> string = function
                | `Identifier id -> Printf.sprintf "(ident %s)" (Identifier.Path.ModuleType.dump id)
                | `ModuleType(m, n) -> Printf.sprintf "%s.%s" (dump_resolved_module m) (Names.ModuleTypeName.to_string n)
            and dump_module : Paths.Path.Module.t -> string = function
                | `Resolved r -> Printf.sprintf "(resolved %s)" (dump_resolved_module r)
                | `Root s -> Printf.sprintf "(root %s)" s
                | `Forward s -> Printf.sprintf "(forward %s)" s
                | `Dot(m, n) -> Printf.sprintf "%s.%s" (dump_module m) n
                | `Apply(m, n) -> Printf.sprintf "%s (%s)" (dump_module m) (dump_module n)
    module Resolved = struct

                module Module = struct
                    let dump = dump_resolved_module
end

                module ModuleType = struct
                    let dump = dump_resolved_module_type
                end

                module Type = struct
                    let dump : Paths.Path.Resolved.Type.t -> string = function
                        | `Identifier id -> Printf.sprintf "(ident %s)" (Identifier.Path.Type.dump id)
                | `Type(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) (Names.TypeName.to_string n)
                | `Class(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) (Names.ClassName.to_string n)
                | `ClassType(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) (Names.ClassTypeName.to_string n)
                end

                module ClassType = struct
                    let dump : Paths.Path.Resolved.ClassType.t -> string = function
                        | `Identifier id -> Printf.sprintf "(ident %s)" (Identifier.Path.ClassType.dump id)
                | `Class(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) (Names.ClassName.to_string n)
                | `ClassType(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) (Names.ClassTypeName.to_string n)
                end

                    let dump : Paths.Path.Resolved.t -> string = function
                        | `Identifier id -> Printf.sprintf "(ident %s)" (Identifier.dump id)
                | #Paths_types.Resolved_path.module_no_id as p -> Module.dump (p:>Paths.Path.Resolved.Module.t)
                | #Paths_types.Resolved_path.module_type_no_id as p -> ModuleType.dump (p:>Paths.Path.Resolved.ModuleType.t)
                | #Paths_types.Resolved_path.type_no_id as p -> Type.dump (p:>Paths.Path.Resolved.Type.t)
                (* | #Paths_types.Resolved_path.class_type_no_id as p -> ClassType.dump (p:>Paths.Path.Resolved.ClassType.t) *)

                end

    module Module = struct
        let dump = dump_module
    end

    module ModuleType = struct
        let dump : Paths.Path.ModuleType.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.ModuleType.dump r)
            | `Dot(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) n
    end

    module Type = struct
        let dump : Paths.Path.Type.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.Type.dump r)
            | `Dot(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) n
    end

    module ClassType = struct
        let dump : Paths.Path.ClassType.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.ClassType.dump r)
            | `Dot(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) n
    end

        let dump : Paths.Path.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.dump r)
            | `Root s -> Printf.sprintf "(root %s)" s
            | `Forward s -> Printf.sprintf "(forward %s)" s
            | `Dot(m, n) -> Printf.sprintf "%s.%s" (Module.dump m) n
            | `Apply(m, n) -> Printf.sprintf "%s (%s)" (Module.dump m) (Module.dump n)
    end
module Fragment = struct
    module Resolved = struct
    let rec dump_signature : Paths.Fragment.Resolved.Signature.t -> string = function
        | `Root -> "{ROOT}"
        | `Subst(t, m) -> Printf.sprintf "(subst %s %s)" (Path.Resolved.ModuleType.dump t) (dump_module m)
        | `SubstAlias(t, m) -> Printf.sprintf "(substalias %s %s)" (Path.Resolved.Module.dump t) (dump_module m)
        | `Module(s, n) -> Printf.sprintf "%s.%s" (dump_signature s) (Names.ModuleName.to_string n)
    and dump_module : Paths.Fragment.Resolved.Module.t -> string = function
        | `Subst(t, m) -> Printf.sprintf "(subst %s %s)" (Path.Resolved.ModuleType.dump t) (dump_module m)
        | `SubstAlias(t, m) -> Printf.sprintf "(substalias %s %s)" (Path.Resolved.Module.dump t) (dump_module m)
        | `Module(s, n) -> Printf.sprintf "%s.%s" (dump_signature s) (Names.ModuleName.to_string n)

        module Signature = struct
            let dump = dump_signature
        end
        module Module = struct
            let dump = dump_module
        end
        module Type = struct
            let dump : Paths.Fragment.Resolved.Type.t -> string = function
                | `Type(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.TypeName.to_string n)
                | `Class(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassName.to_string n)
                | `ClassType(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) (Names.ClassTypeName.to_string n)
        end

            let dump : Paths.Fragment.Resolved.t -> string = function
                | `Root | `Subst _ | `SubstAlias _ | `Module _ as m -> Signature.dump m
                | `Type _ | `Class _ | `ClassType _ as t -> Type.dump t
end
    module Signature = struct
        let rec dump : Paths.Fragment.Signature.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.Signature.dump r)
            | `Dot(s, n) -> Printf.sprintf "%s.%s" (dump s) n
    end
    module Module = struct
        let dump : Paths.Fragment.Module.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.Module.dump r)
            | `Dot(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) n
    end
    module Type = struct
        let dump : Paths.Fragment.Type.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.Type.dump r)
            | `Dot(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) n
    end
        let dump : Paths.Fragment.t -> string = function
            | `Resolved r -> Printf.sprintf "(resolved %s)" (Resolved.dump r)
            | `Dot(s, n) -> Printf.sprintf "%s.%s" (Signature.dump s) n
end
