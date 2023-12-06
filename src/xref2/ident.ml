open Odoc_model.Names
open Odoc_model.Paths

(* For simplicity keep a global counter *)
let counter = ref 0

type signature =
  [ `LRoot of ModuleName.t * int
  | `LModule of ModuleName.t * int
  | `LResult of signature * int
  | `LParameter of ModuleName.t * int
  | `LModuleType of ModuleTypeName.t * int ]

type class_signature =
  [ `LClass of ClassName.t * int | `LClassType of ClassTypeName.t * int ]

type datatype = [ `LType of TypeName.t * int ]

type parent = [ signature | datatype ]

type label_parent =
  [ parent
  | `LPage of PageName.t * int
  | `LLeafPage of PageName.t * int
  | class_signature ]

type module_ =
  [ `LRoot of ModuleName.t * int
  | `LModule of ModuleName.t * int
  | `LParameter of ModuleName.t * int ]

type functor_parameter = [ `LParameter of ModuleName.t * int ]

type path_module = [ module_ | `LResult of signature * int | functor_parameter ]

type module_type = [ `LModuleType of ModuleTypeName.t * int ]

type type_ = datatype

type constructor = [ `LConstructor of ConstructorName.t * int ]

type field = [ `LField of FieldName.t * int ]

type extension = [ `LExtension of ExtensionName.t * int ]

type exception_ = [ `LException of ExceptionName.t * int ]

type value = [ `LValue of ValueName.t * int ]

type class_ = [ `LClass of ClassName.t * int ]

type class_type = [ `LClassType of ClassTypeName.t * int ]

type path_type = [ type_ | class_ | class_type ]

type path_datatype = type_

type path_value = value

type path_class_type = [ class_ | class_type ]

type method_ = [ `LMethod of MethodName.t * int ]

type instance_variable = [ `LInstanceVariable of InstanceVariableName.t * int ]

type label = [ `LLabel of LabelName.t * int ]

type page = [ `LPage of PageName.t * int | `LLeafPage of PageName.t * int ]

type any =
  [ signature
  | class_signature
  | datatype
  | parent
  | label_parent
  | path_module
  | module_type
  | type_
  | constructor
  | field
  | extension
  | exception_
  | value
  | class_
  | class_type
  | method_
  | instance_variable
  | label
  | page ]

let fresh_int () =
  let n = !counter in
  incr counter;
  n

let int_of_any : any -> int = function
  | `LRoot (_, i)
  | `LModule (_, i)
  | `LException (_, i)
  | `LConstructor (_, i)
  | `LClassType (_, i)
  | `LMethod (_, i)
  | `LClass (_, i)
  | `LType (_, i)
  | `LValue (_, i)
  | `LInstanceVariable (_, i)
  | `LParameter (_, i)
  | `LField (_, i)
  | `LResult (_, i)
  | `LLabel (_, i)
  | `LModuleType (_, i)
  | `LPage (_, i)
  | `LLeafPage (_, i)
  | `LExtension (_, i) ->
      i

module Of_Identifier = struct
  open Identifier

  let rec signature : Signature.t -> signature =
   fun sg ->
    let i = fresh_int () in
    match sg.iv with
    | `Root (_, n) -> `LRoot (n, i)
    | `Module (_, n) -> `LModule (n, i)
    | `Parameter (_, n) -> `LParameter (n, i)
    | `ModuleType (_, n) -> `LModuleType (n, i)
    | `Result s -> `LResult (signature s, i)

  let class_signature : ClassSignature.t -> class_signature =
   fun sg ->
    let i = fresh_int () in
    match sg.iv with
    | `Class (_, n) -> `LClass (n, i)
    | `ClassType (_, n) -> `LClassType (n, i)

  let datatype : DataType.t -> datatype =
   fun t ->
    let i = fresh_int () in
    match t.iv with
    | `Type (_, n) -> `LType (n, i)
    | `CoreType _n -> failwith "Bad"

  let field_parent : FieldParent.t -> parent =
   fun p ->
    match p with
    | { iv = #Signature.t_pv; _ } as s -> (signature s :> parent)
    | { iv = #DataType.t_pv; _ } as s -> (datatype s :> parent)

  let label_parent : LabelParent.t -> label_parent =
   fun p ->
    match p with
    | { iv = #ClassSignature.t_pv; _ } as s ->
        (class_signature s :> label_parent)
    | { iv = #FieldParent.t_pv; _ } as s -> (field_parent s :> label_parent)
    | { iv = `Page (_, n); _ } -> `LPage (n, fresh_int ())
    | { iv = `LeafPage (_, n); _ } -> `LLeafPage (n, fresh_int ())

  let module_ : Odoc_model.Paths.Identifier.Module.t -> module_ = function
    | { iv = `Module (_, n) | `Root (_, n); _ } ->
        let i = fresh_int () in
        `LModule (n, i)
    | { iv = `Parameter (_, n); _ } ->
        let i = fresh_int () in
        `LParameter (n, i)

  let functor_parameter :
      Odoc_model.Paths.Identifier.FunctorParameter.t -> functor_parameter =
   fun { iv = `Parameter (_, n); _ } -> `LParameter (n, fresh_int ())

  let path_module : Path.Module.t -> path_module =
   fun m ->
    let i = fresh_int () in
    match m.iv with
    | `Root (_, n) -> `LRoot (n, i)
    | `Module (_, n) -> `LModule (n, i)
    | `Parameter (_, n) -> `LParameter (n, i)
    | `Result x -> `LResult (signature x, i)

  let module_type : ModuleType.t -> module_type =
   fun m ->
    let i = fresh_int () in
    match m.iv with `ModuleType (_, n) -> `LModuleType (n, i)

  let type_ : Type.t -> type_ = datatype

  let constructor : Constructor.t -> constructor =
   fun c ->
    match c.iv with `Constructor (_, n) -> `LConstructor (n, fresh_int ())

  let field : Field.t -> field =
   fun f -> match f.iv with `Field (_, n) -> `LField (n, fresh_int ())

  let extension : Extension.t -> extension =
   fun e -> match e.iv with `Extension (_, n) -> `LExtension (n, fresh_int ())

  let exception_ : Exception.t -> exception_ =
   fun e ->
    match e.iv with
    | `Exception (_, n) -> `LException (n, fresh_int ())
    | `CoreException _ -> failwith "Bad"

  let value : Value.t -> value =
   fun v -> match v.iv with `Value (_, n) -> `LValue (n, fresh_int ())

  let class_ : Class.t -> class_ =
   fun c -> match c.iv with `Class (_, n) -> `LClass (n, fresh_int ())

  let class_type : ClassType.t -> class_type =
   fun c -> match c.iv with `ClassType (_, n) -> `LClassType (n, fresh_int ())

  let method_ : Method.t -> method_ =
   fun c -> match c.iv with `Method (_, n) -> `LMethod (n, fresh_int ())

  let instance_variable : InstanceVariable.t -> instance_variable =
   fun i ->
    match i.iv with
    | `InstanceVariable (_, n) -> `LInstanceVariable (n, fresh_int ())

  let label : Label.t -> label =
   fun l -> match l.iv with `Label (_, n) -> `LLabel (n, fresh_int ())

  let page : Page.t -> page =
   fun p ->
    match p.iv with
    | `Page (_, n) -> `LPage (n, fresh_int ())
    | `LeafPage (_, n) -> `LLeafPage (n, fresh_int ())
end

module Name = struct
  let rec signature : signature -> string = function
    | `LRoot (n, _) -> ModuleName.to_string n
    | `LModule (n, _) -> ModuleName.to_string n
    | `LResult (x, _) -> signature x
    | `LParameter (n, _) -> ModuleName.to_string n
    | `LModuleType (n, _) -> ModuleTypeName.to_string n

  let typed_module : module_ -> ModuleName.t = function
    | `LRoot (n, _) | `LModule (n, _) | `LParameter (n, _) -> n

  let module' : module_ -> ModuleName.t = function
    | `LRoot (n, _) | `LModule (n, _) | `LParameter (n, _) -> n

  let module_ m = ModuleName.to_string (module' m)

  let unsafe_module m = ModuleName.to_string_unsafe (module' m)

  let path_module : path_module -> string = function
    | `LRoot (n, _) -> ModuleName.to_string n
    | `LModule (n, _) -> ModuleName.to_string n
    | `LResult (x, _) -> signature x
    | `LParameter (n, _) -> ModuleName.to_string n

  let typed_functor_parameter : functor_parameter -> ModuleName.t =
   fun (`LParameter (n, _)) -> n

  let functor_parameter : functor_parameter -> string =
   fun (`LParameter (n, _)) -> ModuleName.to_string n

  let type' : type_ -> TypeName.t = function `LType (n, _) -> n

  let type_ t = TypeName.to_string (type' t)

  let unsafe_type : type_ -> string = function
    | `LType (n, _) -> TypeName.to_string_unsafe n

  let typed_type : type_ -> TypeName.t = function `LType (n, _) -> n

  let path_type : path_type -> string = function
    | `LClassType (n, _) -> ClassTypeName.to_string n
    | `LClass (n, _) -> ClassName.to_string n
    | `LType (n, _) -> TypeName.to_string n

  let class' : class_ -> ClassName.t = function `LClass (n, _) -> n

  let class_ c = ClassName.to_string (class' c)

  let unsafe_class c = ClassName.to_string_unsafe (class' c)

  let typed_class : class_ -> ClassName.t = function `LClass (n, _) -> n

  let module_type : module_type -> string = function
    | `LModuleType (n, _) -> ModuleTypeName.to_string n

  let unsafe_module_type : module_type -> string = function
    | `LModuleType (n, _) -> ModuleTypeName.to_string_unsafe n

  let typed_module_type : module_type -> ModuleTypeName.t = function
    | `LModuleType (n, _) -> n

  let path_class_type : path_class_type -> string = function
    | `LClass (n, _) -> ClassName.to_string n
    | `LClassType (n, _) -> ClassTypeName.to_string n

  let class_type' : class_type -> ClassTypeName.t = function
    | `LClassType (n, _) -> n

  let class_type c = ClassTypeName.to_string (class_type' c)

  let unsafe_class_type c = ClassTypeName.to_string_unsafe (class_type' c)

  let typed_class_type : class_type -> ClassTypeName.t = function
    | `LClassType (n, _) -> n

  let exception_ : exception_ -> string = function
    | `LException (n, _) -> ExceptionName.to_string n

  let typed_exception : exception_ -> ExceptionName.t = function
    | `LException (n, _) -> n

  let value : value -> string = function
    | `LValue (n, _) -> ValueName.to_string n

  let typed_value : value -> ValueName.t = function `LValue (n, _) -> n

  let label : label -> string = function
    | `LLabel (n, _) -> LabelName.to_string n

  let typed_label : label -> LabelName.t = function `LLabel (n, _) -> n

  let method_ : method_ -> string = function
    | `LMethod (n, _) -> MethodName.to_string n

  let typed_method : method_ -> MethodName.t = function `LMethod (n, _) -> n

  let instance_variable : instance_variable -> string = function
    | `LInstanceVariable (n, _) -> InstanceVariableName.to_string n

  let typed_instance_variable : instance_variable -> InstanceVariableName.t =
    function
    | `LInstanceVariable (n, _) -> n
end

module Rename = struct
  let rec signature : signature -> signature = function
    | `LRoot (n, _) -> `LRoot (n, fresh_int ())
    | `LModule (n, _) -> `LModule (n, fresh_int ())
    | `LResult (x, _) -> `LResult (signature x, fresh_int ())
    | `LParameter (n, _) -> `LParameter (n, fresh_int ())
    | `LModuleType (n, _) -> `LModuleType (n, fresh_int ())

  let module_ : module_ -> module_ = function
    | `LRoot (n, _) -> `LRoot (n, fresh_int ())
    | `LModule (n, _) -> `LModule (n, fresh_int ())
    | `LParameter (n, _) -> `LParameter (n, fresh_int ())

  let path_module : path_module -> path_module = function
    | `LRoot (n, _) -> `LRoot (n, fresh_int ())
    | `LModule (n, _) -> `LModule (n, fresh_int ())
    | `LResult (x, _) -> `LResult (signature x, fresh_int ())
    | `LParameter (n, _) -> `LParameter (n, fresh_int ())

  let module_type : module_type -> module_type = function
    | `LModuleType (n, _) -> `LModuleType (n, fresh_int ())

  let type_ : type_ -> type_ = function
    | `LType (n, _) -> `LType (n, fresh_int ())

  let exception_ : exception_ -> exception_ = function
    | `LException (n, _) -> `LException (n, fresh_int ())

  let value : value -> value = function
    | `LValue (n, _) -> `LValue (n, fresh_int ())

  let class_ : class_ -> class_ = function
    | `LClass (n, _) -> `LClass (n, fresh_int ())

  let class_type : class_type -> class_type = function
    | `LClassType (n, _) -> `LClassType (n, fresh_int ())
end

let hash : any -> int = Hashtbl.hash

let compare : any -> any -> int = fun a b -> int_of_any a - int_of_any b

module Maps = struct
  module Module = Map.Make (struct
    type t = module_

    let compare x y = compare (x : t :> any) (y : t :> any)
  end)

  module ModuleType = Map.Make (struct
    type t = module_type

    let compare x y = compare (x : t :> any) (y : t :> any)
  end)

  module Type = Map.Make (struct
    type t = type_

    let compare x y = compare (x : t :> any) (y : t :> any)
  end)
end

let reset () = counter := 0

let rec fmt_aux ppf (id : any) =
  match id with
  | `LRoot (n, i) -> Format.fprintf ppf "%s/%d" (ModuleName.to_string n) i
  | `LModule (n, i) -> Format.fprintf ppf "%s/%d" (ModuleName.to_string n) i
  | `LParameter (n, i) -> Format.fprintf ppf "%s/%d" (ModuleName.to_string n) i
  | `LResult (x, _) -> Format.fprintf ppf "result(%a)" fmt_aux (x :> any)
  | `LModuleType (n, i) ->
      Format.fprintf ppf "%s/%d" (ModuleTypeName.to_string n) i
  | `LType (n, i) -> Format.fprintf ppf "%s/%d" (TypeName.to_string n) i
  | `LConstructor (n, i) ->
      Format.fprintf ppf "%s/%d" (ConstructorName.to_string n) i
  | `LField (n, i) -> Format.fprintf ppf "%s/%d" (FieldName.to_string n) i
  | `LExtension (n, i) ->
      Format.fprintf ppf "%s/%d" (ExtensionName.to_string n) i
  | `LException (n, i) ->
      Format.fprintf ppf "%s/%d" (ExceptionName.to_string n) i
  | `LValue (n, i) -> Format.fprintf ppf "%s/%d" (ValueName.to_string n) i
  | `LClass (n, i) -> Format.fprintf ppf "%s/%d" (ClassName.to_string n) i
  | `LClassType (n, i) ->
      Format.fprintf ppf "%s/%d" (ClassTypeName.to_string n) i
  | `LMethod (n, i) -> Format.fprintf ppf "%s/%d" (MethodName.to_string n) i
  | `LInstanceVariable (n, i) ->
      Format.fprintf ppf "%s/%d" (InstanceVariableName.to_string n) i
  | `LLabel (n, i) -> Format.fprintf ppf "%s/%d" (LabelName.to_string n) i
  | `LPage (n, i) -> Format.fprintf ppf "%s/%d" (PageName.to_string n) i
  | `LLeafPage (n, i) -> Format.fprintf ppf "%s/%d" (PageName.to_string n) i

let fmt : Format.formatter -> [< any ] -> unit =
 fun ppf id -> fmt_aux ppf (id :> any)

let rename (s, _) = (s, fresh_int ())
