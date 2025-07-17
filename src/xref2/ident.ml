open Odoc_model.Names
open Odoc_model.Paths

(* For simplicity keep a global counter *)
let counter = ref 0

type module_ = [ `LModule of ModuleName.t * int ]

type module_type = [ `LModuleType of ModuleTypeName.t * int ]

type type_ = [ `LType of TypeName.t * int ]

type constructor = [ `LConstructor of ConstructorName.t * int ]

type field = [ `LField of FieldName.t * int ]

type unboxed_field = [ `LUnboxedField of UnboxedFieldName.t * int ]

type extension = [ `LExtension of ExtensionName.t * int ]

type exception_ = [ `LException of ExceptionName.t * int ]

type value = [ `LValue of ValueName.t * int ]

type method_ = [ `LMethod of MethodName.t * int ]

type instance_variable = [ `LInstanceVariable of InstanceVariableName.t * int ]

type label = [ `LLabel of LabelName.t * int ]

type page = [ `LPage of PageName.t * int ]

type any =
  [ module_
  | module_type
  | type_
  | constructor
  | field
  | unboxed_field
  | extension
  | exception_
  | value
  | method_
  | instance_variable
  | label
  | page ]

let fresh_int () =
  let n = !counter in
  incr counter;
  n

let int_of_any : any -> int = function
  | `LModule (_, i)
  | `LException (_, i)
  | `LConstructor (_, i)
  | `LMethod (_, i)
  | `LType (_, i)
  | `LValue (_, i)
  | `LInstanceVariable (_, i)
  | `LField (_, i)
  | `LUnboxedField (_, i)
  | `LLabel (_, i)
  | `LModuleType (_, i)
  | `LPage (_, i)
  | `LExtension (_, i) ->
      i

module Of_Identifier = struct
  open Identifier

  let type_ : Type.t -> type_ =
   fun t ->
    let i = fresh_int () in
    match t.iv with `Type (_, n) -> `LType (n, i)

  let module_ : Module.t -> module_ = function
    | { iv = `Module (_, n) | `Root (_, n); _ } ->
        let i = fresh_int () in
        `LModule (n, i)
    | { iv = `Parameter (_, n); _ } ->
        let i = fresh_int () in
        `LModule (n, i)

  let functor_parameter : FunctorParameter.t -> module_ =
   fun { iv = `Parameter (_, n); _ } -> `LModule (n, fresh_int ())

  let module_type : ModuleType.t -> module_type =
   fun m ->
    let i = fresh_int () in
    match m.iv with `ModuleType (_, n) -> `LModuleType (n, i)

  let extension : Extension.t -> extension =
   fun e -> match e.iv with `Extension (_, n) -> `LExtension (n, fresh_int ())

  let exception_ : Exception.t -> exception_ =
   fun e -> match e.iv with `Exception (_, n) -> `LException (n, fresh_int ())

  let value : Value.t -> value =
   fun v -> match v.iv with `Value (_, n) -> `LValue (n, fresh_int ())

  let class_ : Class.t -> type_ =
   fun c -> match c.iv with `Class (_, n) -> `LType (n, fresh_int ())

  let class_type : ClassType.t -> type_ =
   fun c -> match c.iv with `ClassType (_, n) -> `LType (n, fresh_int ())

  let method_ : Method.t -> method_ =
   fun c -> match c.iv with `Method (_, n) -> `LMethod (n, fresh_int ())

  let instance_variable : InstanceVariable.t -> instance_variable =
   fun i ->
    match i.iv with
    | `InstanceVariable (_, n) -> `LInstanceVariable (n, fresh_int ())

  let label : Label.t -> label =
   fun l -> match l.iv with `Label (_, n) -> `LLabel (n, fresh_int ())
end

module Name = struct
  let typed_module : module_ -> ModuleName.t = function `LModule (n, _) -> n
  let module_ m = ModuleName.to_string (typed_module m)

  let unsafe_module m = ModuleName.to_string_unsafe (typed_module m)

  let typed_type : type_ -> TypeName.t = function `LType (n, _) -> n
  let type_ t = TypeName.to_string (typed_type t)

  let unsafe_type : type_ -> string = function
    | `LType (n, _) -> TypeName.to_string_unsafe n

  let module_type : module_type -> string = function
    | `LModuleType (n, _) -> ModuleTypeName.to_string n

  let unsafe_module_type : module_type -> string = function
    | `LModuleType (n, _) -> ModuleTypeName.to_string_unsafe n

  let typed_module_type : module_type -> ModuleTypeName.t = function
    | `LModuleType (n, _) -> n

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
  let module_ : module_ -> module_ = function
    | `LModule (n, _) -> `LModule (n, fresh_int ())

  let module_type : module_type -> module_type = function
    | `LModuleType (n, _) -> `LModuleType (n, fresh_int ())

  let type_ : type_ -> type_ = function
    | `LType (n, _) -> `LType (n, fresh_int ())

  let exception_ : exception_ -> exception_ = function
    | `LException (n, _) -> `LException (n, fresh_int ())

  let value : value -> value = function
    | `LValue (n, _) -> `LValue (n, fresh_int ())
end

let hash : any -> int = Hashtbl.hash

let compare : any -> any -> int = fun a b -> int_of_any a - int_of_any b

let reset () = counter := 0

let fmt_aux (id : any) : string * int =
  match id with
  | `LModule (n, i) -> (ModuleName.to_string n, i)
  | `LModuleType (n, i) -> (ModuleTypeName.to_string n, i)
  | `LType (n, i) -> (TypeName.to_string n, i)
  | `LConstructor (n, i) -> (ConstructorName.to_string n, i)
  | `LField (n, i) -> (FieldName.to_string n, i)
  | `LUnboxedField (n, i) -> (UnboxedFieldName.to_string n, i)
  | `LExtension (n, i) -> (ExtensionName.to_string n, i)
  | `LException (n, i) -> (ExceptionName.to_string n, i)
  | `LValue (n, i) -> (ValueName.to_string n, i)
  | `LMethod (n, i) -> (MethodName.to_string n, i)
  | `LInstanceVariable (n, i) -> (InstanceVariableName.to_string n, i)
  | `LLabel (n, i) -> (LabelName.to_string n, i)
  | `LPage (n, i) -> (PageName.to_string n, i)

let fmt : Format.formatter -> [< any ] -> unit =
 fun ppf id ->
  let n, i = fmt_aux (id :> any) in
  Format.fprintf ppf "%s/%d" n i

let short_fmt : Format.formatter -> [< any ] -> unit =
 fun ppf id ->
  let n, _i = fmt_aux (id :> any) in
  Format.fprintf ppf "%s" n

let rename (s, _) = (s, fresh_int ())
