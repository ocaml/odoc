
open Paths
open Lang

let rec list_map f l =
  match l with
  | [] -> l
  | x :: r ->
      let x' = f x in
        if x != x' then x' :: List.map f r
        else
          let r' = list_map f r in
            if r != r' then x' :: r'
            else l

let option_map f o =
  match o with
  | None -> o
  | Some x ->
      let x' = f x in
        if x != x' then Some x'
        else o

let pair_map f g p =
  let (a, b) = p in
  let a' = f a in
  let b' = g b in
    if a != a' || b != b' then (a', b')
    else p

class virtual identifier = object (self)

  method virtual root : Root.t -> Root.t

  method identifier : type k . k Identifier.t -> k Identifier.t =
    fun id ->
      let open Identifier in
        match id with
        | Root(root, name) ->
            let root' = self#root root in
            let name' = self#identifier_root_name name in
              if root != root' || name != name' then Root(root', name')
              else id
        | Page(root, name) ->
            let root' = self#root root in
            let name' = self#identifier_page_name name in
              if root != root' || name != name' then Page(root', name')
              else id
        | Module(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_module_name name in
              if parent != parent' || name != name' then
                Module(parent', name')
              else id
        | Argument(parent, pos, name) ->
            let parent' = self#identifier_signature parent in
            let pos' = self#identifier_argument_position pos in
            let name' = self#identifier_argument_name name in
              if parent != parent' || pos != pos' || name != name' then
                Argument(parent', pos', name')
              else id
        | ModuleType(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_module_type_name name in
              if parent != parent' || name != name' then
                ModuleType(parent', name')
              else id
        | Type(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_type_name name in
              if parent != parent' || name != name' then
                Type(parent', name')
              else id
        | CoreType name ->
            let name' = self#identifier_core_type_name name in
              if name != name' then CoreType name'
              else id
        | Constructor(parent, name) ->
            let parent' = self#identifier parent in
            let name' = self#identifier_constructor_name name in
              if parent != parent' || name != name' then
                Constructor(parent', name')
              else id
        | Field(parent, name) ->
            let parent' = self#identifier parent in
            let name' = self#identifier_field_name name in
              if parent != parent' || name != name' then
                Field(parent', name')
              else id
        | Extension(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_extension_name name in
              if parent != parent' || name != name' then
                Extension(parent', name')
              else id
        | Exception(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_exception_name name in
              if parent != parent' || name != name' then
                Exception(parent', name')
              else id
        | CoreException name ->
            let name' = self#identifier_core_exception_name name in
              if name != name' then CoreException name'
              else id
        | Value(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_value_name name in
              if parent != parent' || name != name' then
                Value(parent', name')
              else id
        | Class(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_class_name name in
              if parent != parent' || name != name' then
                Class(parent', name')
              else id
        | ClassType(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else id
        | Method(parent, name) ->
            let parent' = self#identifier_class_signature parent in
            let name' = self#identifier_method_name name in
              if parent != parent' || name != name' then
                Method(parent', name')
              else id
        | InstanceVariable(parent, name) ->
            let parent' = self#identifier_class_signature parent in
            let name' = self#identifier_instance_variable_name name in
              if parent != parent' || name != name' then
                InstanceVariable(parent', name')
              else id
        | Label(parent, name) ->
            let parent' =
              match parent with
              | (Root _ | Module _ | Argument _ | ModuleType _) as parent ->
                label_parent_of_parent
                  (parent_of_signature
                     (self#identifier_signature parent))
              | (Class _ | ClassType _) as parent ->
                label_parent_of_parent
                  (parent_of_class_signature
                     (self#identifier_class_signature parent))
              | Type _ | CoreType _ as parent ->
                label_parent_of_parent
                  (parent_of_datatype
                    (self#identifier_datatype parent))
              | Page _ as parent ->
                label_parent_of_page
                  (self#identifier_page parent)
            in
            let name' = self#identifier_label_name name in
              if parent != parent' || name != name' then
                Label(parent', name')
              else id

  method identifier_root_name name = name

  method identifier_page_name name = name

  method identifier_module_name name = name

  method identifier_argument_position pos = pos

  method identifier_argument_name name = name

  method identifier_module_type_name name = name

  method identifier_type_name name = name

  method identifier_core_type_name name = name

  method identifier_constructor_name name = name

  method identifier_field_name name = name

  method identifier_extension_name name = name

  method identifier_exception_name name = name

  method identifier_core_exception_name name = name

  method identifier_value_name name = name

  method identifier_class_name name = name

  method identifier_class_type_name name = name

  method identifier_method_name name = name

  method identifier_instance_variable_name name = name

  method identifier_label_name name = name

  method identifier_page (id : Identifier.page) =
    self#identifier id

  method identifier_signature (id : Identifier.signature) =
    self#identifier id

  method identifier_class_signature (id : Identifier.class_signature) =
    self#identifier id

  method identifier_datatype (id : Identifier.datatype) =
    self#identifier id

  method identifier_module (id : Identifier.module_) =
    self#identifier id

  method identifier_module_type (id : Identifier.module_type) =
    self#identifier id

  method identifier_type (id : Identifier.type_) =
    self#identifier id

  method identifier_constructor (id : Identifier.constructor) =
    self#identifier id

  method identifier_field (id : Identifier.field) =
    self#identifier id

  method identifier_extension (id : Identifier.extension) =
    self#identifier id

  method identifier_exception (id : Identifier.exception_) =
    self#identifier id

  method identifier_value (id : Identifier.value) =
    self#identifier id

  method identifier_class (id : Identifier.class_) =
    self#identifier id

  method identifier_class_type (id : Identifier.class_type) =
    self#identifier id

  method identifier_method (id : Identifier.method_) =
    self#identifier id

  method identifier_instance_variable (id : Identifier.instance_variable) =
    self#identifier id

  method identifier_label (id : Identifier.label) =
    self#identifier id

end

class virtual path = object (self)

  method virtual identifier : 'k . 'k Identifier.t -> 'k Identifier.t

  method path_resolved : type k. k Path.Resolved.t -> k Path.Resolved.t =
    fun p ->
      let open Path.Resolved in
        match p with
        | Identifier id ->
            let id' = self#identifier id in
              if id != id' then Identifier id'
              else p
        | Subst(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#path_resolved orig in
              if sub != sub' || orig != orig' then Subst(sub', orig')
              else p
        | SubstAlias(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#path_resolved orig in
              if sub != sub' || orig != orig' then SubstAlias(sub', orig')
              else p
        | Hidden hp ->
            let hp' = self#path_resolved hp in
              if hp != hp' then Hidden hp'
              else p
        | Module(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_module_name name in
              if parent != parent' || name != name' then
                Module(parent', name')
              else p
        | Canonical(orig, cano) ->
            let orig' = self#path_resolved orig in
            let cano' = self#path cano in
              if orig != orig' || cano != cano' then Canonical(orig', cano')
              else p
        | Apply(fn, arg) ->
            let fn' = self#path_resolved fn in
            let arg' = self#path arg in
              if fn != fn' || arg != arg' then Apply(fn', arg')
              else p
        | ModuleType(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_module_type_name name in
              if parent != parent' || name != name' then
                ModuleType(parent', name')
              else p
        | Type(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_type_name name in
              if parent != parent' || name != name' then Type(parent', name')
              else p
        | Class(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_class_name name in
              if parent != parent' || name != name' then Class(parent', name')
              else p
        | ClassType(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else p

  method path_resolved_module_name name = name

  method path_resolved_module_type_name name = name

  method path_resolved_type_name name = name

  method path_resolved_class_name name = name

  method path_resolved_class_type_name name = name

  method path_resolved_module (p : Path.Resolved.module_) =
    self#path_resolved p

  method path_resolved_module_type (p : Path.Resolved.module_type) =
    self#path_resolved p

  method path_resolved_type (p : Path.Resolved.type_) =
    self#path_resolved p

  method path_resolved_class_type (p : Path.Resolved.class_type) =
    self#path_resolved p

  method path : type k . k Path.t -> k Path.t =
    fun p ->
      let open Path in
        match p with
        | Resolved res ->
            let res' = self#path_resolved res in
              if res != res' then Resolved res'
              else p
        | Root name ->
            let name' = self#path_root_name name in
              if name != name' then Root name'
              else p
        | Forward name ->
            let name' = self#path_root_name name in
              if name != name' then Forward name'
              else p
        | Dot(parent, name) ->
            let parent' = self#path parent in
            let name' = self#path_dot_name name in
              if parent != parent' || name != name' then Dot(parent', name')
              else p
        | Apply(fn, arg) ->
            let fn' = self#path fn in
            let arg' = self#path arg in
              if fn != fn' || arg != arg' then Apply(fn', arg')
              else p

  method path_root_name name = name

  method path_dot_name name = name

  method path_module (p : Path.module_) =
    self#path p

  method path_module_type (p : Path.module_type) =
    self#path p

  method path_type (p : Path.type_) =
    self#path p

  method path_class_type (p : Path.class_type) =
    self#path p

end

class virtual fragment = object (self)

  method virtual path_resolved : 'k. 'k Path.Resolved.t ->
                                   'k Path.Resolved.t

  method fragment_resolved : type k s. (k, s) Fragment.Resolved.raw ->
                                         (k, s) Fragment.Resolved.raw =
    fun p ->
      let open Fragment.Resolved in
        match p with
        | Root -> p
        | Subst(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#fragment_resolved orig in
              if sub != sub' || orig != orig' then Subst(sub', orig')
              else p
        | SubstAlias(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#fragment_resolved orig in
              if sub != sub' || orig != orig' then SubstAlias(sub', orig')
              else p
        | Module(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_module_name name in
              if parent != parent' || name != name' then Module(parent', name')
              else p
        | Type(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_type_name name in
              if parent != parent' || name != name' then Type(parent', name')
              else p
        | Class(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_class_name name in
              if parent != parent' || name != name' then Class(parent', name')
              else p
        | ClassType(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else p

  method fragment_resolved_module_name name = name

  method fragment_resolved_type_name name = name

  method fragment_resolved_class_name name = name

  method fragment_resolved_class_type_name name = name

  method fragment_resolved_module (p : Fragment.Resolved.module_) =
    self#fragment_resolved p

  method fragment_resolved_type (p : Fragment.Resolved.type_) =
    self#fragment_resolved p

  method fragment : type k s. (k, s) Fragment.raw -> (k, s) Fragment.raw =
    fun p ->
      let open Fragment in
        match p with
        | Resolved res ->
            let res' = self#fragment_resolved res in
              if res != res' then Resolved res'
              else p
        | Dot(parent, name) ->
            let parent' = self#fragment parent in
            let name' = self#fragment_name name in
              if parent != parent' || name != name' then Dot(parent', name')
              else p

  method fragment_name name = name

  method fragment_module (p : Fragment.module_) =
    self#fragment p

  method fragment_type (p : Fragment.type_) =
    self#fragment p

end

class virtual reference = object (self)

  method virtual identifier : 'k . 'k Identifier.t -> 'k Identifier.t

  method virtual path_resolved : 'k. 'k Path.Resolved.t -> 'k Path.Resolved.t

  method reference_resolved : type k. k Reference.Resolved.t ->
                                        k Reference.Resolved.t =
    fun r ->
      let open Reference.Resolved in
        match r with
        | Identifier id ->
            let id' = self#identifier id in
              if id != id' then Identifier id'
              else r
        | SubstAlias(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#reference_resolved orig in
              if sub != sub' || orig != orig' then
                SubstAlias(sub', orig')
              else r
        | Module(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_module_name name in
              if parent != parent' || name != name' then
                Module(parent', name')
              else r
        | Canonical(orig, cano) ->
            let orig' = self#reference_resolved orig in
            let cano' = self#reference cano in
              if orig != orig' || cano != cano' then
                Canonical(orig', cano')
              else r
        | ModuleType(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_module_type_name name in
              if parent != parent' || name != name' then
                ModuleType(parent', name')
              else r
        | Type(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_type_name name in
              if parent != parent' || name != name' then
                Type(parent', name')
              else r
        | Constructor(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_constructor_name name in
              if parent != parent' || name != name' then
                Constructor(parent', name')
              else r
        | Field(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_field_name name in
              if parent != parent' || name != name' then
                Field(parent', name')
              else r
        | Extension(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_extension_name name in
              if parent != parent' || name != name' then
                Extension(parent', name')
              else r
        | Exception(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_exception_name name in
              if parent != parent' || name != name' then
                Exception(parent', name')
              else r
        | Value(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_value_name name in
              if parent != parent' || name != name' then
                Value(parent', name')
              else r
        | Class(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_class_name name in
              if parent != parent' || name != name' then
                Class(parent', name')
              else r
        | ClassType(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else r
        | Method(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_method_name name in
              if parent != parent' || name != name' then
                Method(parent', name')
              else r
        | InstanceVariable(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_instance_variable_name name in
              if parent != parent' || name != name' then
                InstanceVariable(parent', name')
              else r
        | Label(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_label_name name in
              if parent != parent' || name != name' then
                Label(parent', name')
              else r

  method reference_resolved_module_name name = name

  method reference_resolved_module_type_name name = name

  method reference_resolved_type_name name = name

  method reference_resolved_class_name name = name

  method reference_resolved_class_type_name name = name

  method reference_resolved_constructor_name name = name

  method reference_resolved_extension_name name = name

  method reference_resolved_exception_name name = name

  method reference_resolved_field_name name = name

  method reference_resolved_value_name name = name

  method reference_resolved_method_name name = name

  method reference_resolved_instance_variable_name name = name

  method reference_resolved_label_name name = name

  method reference_resolved_module (r : Reference.Resolved.module_) =
    self#reference_resolved r

  method reference_resolved_module_type
           (r : Reference.Resolved.module_type) =
    self#reference_resolved r

  method reference_resolved_type (r : Reference.Resolved.type_) =
    self#reference_resolved r

  method reference_resolved_constructor (r : Reference.Resolved.constructor) =
    self#reference_resolved r

  method reference_resolved_field (r : Reference.Resolved.field) =
    self#reference_resolved r

  method reference_resolved_extension (r : Reference.Resolved.extension) =
    self#reference_resolved r

  method reference_resolved_exception (r : Reference.Resolved.exception_) =
    self#reference_resolved r

  method reference_resolved_value (r : Reference.Resolved.value) =
    self#reference_resolved r

  method reference_resolved_class (r : Reference.Resolved.class_) =
    self#reference_resolved r

  method reference_resolved_class_type (r : Reference.Resolved.class_type) =
    self#reference_resolved r

  method reference_resolved_method (r : Reference.Resolved.method_) =
    self#reference_resolved r

  method reference_resolved_instance_variable
           (r : Reference.Resolved.instance_variable) =
    self#reference_resolved r

  method reference_resolved_label (r : Reference.Resolved.label) =
    self#reference_resolved r

  method reference_resolved_any (r : Reference.Resolved.any) =
    self#reference_resolved r

  method reference : type k . k Reference.t -> k Reference.t =
    fun r ->
      let open Reference in
        match r with
        | Resolved res ->
            let res' = self#reference_resolved res in
              if res != res' then Resolved res'
              else r
        | Root (name, kind) ->
            let name' = self#reference_root_name name in
              if name != name' then Root (name', kind)
              else r
        | Dot(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_dot_name name in
              if parent != parent' || name != name' then Dot(parent', name')
              else r
        | Module(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_module_name name in
              if parent != parent' || name != name' then Module(parent', name')
              else r
        | ModuleType(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_module_type_name name in
              if parent != parent' || name != name' then ModuleType(parent', name')
              else r
        | Type(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_type_name name in
              if parent != parent' || name != name' then Type(parent', name')
              else r
        | Constructor(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_constructor_name name in
              if parent != parent' || name != name' then Constructor(parent', name')
              else r
        | Extension(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_extension_name name in
              if parent != parent' || name != name' then Extension(parent', name')
              else r
        | Exception(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_exception_name name in
              if parent != parent' || name != name' then Exception(parent', name')
              else r
        | Field(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_field_name name in
              if parent != parent' || name != name' then Field(parent', name')
              else r
        | Value(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_value_name name in
              if parent != parent' || name != name' then Value(parent', name')
              else r
        | Class(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_class_name name in
              if parent != parent' || name != name' then Class(parent', name')
              else r
        | ClassType(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_class_type_name name in
              if parent != parent' || name != name' then ClassType(parent', name')
              else r
        | Method(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_method_name name in
              if parent != parent' || name != name' then Method(parent', name')
              else r
        | InstanceVariable(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_instance_variable_name name in
              if parent != parent' || name != name' then InstanceVariable(parent', name')
              else r
        | Label(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_label_name name in
              if parent != parent' || name != name' then Label(parent', name')
              else r

  method reference_root_name name = name

  method reference_dot_name name = name

  method reference_module_name name = name

  method reference_module_type_name name = name

  method reference_type_name name = name

  method reference_constructor_name name = name

  method reference_field_name name = name

  method reference_extension_name name = name

  method reference_exception_name name = name

  method reference_value_name name = name

  method reference_class_name name = name

  method reference_class_type_name name = name

  method reference_method_name name = name

  method reference_instance_variable_name name = name

  method reference_label_name name = name

  method reference_module (r : Reference.module_) =
    self#reference r

  method reference_module_type (r : Reference.module_type) =
    self#reference r

  method reference_type (r : Reference.type_) =
    self#reference r

  method reference_constructor (r : Reference.constructor) =
    self#reference r

  method reference_field (r : Reference.field) =
    self#reference r

  method reference_extension (r : Reference.extension) =
    self#reference r

  method reference_exception (r : Reference.exception_) =
    self#reference r

  method reference_value (r : Reference.value) =
    self#reference r

  method reference_class (r : Reference.class_) =
    self#reference r

  method reference_class_type (r : Reference.class_type) =
    self#reference r

  method reference_method (r : Reference.method_) =
    self#reference r

  method reference_instance_variable (r : Reference.instance_variable) =
    self#reference r

  method reference_label (r : Reference.label) =
    self#reference r

  method reference_any (r : Reference.any) =
    self#reference r

end

class virtual paths = object
  inherit identifier
  inherit path
  inherit fragment
  inherit reference
end

class virtual documentation = object (self)

  method virtual identifier_label :
    Identifier.label -> Identifier.label

  method virtual identifier :
    'k. 'k Identifier.t -> 'k Identifier.t

  method virtual path_module :
    Path.module_ -> Path.module_

  method virtual reference_module :
    Reference.module_ -> Reference.module_

  method virtual reference_module_type :
    Reference.module_type -> Reference.module_type

  method virtual reference_type :
    Reference.type_ -> Reference.type_

  method virtual reference_constructor :
    Reference.constructor -> Reference.constructor

  method virtual reference_field :
    Reference.field -> Reference.field

  method virtual reference_extension :
    Reference.extension -> Reference.extension

  method virtual reference_exception :
    Reference.exception_ -> Reference.exception_

  method virtual reference_value :
    Reference.value -> Reference.value

  method virtual reference_class :
    Reference.class_ -> Reference.class_

  method virtual reference_class_type :
    Reference.class_type -> Reference.class_type

  method virtual reference_method :
    Reference.method_ -> Reference.method_

  method virtual reference_instance_variable :
    Reference.instance_variable -> Reference.instance_variable

  method virtual reference_label :
    Reference.label -> Reference.label

  method virtual reference_any :
    Reference.any -> Reference.any

  method documentation_reference ((path, content) as r) =
    let path' = self#reference_any path in
    if path' != path then
      (path', content)
    else
      r

  method private documentation_special_modules reference =
    let reference' = self#reference_module reference in
    if reference' != reference then
      reference'
    else
      reference

  method private documentation_inline_element element =
    match element with
    | `Styled (style, nested_elements) ->
      let nested_elements =
        List.map
          (Location_.map self#documentation_inline_element) nested_elements
      in
      `Styled (style, nested_elements)
    | `Reference (path, nested_elements) ->
      let path', nested_elements' =
        self#documentation_reference (path, nested_elements) in
      if path' != path || nested_elements' != nested_elements then
        `Reference (path', nested_elements')
      else
        element
    | _ ->
      element

  method private documentation_nestable_block_element = function
    | `Paragraph elements ->
      `Paragraph
        (list_map (Location_.map self#documentation_inline_element) elements)
    | `Modules modules ->
      `Modules (List.map self#documentation_special_modules modules)
    | `List (tag, elements) ->
        `List (tag,
          (List.map
            (List.map
              (Location_.map self#documentation_nestable_block_element))
            elements))
    | element ->
      element

  method private documentation_block_element element =
    match element with
    | #Comment.nestable_block_element as element ->
      (self#documentation_nestable_block_element element
        :> Comment.block_element)
    | `Tag tag ->
      let tag' =
        match tag with
        | `Deprecated elements ->
          `Deprecated
            (list_map
              (Location_.map self#documentation_nestable_block_element)
              elements)
        | `Param (s, elements) ->
          `Param
            (s, list_map
              (Location_.map self#documentation_nestable_block_element)
              elements)
        | `Raise (s, elements) ->
          `Raise
            (s, list_map
              (Location_.map self#documentation_nestable_block_element)
              elements)
        | `Return elements ->
          `Return
            (list_map
              (Location_.map self#documentation_nestable_block_element)
              elements)
        | `See (k, s, elements) ->
          `See
            (k, s, list_map
              (Location_.map self#documentation_nestable_block_element)
              elements)
        | `Before (s, elements) ->
          `Before
            (s, list_map
              (Location_.map self#documentation_nestable_block_element)
              elements)
        | _ ->
          tag
      in
      if tag' != tag then
        `Tag tag'
      else
        element
    | _ ->
      element

  method documentation doc =
    list_map (Location_.map self#documentation_block_element) doc

  method documentation_comment (comment : Comment.docs_or_stop) =
    match comment with
    | `Docs doc ->
      let doc' = self#documentation doc in
      if doc != doc' then `Docs doc'
      else comment
    | `Stop ->
      comment

end

class virtual module_ = object (self)

  method virtual identifier_module :
    Identifier.module_ -> Identifier.module_

  method virtual path_module :
    Path.module_ -> Path.module_

  method virtual reference_module :
    Reference.module_ -> Reference.module_

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual module_type_expr :
    ModuleType.expr -> ModuleType.expr

  method virtual signature : Signature.t -> Signature.t

  method virtual module_type_functor_arg :
    FunctorArgument.t option -> FunctorArgument.t option

  method module_hidden h = h

  method module_expansion expn =
    let open Module in
    match expn with
    | AlreadyASig -> AlreadyASig
    | Signature sg ->
        let sg' = self#signature sg in
        if sg != sg' then Signature sg'
        else expn
    | Functor (args, sg) ->
        let args' = list_map self#module_type_functor_arg args in
        let sg' = self#signature sg in
        if args != args' || sg != sg' then Functor(args', sg')
        else expn

  method module_decl decl =
    let open Module in
      match decl with
      | Alias p ->
          let p' = self#path_module p in
            if p != p' then Alias p'
            else decl
      | ModuleType expr ->
          let expr' = self#module_type_expr expr in
            if expr != expr' then ModuleType expr'
            else decl

  method module_ md =
    let open Module in
    let {id; doc; type_; expansion; canonical; hidden; display_type} = md in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let type' = self#module_decl type_ in
    let expansion' = option_map self#module_expansion expansion in
    let canonical' =
      option_map (pair_map self#path_module self#reference_module) canonical
    in
    let hidden' = self#module_hidden hidden in
    let display_type' = option_map self#module_decl display_type in
      if id != id' || doc != doc' || type_ != type'
         || expansion != expansion' || canonical != canonical'
         || hidden != hidden' || display_type != display_type'
      then
        {id = id'; doc = doc'; type_ = type'; expansion = expansion';
         canonical = canonical'; hidden = hidden'; display_type = display_type'}
      else md

  method module_equation eq =
    self#module_decl eq

end

class virtual module_type = object (self)

  method virtual identifier_module :
    Identifier.module_ -> Identifier.module_

  method virtual identifier_module_type :
    Identifier.module_type -> Identifier.module_type

  method virtual path_module :
    Path.module_ -> Path.module_

  method virtual path_module_type :
    Path.module_type -> Path.module_type

  method virtual path_type :
    Path.type_ -> Path.type_

  method virtual fragment_module :
    Fragment.module_ -> Fragment.module_

  method virtual fragment_type :
    Fragment.type_ -> Fragment.type_

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual module_decl :
    Module.decl -> Module.decl

  method virtual module_equation :
    Module.Equation.t -> Module.Equation.t

  method virtual signature :
    Signature.t -> Signature.t

  method virtual type_decl_equation :
    TypeDecl.Equation.t -> TypeDecl.Equation.t

  method virtual type_decl_param_name :
    string -> string

  method virtual module_expansion :
    Module.expansion -> Module.expansion

  method module_type_substitution subst =
    let open ModuleType in
      match subst with
      | ModuleEq(frag, eq) ->
          let frag' = self#fragment_module frag in
          let eq' = self#module_equation eq in
            if frag != frag' || eq != eq' then ModuleEq(frag', eq')
            else subst
      | TypeEq(frag, eq) ->
          let frag' = self#fragment_type frag in
          let eq' = self#type_decl_equation eq in
            if frag != frag' || eq != eq' then TypeEq(frag', eq')
            else subst
      | ModuleSubst(frag, p) ->
          let frag' = self#fragment_module frag in
          let p' = self#path_module p in
            if frag != frag' || p != p' then
              ModuleSubst(frag', p')
            else subst
      | TypeSubst(frag, eq) ->
          let frag' = self#fragment_type frag in
          let eq' = self#type_decl_equation eq in
            if frag != frag' || eq != eq' then TypeSubst(frag', eq')
            else subst

  method module_type_expr expr =
    let open ModuleType in
      match expr with
      | Path p ->
          let p' = self#path_module_type p in
            if p != p' then Path p'
            else expr
      | Signature sg ->
          let sg' = self#signature sg in
            if sg != sg' then Signature sg'
            else expr
      | Functor(arg, res) ->
          let arg' = self#module_type_functor_arg arg in
          let res' = self#module_type_expr res in
            if arg != arg' || res != res' then Functor(arg', res')
            else expr
      | With(body, substs) ->
          let body' = self#module_type_expr body in
          let substs' = list_map self#module_type_substitution substs in
            if body != body' || substs != substs' then With(body', substs')
            else expr
      | TypeOf decl ->
          let decl' = self#module_decl decl in
            if decl != decl' then TypeOf decl'
            else expr

  method module_type_functor_arg arg =
    match arg with
    | None -> arg
    | Some { FunctorArgument. id; expr; expansion } ->
        let id' = self#identifier_module id in
        let expr' = self#module_type_expr expr in
        let expansion' = option_map self#module_expansion expansion in
          if id != id' || expr != expr' || expansion != expansion' then
            Some {FunctorArgument. id = id'; expr = expr'; expansion = expansion'}
          else arg

  method module_type mty =
    let open ModuleType in
    let {id; doc; expr; expansion} = mty in
    let id' = self#identifier_module_type id in
    let doc' = self#documentation doc in
    let expr' = option_map self#module_type_expr expr in
    let expansion' = option_map self#module_expansion expansion in
      if id != id' || doc != doc' || expr != expr' || expansion != expansion' then
        {id = id'; doc = doc'; expr = expr'; expansion = expansion'}
      else mty
end

class virtual signature = object (self)

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual module_ :
    Module.t -> Module.t

  method virtual module_type :
    ModuleType.t -> ModuleType.t

  method virtual type_decl :
    TypeDecl.t -> TypeDecl.t

  method virtual extension :
    Extension.t -> Extension.t

  method virtual exception_ :
    Exception.t -> Exception.t

  method virtual value :
    Value.t -> Value.t

  method virtual external_ :
    External.t -> External.t

  method virtual class_ :
    Class.t -> Class.t

  method virtual class_type :
    ClassType.t -> ClassType.t

  method virtual include_:
    Include.t -> Include.t

  method signature_item item =
    let open Signature in
      match item with
      | Value v ->
          let v' = self#value v in
            if v != v' then Value v'
            else item
      | External ve ->
          let ve' = self#external_ ve in
            if ve != ve' then External ve'
            else item
      | Type decl ->
          let decl' = self#type_decl decl in
            if decl != decl' then Type decl'
            else item
      | TypExt ext ->
          let ext' = self#extension ext in
            if ext != ext' then TypExt ext'
            else item
      | Exception exn ->
          let exn' = self#exception_ exn in
            if exn != exn' then Exception exn'
            else item
      | Class cls ->
          let cls' = self#class_ cls in
            if cls != cls' then Class cls'
            else item
      | ClassType clty ->
          let clty' = self#class_type clty in
            if clty != clty' then ClassType clty'
            else item
      | Module md ->
          let md' = self#module_ md in
            if md != md' then Module md'
            else item
      | ModuleType mty ->
          let mty' = self#module_type mty in
            if mty != mty' then ModuleType mty'
            else item
      | Include incl ->
          let incl' = self#include_ incl in
            if incl != incl' then Include incl'
            else item
      | Comment com ->
          let com' = self#documentation_comment com in
            if com != com' then Comment com'
            else item

  method signature sg =
    list_map self#signature_item sg

end

class virtual include_ = object (self)

  method virtual module_decl :
    Module.decl -> Module.decl

  method virtual identifier_signature :
    Identifier.signature -> Identifier.signature

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual signature : Signature.t -> Signature.t

  method include_expansion_resolved resolved =
    resolved

  method include_expansion expn =
    let open Include in
    let {resolved; content} = expn in
    let resolved' = self#include_expansion_resolved resolved in
    let content' = self#signature content in
      if content != content' || resolved != resolved' then
        {resolved = resolved'; content = content'}
      else expn

  method include_ incl =
    let open Include in
    let {parent; doc; decl; expansion} = incl in
    let parent' = self#identifier_signature parent in
    let doc' = self#documentation doc in
    let decl' = self#module_decl decl in
    let expansion' = self#include_expansion expansion in
      if parent != parent' || doc != doc' || decl != decl' || expansion != expansion' then
        {parent = parent'; doc = doc'; decl = decl'; expansion = expansion'}
      else incl

end

class virtual type_decl = object (self)

  method virtual identifier_type :
    Identifier.type_ -> Identifier.type_

  method virtual identifier_constructor :
    Identifier.constructor -> Identifier.constructor

  method virtual identifier_field :
    Identifier.field -> Identifier.field

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method type_decl_constructor_argument arg =
    let open TypeDecl.Constructor in
    match arg with
    | Tuple args ->
        let args' = list_map self#type_expr args in
          if args != args' then Tuple args'
          else arg
    | Record fields ->
          let fields' = list_map self#type_decl_field fields in
            if fields != fields' then Record fields'
            else arg

  method type_decl_constructor cstr =
    let open TypeDecl.Constructor in
    let {id; doc; args; res} = cstr in
    let id' = self#identifier_constructor id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else cstr

  method type_decl_field field =
    let open TypeDecl.Field in
    let {id; doc; mutable_; type_} = field in
    let id' = self#identifier_field id in
    let doc' = self#documentation doc in
    let mutable' = self#type_decl_field_mutable mutable_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc'
         || mutable_ != mutable' || type_ != type'
      then
        {id = id'; doc = doc'; mutable_ = mutable'; type_ = type'}
      else field

  method type_decl_field_mutable mutable_ = mutable_

  method type_decl_representation repr =
    let open TypeDecl.Representation in
      match repr with
      | Variant cstrs ->
          let cstrs' = list_map self#type_decl_constructor cstrs in
            if cstrs != cstrs' then Variant cstrs'
            else repr
      | Record fields ->
          let fields' = list_map self#type_decl_field fields in
            if fields != fields' then Record fields'
            else repr
      | Extensible -> repr

  method type_decl_variance variance = variance

  method type_decl_param_desc desc =
    let open TypeDecl in
      match desc with
      | Any -> desc
      | Var name ->
          let name' = self#type_decl_param_name name in
            if name != name' then Var name'
            else desc

  method type_decl_param_name name = name

  method type_decl_param param =
    let desc, var = param in
    let desc' = self#type_decl_param_desc desc in
    let var' = option_map self#type_decl_variance var in
      if desc != desc' || var != var' then (desc', var')
      else param

  method type_decl_equation eq =
    let open TypeDecl.Equation in
    let {params; private_; manifest; constraints} = eq in
    let params' = list_map self#type_decl_param params in
    let private' = self#type_decl_private private_ in
    let manifest' = option_map self#type_expr manifest in
    let constraints' = list_map self#type_decl_constraint constraints in
      if params != params' || private_ != private'
         || manifest != manifest' || constraints != constraints'
      then
        {params = params'; private_ = private';
         manifest = manifest'; constraints = constraints'}
      else eq

  method type_decl_private priv = priv

  method type_decl_constraint cstr =
    let typ1, typ2 = cstr in
    let typ1' = self#type_expr typ1 in
    let typ2' = self#type_expr typ2 in
      if typ1 != typ1' || typ1 != typ1' then (typ1', typ2')
      else cstr

  method type_decl decl =
    let open TypeDecl in
    let {id; doc; equation; representation = repr} = decl in
    let id' = self#identifier_type id in
    let doc' = self#documentation doc in
    let equation' = self#type_decl_equation equation in
    let repr' =
      option_map self#type_decl_representation repr
    in
      if id != id' || doc != doc'
         || equation != equation' || repr != repr'
      then
        {id = id'; doc = doc';
         equation = equation'; representation = repr'}
      else decl

end

class virtual extension = object (self)

  method virtual identifier_extension :
    Identifier.extension -> Identifier.extension

  method virtual path_type :
    Path.type_ -> Path.type_

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual type_decl_private :
    bool -> bool

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method extension_constructor cstr =
    let open Extension.Constructor in
    let {id; doc; args; res} = cstr in
    let id' = self#identifier_extension id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else cstr

  method extension ext =
    let open Extension in
    let {type_path; doc; type_params; private_; constructors} = ext in
    let type_path' = self#path_type type_path in
    let doc' = self#documentation doc in
    let type_params' = list_map self#type_decl_param type_params in
    let private' = self#type_decl_private private_ in
    let constructors' = list_map self#extension_constructor constructors in
      if type_path != type_path' || doc != doc' || type_params != type_params'
         || private_ != private' || constructors != constructors'
      then
        {type_path = type_path'; doc = doc'; type_params = type_params';
         private_ = private'; constructors = constructors'}
      else ext

end

class virtual exception_ = object (self)

  method virtual identifier_exception :
    Identifier.exception_ -> Identifier.exception_

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method exception_ exn =
    let open Exception in
    let {id; doc; args; res} = exn in
    let id' = self#identifier_exception id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else exn

end

class virtual value = object (self)

  method virtual identifier_value :
    Identifier.value -> Identifier.value

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method value v =
    let open Value in
    let {id; doc; type_} = v in
    let id' = self#identifier_value id in
    let doc' = self#documentation doc in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || type_ != type' then
        {id = id'; doc = doc'; type_ = type'}
      else v

end

class virtual external_ = object (self)

  method virtual identifier_value :
    Identifier.value -> Identifier.value

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method external_ ve =
    let open External in
    let {id; doc; type_; primitives} = ve in
    let id' = self#identifier_value id in
    let doc' = self#documentation doc in
    let type' = self#type_expr type_ in
    let primitives' = list_map self#external_primitive primitives in
      if id != id' || doc != doc'
         || type_ != type' || primitives != primitives'
      then
        {id = id'; doc = doc'; type_ = type'; primitives = primitives'}
      else ve

  method external_primitive prim = prim

end

class virtual class_ = object (self)

  method virtual identifier_class :
    Identifier.class_ -> Identifier.class_

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual class_type_expr :
    ClassType.expr -> ClassType.expr

  method virtual type_expr_label :
    TypeExpr.label -> TypeExpr.label

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method virtual class_signature :
    ClassSignature.t -> ClassSignature.t

  method class_decl decl =
    let open Class in
      match decl with
      | ClassType expr ->
          let expr' = self#class_type_expr expr in
            if expr != expr' then ClassType expr'
            else decl
      | Arrow(lbl, typ, body) ->
          let lbl' = option_map self#type_expr_label lbl in
          let typ' = self#type_expr typ in
          let body' = self#class_decl body in
            if lbl != lbl' || typ != typ' || body != body' then
              Arrow(lbl', typ', body')
            else decl

  method class_ cls =
    let open Class in
    let {id; doc; virtual_; params; type_; expansion} = cls in
    let id' = self#identifier_class id in
    let doc' = self#documentation doc in
    let virtual' = self#class_virtual virtual_ in
    let params' = list_map self#type_decl_param params in
    let type' = self#class_decl type_ in
    let expansion' = option_map self#class_signature expansion in
      if id != id' || doc != doc' || virtual_ != virtual'
         || params != params' || type_ != type' || expansion != expansion'
      then
        {id = id'; doc = doc'; virtual_ = virtual';
         params = params'; type_ = type'; expansion = expansion'}
      else cls

  method class_virtual virt = virt

end

class virtual class_type = object (self)

  method virtual identifier_class_type :
    Identifier.class_type -> Identifier.class_type

  method virtual path_class_type :
    Path.class_type -> Path.class_type

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual class_signature :
    ClassSignature.t -> ClassSignature.t

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method class_type_expr expr =
    let open ClassType in
      match expr with
      | Constr(p, params) ->
          let p' = self#path_class_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Constr(p', params')
            else expr
      | Signature csig ->
          let csig' = self#class_signature csig in
            if csig != csig' then Signature csig'
            else expr

  method class_type clty =
    let open ClassType in
    let {id; doc; virtual_; params; expr; expansion} = clty in
    let id' = self#identifier_class_type id in
    let doc' = self#documentation doc in
    let virtual' = self#class_type_virtual virtual_ in
    let params' = list_map self#type_decl_param params in
    let expr' = self#class_type_expr expr in
    let expansion' = option_map self#class_signature expansion in
      if id != id' || doc != doc' || virtual_ != virtual'
         || params != params' || expr != expr' || expansion != expansion'
      then
        {id = id'; doc = doc'; virtual_ = virtual';
         params = params'; expr = expr'; expansion = expansion'}
      else clty

  method class_type_virtual virt = virt

end

class virtual class_signature = object (self)

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual class_type_expr :
    ClassType.expr -> ClassType.expr

  method virtual method_ :
    Method.t -> Method.t

  method virtual instance_variable :
    InstanceVariable.t -> InstanceVariable.t

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method class_signature_item item =
    let open ClassSignature in
      match item with
      | InstanceVariable inst ->
          let inst' = self#instance_variable inst in
            if inst != inst' then InstanceVariable inst'
            else item
      | Method meth ->
          let meth' = self#method_ meth in
            if meth != meth' then Method meth'
            else item
      | Constraint(typ1, typ2) ->
          let typ1' = self#type_expr typ1 in
          let typ2' = self#type_expr typ2 in
            if typ1 != typ1' || typ1 != typ1' then Constraint(typ1', typ2')
            else item
      | Inherit expr ->
          let expr' = self#class_type_expr expr in
            if expr != expr' then Inherit expr'
            else item
      | Comment com ->
          let com' = self#documentation_comment com in
            if com != com' then Comment com'
            else item

  method class_signature csig =
    let open ClassSignature in
    let {self = slf; items} = csig in
    let slf' = option_map self#type_expr slf in
    let items' = list_map self#class_signature_item items in
      if slf != slf' || items != items' then
        {self = slf'; items = items'}
      else csig

end

class virtual method_ = object (self)

  method virtual identifier_method :
    Identifier.method_ -> Identifier.method_

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method method_ meth =
    let open Method in
    let {id; doc; private_; virtual_; type_} = meth in
    let id' = self#identifier_method id in
    let doc' = self#documentation doc in
    let private' = self#method_private private_ in
    let virtual' = self#method_virtual virtual_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || private_ != private'
         || virtual_ != virtual' || type_ != type'
      then
        {id = id'; doc = doc'; private_ = private';
         virtual_ = virtual'; type_ = type'}
      else meth

  method method_private priv = priv

  method method_virtual virt = virt

end

class virtual instance_variable = object (self)

  method virtual identifier_instance_variable :
    Identifier.instance_variable -> Identifier.instance_variable

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method instance_variable meth =
    let open InstanceVariable in
    let {id; doc; mutable_; virtual_; type_} = meth in
    let id' = self#identifier_instance_variable id in
    let doc' = self#documentation doc in
    let mutable' = self#instance_variable_mutable mutable_ in
    let virtual' = self#instance_variable_virtual virtual_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || mutable_ != mutable'
         || virtual_ != virtual' || type_ != type'
      then
        {id = id'; doc = doc'; mutable_ = mutable';
         virtual_ = virtual'; type_ = type'}
      else meth

  method instance_variable_mutable mut = mut

  method instance_variable_virtual virt = virt

end

class virtual type_expr = object (self)

  method virtual path_module_type :
    Path.module_type -> Path.module_type

  method virtual path_type :
    Path.type_ -> Path.type_

  method virtual path_class_type :
    Path.class_type -> Path.class_type

  method virtual fragment_type :
    Fragment.type_ -> Fragment.type_

  method type_expr_variant_kind kind = kind

  method type_expr_variant_element elem =
    let open TypeExpr.Variant in
      match elem with
      | Type typ ->
          let typ' = self#type_expr typ in
            if typ != typ' then Type typ'
            else elem
      | Constructor(name, const, args) ->
          let name' = self#type_expr_variant_constructor_name name in
          let const' = self#type_expr_variant_constructor_const const in
          let args' = list_map self#type_expr args in
            if name != name' || const != const' || args != args' then
              Constructor(name', const', args')
            else elem

  method type_expr_variant_constructor_name name = name

  method type_expr_variant_constructor_const const = const

  method type_expr_variant var =
    let open TypeExpr.Variant in
    let {kind; elements} = var in
    let kind' = self#type_expr_variant_kind kind in
    let elements' = list_map self#type_expr_variant_element elements in
      if kind != kind' || elements != elements' then
        {kind = kind'; elements = elements'}
      else var

  method type_expr_object_method meth =
    let open TypeExpr.Object in
    let {name; type_} = meth in
    let name' = self#type_expr_object_method_name name in
    let type' = self#type_expr type_ in
      if name != name' || type_ != type' then
        {name = name'; type_ = type'}
      else meth

  method type_expr_object_method_name name = name

  method type_expr_object_field fld =
    let open TypeExpr.Object in
    match fld with
    | Method meth ->
        let meth' = self#type_expr_object_method meth in
          if meth != meth' then Method meth' else fld
    | Inherit typ ->
        let typ' = self#type_expr typ in
          if typ != typ' then Inherit typ' else fld

  method type_expr_object obj =
    let open TypeExpr.Object in
    let {fields; open_} = obj in
    let fields' = list_map self#type_expr_object_field fields in
    let open' = self#type_expr_object_open open_ in
      if fields != fields' || open_ != open' then
        {fields = fields'; open_ = open'}
      else obj

  method type_expr_object_open opn = opn

  method type_expr_package_substitution subst =
    let frag, typ = subst in
    let frag' = self#fragment_type frag in
    let typ' = self#type_expr typ in
      if frag != frag' || typ != typ' then (frag', typ')
      else subst

  method type_expr_package pkg =
    let open TypeExpr.Package in
    let {path; substitutions = substs} = pkg in
    let path' = self#path_module_type path in
    let substs' = list_map self#type_expr_package_substitution substs in
      if path != path' || substs != substs' then
        {path = path'; substitutions = substs'}
      else pkg

  method type_expr_label lbl =
    let open TypeExpr in
      match lbl with
      | Label name ->
          let name' = self#type_expr_label_name name in
            if name != name' then Label name'
            else lbl
      | Optional name ->
          let name' = self#type_expr_label_name name in
            if name != name' then Optional name'
            else lbl

  method type_expr_label_name name = name

  method type_expr typ =
    let open TypeExpr in
      match typ with
      | Var name ->
          let name' = self#type_expr_var_name name in
            if name != name' then Var name'
            else typ
      | Any -> typ
      | Alias(body, name) ->
          let body' = self#type_expr body in
          let name' = self#type_expr_var_name name in
            if body != body' || name != name' then Alias(body', name')
            else typ
      | Arrow(lbl, arg, res) ->
          let lbl' = option_map self#type_expr_label lbl in
          let arg' = self#type_expr arg in
          let res' = self#type_expr res in
            if lbl != lbl' || arg != arg' || res != res' then Arrow(lbl', arg', res')
            else typ
      | Tuple typs ->
          let typs' = list_map self#type_expr typs in
            if typs != typs' then Tuple typs'
            else typ
      | Constr(p, params) ->
          let p' = self#path_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Constr(p', params')
            else typ
      | Variant var ->
          let var' = self#type_expr_variant var in
            if var != var' then Variant var'
            else typ
      | Object obj ->
          let obj' = self#type_expr_object obj in
            if obj != obj' then Object obj'
            else typ
      | Class(p, params) ->
          let p' = self#path_class_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Class(p', params')
            else typ
      | Poly(vars, body) ->
          let vars' = list_map self#type_expr_var_name vars in
          let body' = self#type_expr body in
            if vars != vars' || body != body' then Poly(vars', body')
            else typ
      | Package pkg ->
          let pkg' = self#type_expr_package pkg in
            if pkg != pkg' then Package pkg'
            else typ

  method type_expr_var_name name = name

end

class virtual unit = object (self)

  method virtual root : Root.t -> Root.t

  method virtual identifier_module :
    Identifier.module_ -> Identifier.module_

  method virtual path_module :
    Path.module_ -> Path.module_

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual signature :
    Signature.t -> Signature.t

  method unit_import import =
    let open Compilation_unit.Import in
      match import with
      | Unresolved(name, digest) ->
          let name' = self#unit_import_name name in
          let digest' = option_map self#unit_import_digest digest in
            if name != name' || digest != digest' then
              Unresolved(name', digest')
            else import
      | Resolved r ->
          let r' = self#root r in
            if r != r' then Resolved r'
            else import

  method unit_import_name name = name

  method unit_import_digest digest = digest

  method unit_source source =
    let open Compilation_unit.Source in
    let {file; build_dir; digest} = source in
    let file' = self#unit_source_file file in
    let build_dir' = self#unit_source_build_dir build_dir in
    let digest' = self#unit_source_digest digest in
      if file != file' || build_dir != build_dir' || digest != digest' then
        {file = file'; build_dir = build_dir'; digest = digest'}
      else source

  method unit_source_file file = file

  method unit_source_build_dir build_dir = build_dir

  method unit_source_digest digest = digest

  method unit_packed_item item =
    let open Compilation_unit.Packed in
    let {id; path} = item in
    let id' = self#identifier_module id in
    let path' = self#path_module path in
      if id != id' || path != path' then { id = id'; path = path' }
      else item

  method unit_packed items =
    list_map self#unit_packed_item items

  method unit_content content =
    let open Compilation_unit in
      match content with
      | Module items ->
          let items' = self#signature items in
            if items != items' then Module items'
            else content
      | Pack items ->
          let items' = self#unit_packed items in
            if items' != items then Pack items'
            else content

  method unit unit =
    let open Compilation_unit in
    let {id; doc; digest; imports;
         source; interface; hidden; content; expansion} = unit
    in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let digest' = self#unit_digest digest in
    let imports' = list_map self#unit_import imports in
    let source' = option_map self#unit_source source in
    let interface' = self#unit_interface interface in
    let hidden' = self#unit_hidden hidden in
    let content' = self#unit_content content in
    let expansion' = option_map self#signature expansion in
      if id != id' || doc != doc' || digest != digest'
         || imports != imports' || source != source'
         || interface != interface' || hidden != hidden'
         || content != content' || expansion != expansion'
      then
        {id = id'; doc = doc'; digest = digest';
         imports = imports'; source = source';
         interface = interface'; hidden = hidden';
         content = content'; expansion = expansion'}
      else unit

  method unit_digest digest = digest

  method unit_interface intf = intf

  method unit_hidden hidden = hidden

end

class virtual page = object (self)

  method virtual identifier_page :
    Identifier.page -> Identifier.page

  method virtual documentation :
    Comment.docs -> Comment.docs

  method page page =
    let open Page in
    let {name; content; digest} = page in
    let name' = self#identifier_page name in
    let content' = self#documentation content in
    let digest' = self#page_digest digest in
    if name != name' || content != content' || digest != digest' then
      {name = name'; content = content'; digest = digest'}
    else
      page

  method page_digest digest = digest

end

class virtual types = object
  inherit documentation
  inherit module_
  inherit module_type
  inherit signature
  inherit include_
  inherit type_decl
  inherit extension
  inherit exception_
  inherit value
  inherit external_
  inherit class_
  inherit class_type
  inherit class_signature
  inherit method_
  inherit instance_variable
  inherit type_expr
  inherit unit
  inherit page
end
