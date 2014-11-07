
%parameter< Root : sig type t end >

%token ALIAS
%token ANY
%token APPLY
%token ARGUMENTS
%token ARROW
%token AUTHOR
%token BEFORE
%token BOLD
%token CENTER
%token CLASS
%token CLASS_TYPE
%token CLOSED
%token CODE
%token COMMENT
%token CONSTANT
%token CONSTRAINT
%token CONSTRUCTOR
%token DEPRECATED
%token DIGEST
%token DOC
%token DOT
%token ELEMENT
%token EMPHASIZE
%token ENUM
%token EXCEPTION
%token EXTENSIBLE
%token EXTENSION
%token EXTERNAL
%token FIELD
%token FILE
%token FIXED
%token FUNCTOR
%token IDENTIFIER
%token IMPORT
%token INCLUDE
%token INDEX
%token INHERIT
%token INSTANCE_VARIABLE
%token ITALIC
%token ITEM
%token LABEL
%token LEFT
%token LINK
%token LIST
%token METHOD
%token MODULE
%token MODULES
%token MODULE_SUBST
%token MODULE_TYPE
%token MUTABLE
%token NAME
%token NEG
%token NEWLINE
%token OBJECT
%token OPEN
%token OPTIONAL
%token PACKAGE
%token PARAM
%token PATH
%token POLY
%token POLY_VARIANT
%token POS
%token PRECODE
%token PRIMITIVE
%token PRIVATE
%token RAISE
%token RECORD
%token REFERENCE
%token RESOLVED
%token RESULT
%token RETURN
%token RIGHT
%token ROOT
%token SECTION
%token SEE
%token SIGNATURE
%token SINCE
%token SPECIAL
%token STOP
%token SUBSCRIPT
%token SUPERSCRIPT
%token TAG
%token TUPLE
%token TYPE
%token TYPEOF
%token TYPE_SUBST
%token UNIT
%token URL
%token VALUE
%token VAR
%token VARIANT
%token VERBATIM
%token VERSION
%token VIRTUAL
%token WITH


%token <int option> Argument
%token <string> Custom
%token <string option> Target
%token <int> Title


%token DTD
%token CLOSE
%token <string> Data
%token <Root.t> Base
%token EOF

%start <Root.t DocOckTypes.Unit.t> unit
%start <Root.t DocOckTypes.Unit.t> file

%%

name:
  | NAME data = Data CLOSE
      { data }

flag(X):
  | (* empty *)
      { false }
  | X CLOSE
      { true }

module_identifier:
  | base = Base
      { DocOckPaths.Identifier.Root base }
  | MODULE sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Module(sg, data) }
  | pos = Argument sg = signature_identifier data = Data CLOSE
      { match pos with
        | None -> raise Error
        | Some pos -> DocOckPaths.Identifier.Argument(sg, pos, data) }

module_type_identifier:
  | MODULE_TYPE sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.ModuleType(sg, data) }

signature_identifier:
  | md = module_identifier
      { DocOckPaths.Identifier.module_signature md }
  | mty = module_type_identifier
      { DocOckPaths.Identifier.module_type_signature mty }

type_identifier:
  | TYPE sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Type(sg, data) }

constructor_identifier:
  | CONSTRUCTOR sg = type_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Constructor(sg, data) }

field_identifier:
  | FIELD sg = type_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Field(sg, data) }

extension_identifier:
  | EXTENSION sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Extension(sg, data) }

exception_identifier:
  | EXCEPTION sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Exception(sg, data) }

value_identifier:
  | VALUE sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Value(sg, data) }

class_identifier:
  | CLASS sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Class(sg, data) }

class_type_identifier:
  | CLASS_TYPE sg = signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.ClassType(sg, data) }

class_signature_identifier:
  | cl = class_identifier
      { DocOckPaths.Identifier.class_signature cl }
  | clty = class_type_identifier
      { DocOckPaths.Identifier.class_type_signature clty }

method_identifier:
  | METHOD sg = class_signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Method(sg, data) }

instance_variable_identifier:
  | INSTANCE_VARIABLE sg = class_signature_identifier data = Data CLOSE
      { DocOckPaths.Identifier.InstanceVariable(sg, data) }

label_identifier:
  | LABEL sg = container_identifier data = Data CLOSE
      { DocOckPaths.Identifier.Label(sg, data) }

container_identifier:
  | sg = signature_identifier
    { DocOckPaths.Identifier.container_of_signature sg }
  | csig = class_signature_identifier
    { DocOckPaths.Identifier.container_of_class_signature csig }

module_resolved_path:
  | IDENTIFIER id = module_identifier CLOSE
      { DocOckPaths.Path.Resolved.ident_module id }
  | MODULE md = module_resolved_path data = Data CLOSE
      { DocOckPaths.Path.Resolved.Module(md, data) }
  | APPLY md = module_resolved_path arg = module_path CLOSE
      { DocOckPaths.Path.Resolved.Apply(md, arg) }

module_type_resolved_path:
  | IDENTIFIER id = module_type_identifier CLOSE
      { DocOckPaths.Path.Resolved.ident_module_type id }
  | MODULE_TYPE md = module_resolved_path data = Data CLOSE
      { DocOckPaths.Path.Resolved.ModuleType(md, data) }

type_resolved_path:
  | IDENTIFIER id = type_identifier CLOSE
      { DocOckPaths.Path.Resolved.ident_type id }
  | TYPE md = module_resolved_path data = Data CLOSE
      { DocOckPaths.Path.Resolved.Type(md, data) }
  | cltyp = class_type_resolved_path
      { DocOckPaths.Path.Resolved.type_of_class_type cltyp }

class_resolved_path:
  | IDENTIFIER id = class_identifier CLOSE
      { DocOckPaths.Path.Resolved.ident_class id }
  | CLASS md = module_resolved_path data = Data CLOSE
      { DocOckPaths.Path.Resolved.Class(md, data) }

class_type_resolved_path:
  | IDENTIFIER id = class_type_identifier CLOSE
      { DocOckPaths.Path.Resolved.ident_class_type id }
  | CLASS_TYPE md = module_resolved_path data = Data CLOSE
      { DocOckPaths.Path.Resolved.ClassType(md, data) }
  | cl = class_resolved_path
      { DocOckPaths.Path.Resolved.class_type_of_class cl }

module_path:
  | RESOLVED path = module_resolved_path CLOSE
      { DocOckPaths.Path.Resolved path }
  | ROOT data = Data CLOSE
      { DocOckPaths.Path.Root data }
  | DOT md = module_path data = Data CLOSE
      { DocOckPaths.Path.Dot(md, data) }
  | APPLY md = module_path arg = module_path CLOSE
      { DocOckPaths.Path.Apply(md, arg) }

module_type_path:
  | RESOLVED path = module_type_resolved_path CLOSE
      { DocOckPaths.Path.Resolved path }
  | DOT md = module_path data = Data CLOSE
      { DocOckPaths.Path.Dot(md, data) }

type_path:
  | RESOLVED path = type_resolved_path CLOSE
      { DocOckPaths.Path.Resolved path }
  | DOT md = module_path data = Data CLOSE
      { DocOckPaths.Path.Dot(md, data) }

class_type_path:
  | RESOLVED path = class_type_resolved_path CLOSE
      { DocOckPaths.Path.Resolved path }
  | DOT md = module_path data = Data CLOSE
      { DocOckPaths.Path.Dot(md, data) }

module_resolved_fragment:
  | MODULE md = signature_resolved_fragment data = Data CLOSE
      { DocOckPaths.Fragment.Resolved.Module(md, data) }

type_resolved_fragment:
  | TYPE md = signature_resolved_fragment data = Data CLOSE
      { DocOckPaths.Fragment.Resolved.Type(md, data) }
  | CLASS md = signature_resolved_fragment data = Data CLOSE
      { DocOckPaths.Fragment.Resolved.Class(md, data) }
  | CLASS_TYPE md = signature_resolved_fragment data = Data CLOSE
      { DocOckPaths.Fragment.Resolved.ClassType(md, data) }

signature_resolved_fragment:
  | ROOT CLOSE
      { DocOckPaths.Fragment.Resolved.Root }
  | md = module_resolved_fragment
      { DocOckPaths.Fragment.Resolved.module_signature md }

module_fragment:
  | RESOLVED frag = module_resolved_fragment CLOSE
      { DocOckPaths.Fragment.Resolved frag }
  | DOT md = module_fragment data = Data CLOSE
      { DocOckPaths.Fragment.Dot(md, data) }

type_fragment:
  | RESOLVED frag = type_resolved_fragment CLOSE
      { DocOckPaths.Fragment.Resolved frag }
  | DOT md = module_fragment data = Data CLOSE
      { DocOckPaths.Fragment.Dot(md, data) }

module_resolved_reference:
  | IDENTIFIER id = module_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_module id }
  | MODULE sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Module(sg, data) }

module_type_resolved_reference:
  | IDENTIFIER id = module_type_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_module_type id }
  | MODULE_TYPE sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.ModuleType(sg, data) }

signature_resolved_reference:
  | md = module_resolved_reference
      { DocOckPaths.Reference.Resolved.module_signature md }
  | mty = module_type_resolved_reference
      { DocOckPaths.Reference.Resolved.module_type_signature mty }

datatype_resolved_reference:
  | IDENTIFIER id = type_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_datatype id }
  | TYPE sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Type(sg, data) }

type_resolved_reference:
  | typ = datatype_resolved_reference
      { DocOckPaths.Reference.Resolved.type_of_datatype typ }
  | cltyp = class_type_resolved_reference
      { DocOckPaths.Reference.Resolved.type_of_class_type cltyp }

constructor_resolved_reference:
  | IDENTIFIER id = constructor_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_constructor id }
  | CONSTRUCTOR sg = datatype_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Constructor(sg, data) }
  | ext = extension_resolved_reference
      { DocOckPaths.Reference.Resolved.constructor_of_extension ext }

field_resolved_reference:
  | IDENTIFIER id = field_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_field id }
  | FIELD sg = datatype_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Field(sg, data) }

extension_resolved_reference:
  | IDENTIFIER id = extension_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_extension id }
  | EXTENSION sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Extension(sg, data) }
  | exn = exception_resolved_reference
      { DocOckPaths.Reference.Resolved.extension_of_exception exn}

exception_resolved_reference:
  | IDENTIFIER id = exception_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_exception id }
  | EXCEPTION sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Exception(sg, data) }

value_resolved_reference:
  | IDENTIFIER id = value_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_value id }
  | VALUE sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Value(sg, data) }

class_resolved_reference:
  | IDENTIFIER id = class_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_class id }
  | CLASS sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Class(sg, data) }

class_type_resolved_reference:
  | IDENTIFIER id = class_type_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_class_type id }
  | CLASS_TYPE sg = signature_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.ClassType(sg, data) }
  | cl = class_resolved_reference
      { DocOckPaths.Reference.Resolved.class_type_of_class cl }

method_resolved_reference:
  | IDENTIFIER id = method_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_method id }
  | METHOD sg = class_type_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Method(sg, data) }

instance_variable_resolved_reference:
  | IDENTIFIER id = instance_variable_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_instance_variable id }
  | INSTANCE_VARIABLE sg = class_type_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.InstanceVariable(sg, data) }

label_resolved_reference:
  | IDENTIFIER id = label_identifier CLOSE
      { DocOckPaths.Reference.Resolved.ident_label id }
  | LABEL sg = container_resolved_reference data = Data CLOSE
      { DocOckPaths.Reference.Resolved.Label(sg, data) }

container_resolved_reference:
  | c = signature_resolved_reference
      { DocOckPaths.Reference.Resolved.container_of_signature c }
  | c = class_type_resolved_reference
      { DocOckPaths.Reference.Resolved.container_of_class_signature c }

parent_resolved_reference:
  | sg = signature_resolved_reference
      { DocOckPaths.Reference.Resolved.parent_of_signature sg }
  | csig = class_type_resolved_reference
      { DocOckPaths.Reference.Resolved.parent_of_class_signature csig }
  | t = datatype_resolved_reference
      { DocOckPaths.Reference.Resolved.parent_of_datatype t }

module_reference:
  | RESOLVED rf = module_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

module_type_reference:
  | RESOLVED rf = module_type_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

type_reference:
  | RESOLVED rf = type_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

constructor_reference:
  | RESOLVED rf = constructor_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

field_reference:
  | RESOLVED rf = field_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

extension_reference:
  | RESOLVED rf = extension_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

exception_reference:
  | RESOLVED rf = exception_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

value_reference:
  | RESOLVED rf = value_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

class_reference:
  | RESOLVED rf = class_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

class_type_reference:
  | RESOLVED rf = class_type_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

method_reference:
  | RESOLVED rf = method_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

instance_variable_reference:
  | RESOLVED rf = instance_variable_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

label_reference:
  | RESOLVED rf = label_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

parent_reference:
  | RESOLVED rf = parent_resolved_reference CLOSE
      { DocOckPaths.Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

element_reference:
  | ROOT data = Data CLOSE
      { DocOckPaths.Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { DocOckPaths.Reference.Dot(p, data) }

reference:
  | MODULE rf = module_reference CLOSE
      { DocOckTypes.Documentation.Module rf }
  | MODULE_TYPE rf = module_type_reference CLOSE
      { DocOckTypes.Documentation.ModuleType rf }
  | TYPE rf = type_reference CLOSE
      { DocOckTypes.Documentation.Type rf }
  | CONSTRUCTOR rf = constructor_reference CLOSE
      { DocOckTypes.Documentation.Constructor rf }
  | FIELD rf = field_reference CLOSE
      { DocOckTypes.Documentation.Field rf }
  | EXTENSION rf = extension_reference CLOSE
      { DocOckTypes.Documentation.Extension rf }
  | EXCEPTION rf = exception_reference CLOSE
      { DocOckTypes.Documentation.Exception rf }
  | VALUE rf = value_reference CLOSE
      { DocOckTypes.Documentation.Value rf }
  | CLASS rf = class_reference CLOSE
      { DocOckTypes.Documentation.Class rf }
  | CLASS_TYPE rf = class_type_reference CLOSE
      { DocOckTypes.Documentation.ClassType rf }
  | METHOD rf = method_reference CLOSE
      { DocOckTypes.Documentation.Method rf }
  | INSTANCE_VARIABLE rf = instance_variable_reference CLOSE
      { DocOckTypes.Documentation.InstanceVariable rf }
  | ELEMENT rf = element_reference CLOSE
      { DocOckTypes.Documentation.Element rf }
  | SECTION rf = label_reference CLOSE
      { DocOckTypes.Documentation.Section rf }
  | LINK data = Data CLOSE
      { DocOckTypes.Documentation.Link data }
  | tag = Custom data = Data CLOSE
      { DocOckTypes.Documentation.Custom(tag, data) }

special:
  | MODULES modules = module_reference* CLOSE
      { DocOckTypes.Documentation.Modules modules }
  | INDEX CLOSE
      { DocOckTypes.Documentation.Index }

item:
  | ITEM text = text CLOSE
      { text }

text_element:
  | data = Data
      { DocOckTypes.Documentation.Raw data }
  | CODE data = Data CLOSE
      { DocOckTypes.Documentation.Code data }
  | PRECODE data = Data CLOSE
      { DocOckTypes.Documentation.PreCode data }
  | VERBATIM data = Data CLOSE
      { DocOckTypes.Documentation.Verbatim data }
  | BOLD text = text CLOSE
      { DocOckTypes.Documentation.(Style(Bold, text)) }
  | ITALIC text = text CLOSE
      { DocOckTypes.Documentation.(Style(Italic, text)) }
  | EMPHASIZE text = text CLOSE
      { DocOckTypes.Documentation.(Style(Emphasize, text)) }
  | CENTER text = text CLOSE
      { DocOckTypes.Documentation.(Style(Center, text)) }
  | LEFT text = text CLOSE
      { DocOckTypes.Documentation.(Style(Left, text)) }
  | RIGHT text = text CLOSE
      { DocOckTypes.Documentation.(Style(Right, text)) }
  | SUPERSCRIPT text = text CLOSE
      { DocOckTypes.Documentation.(Style(Superscript, text)) }
  | SUBSCRIPT text = text CLOSE
      { DocOckTypes.Documentation.(Style(Subscript, text)) }
  | tag = Custom text = text CLOSE
      { DocOckTypes.Documentation.(Style(Custom tag, text)) }
  | LIST i = item+ CLOSE
      { DocOckTypes.Documentation.List i }
  | ENUM i = item+ CLOSE
      { DocOckTypes.Documentation.Enum i }
  | NEWLINE CLOSE
      { DocOckTypes.Documentation.Newline }
  | level = Title label = label_identifier? text = text CLOSE
      { DocOckTypes.Documentation.Title(level, label, text) }
  | REFERENCE rf = reference text = opttext CLOSE
      { DocOckTypes.Documentation.Reference(rf, text) }
  | target = Target data = Data CLOSE
      { DocOckTypes.Documentation.Target(target, data) }
  | SPECIAL special = special CLOSE
      { DocOckTypes.Documentation.Special special }

%inline opttext:
  | (* empty *)
      { None }
  | elems = text_element+
      { Some elems }

%inline text:
  | elems = text_element*
      { elems }

see:
  | URL data = Data CLOSE
      { DocOckTypes.Documentation.Url data }
  | FILE data = Data CLOSE
      { DocOckTypes.Documentation.File data }
  | DOC data = Data CLOSE
      { DocOckTypes.Documentation.Doc data }

tag:
  | AUTHOR data = Data CLOSE
      { DocOckTypes.Documentation.Author data }
  | VERSION data = Data CLOSE
      { DocOckTypes.Documentation.Version data }
  | SEE see = see text = text CLOSE
      { DocOckTypes.Documentation.See(see, text) }
  | SINCE data = Data CLOSE
      { DocOckTypes.Documentation.Since data }
  | BEFORE name = name text = text CLOSE
      { DocOckTypes.Documentation.Before(name, text) }
  | DEPRECATED text = text CLOSE
      { DocOckTypes.Documentation.Deprecated text }
  | PARAM name = name text = text CLOSE
      { DocOckTypes.Documentation.Param(name, text) }
  | RAISE name = name text = text CLOSE
      { DocOckTypes.Documentation.Raise(name, text) }
  | RETURN text = text CLOSE
      { DocOckTypes.Documentation.Return text }
  | TAG name = name text = text CLOSE
      { DocOckTypes.Documentation.Tag(name, text) }

%inline tags:
  | tags = tag*
      { tags }

doc:
| (* empty *)
    { DocOckAttrs.empty }
| DOC text = text tags = tags CLOSE
    { DocOckTypes.Documentation.{text; tags} }

comment:
  | COMMENT text = text tags = tags CLOSE
      { DocOckTypes.Documentation.(Documentation {text; tags}) }
  | STOP CLOSE
      { DocOckTypes.Documentation.Stop }


poly_variant_kind:
  | FIXED CLOSE
      { DocOckTypes.TypeExpr.Variant.Fixed }
  | CLOSED names = name* CLOSE
      { DocOckTypes.TypeExpr.Variant.Closed names }
  | OPEN CLOSE
      { DocOckTypes.TypeExpr.Variant.Open }

poly_variant_element:
  | TYPE expr = type_expr CLOSE
      { DocOckTypes.TypeExpr.Variant.Type expr }
  | CONSTRUCTOR data = Data constant = flag(CONSTANT) types = type_expr* CLOSE
      { DocOckTypes.TypeExpr.Variant.Constructor(data, constant, types) }

poly_variant:
  | kind = poly_variant_kind elements = poly_variant_element*
      { DocOckTypes.TypeExpr.Variant.{kind; elements} }

object_method:
  | name = name type_ = type_expr
      { DocOckTypes.TypeExpr.Object.{name; type_} }

object_:
  | methods = object_method* open_ = flag(OPEN)
      { DocOckTypes.TypeExpr.Object.{methods; open_} }

package_substitution:
  | frag = type_fragment expr = type_expr
      { (frag, expr) }

package:
  | path = module_type_path substitutions = package_substitution+
      { DocOckTypes.TypeExpr.Package.{path; substitutions} }

argument_label:
  | LABEL data = Data CLOSE
      { DocOckTypes.TypeExpr.Label data }
  | OPTIONAL data = Data CLOSE
      { DocOckTypes.TypeExpr.Optional data }

type_expr:
  | VAR data = Data CLOSE
      { DocOckTypes.TypeExpr.Var data }
  | ANY CLOSE
      { DocOckTypes.TypeExpr.Any }
  | ALIAS expr = type_expr data = Data CLOSE
      { DocOckTypes.TypeExpr.Alias(expr, data) }
  | ARROW lbl = argument_label? arg = type_expr res = type_expr CLOSE
      { DocOckTypes.TypeExpr.Arrow(lbl, arg, res) }
  | TUPLE types = type_expr+ CLOSE
      { DocOckTypes.TypeExpr.Tuple types }
  | PATH p = type_path params = type_expr* CLOSE
      { DocOckTypes.TypeExpr.Constr(p, params) }
  | POLY_VARIANT v = poly_variant CLOSE
      { DocOckTypes.TypeExpr.Variant v }
  | OBJECT o = object_ CLOSE
      { DocOckTypes.TypeExpr.Object o }
  | CLASS p = class_type_path params = type_expr* CLOSE
      { DocOckTypes.TypeExpr.Class(p, params) }
  | POLY names = name+ expr = type_expr CLOSE
      { DocOckTypes.TypeExpr.Poly(names, expr) }
  | PACKAGE pkg = package CLOSE
      { DocOckTypes.TypeExpr.Package pkg }

external_primitive:
  | PRIMITIVE data = Data CLOSE
      { data }

constructor_arguments:
  | (* empty *)
      { [] }
  | ARGUMENTS types = type_expr* CLOSE
      { types }

constructor_result:
  | (* empty *)
      { None }
  | RESULT type_ = type_expr CLOSE
      { Some type_ }

constructor:
  | CONSTRUCTOR id = constructor_identifier doc = doc
      args = constructor_arguments res = constructor_result CLOSE
        { DocOckTypes.TypeDecl.Constructor.{id; doc; args; res} }

field:
  | FIELD id = field_identifier doc = doc type_ = type_expr CLOSE
      { DocOckTypes.TypeDecl.Field.{id; doc; type_} }

type_kind:
  | VARIANT constructors = constructor+ CLOSE
      { DocOckTypes.TypeDecl.Variant constructors }
  | RECORD fields = field+ CLOSE
      { DocOckTypes.TypeDecl.Record fields }
  | EXTENSIBLE CLOSE
      { DocOckTypes.TypeDecl.Extensible }

variance:
  | POS CLOSE
      { DocOckTypes.TypeDecl.Pos }
  | NEG CLOSE
      { DocOckTypes.TypeDecl.Neg }

type_parameter:
  | PARAM v = variance? CLOSE
      { (DocOckTypes.TypeDecl.Any, v) }
  | PARAM name = Data v = variance? CLOSE
      { (DocOckTypes.TypeDecl.Var name, v) }

type_constraint:
  | CONSTRAINT expr1 = type_expr expr2 = type_expr CLOSE
      { (expr1, expr2) }

type_equation:
  | params = type_parameter* private_ = flag(PRIVATE) manifest = type_expr
      { DocOckTypes.TypeDecl.Equation.{params; private_; manifest} }

extension_constructor:
  | CONSTRUCTOR id = extension_identifier doc = doc
      args = constructor_arguments res = constructor_result CLOSE
        { DocOckTypes.Extension.Constructor.{id; doc; args; res} }

class_decl:
  | clty = class_type_expr
      { DocOckTypes.Class.ClassType clty }
  | ARROW lbl = argument_label? arg = type_expr res = class_decl CLOSE
      { DocOckTypes.Class.Arrow(lbl, arg, res) }

class_type_expr:
  | PATH p = class_type_path params = type_expr* CLOSE
      { DocOckTypes.ClassType.Constr(p, params) }
  | SIGNATURE self = type_expr? items = class_signature_item* CLOSE
      { let sg = DocOckTypes.ClassSignature.{self; items} in
          DocOckTypes.ClassType.Signature sg }

class_signature_item:
  | INSTANCE_VARIABLE id = instance_variable_identifier doc = doc
      mutable_ = flag(MUTABLE) virtual_ = flag(VIRTUAL) type_ = type_expr CLOSE
        { let open DocOckTypes.ClassSignature in
          let open DocOckTypes.InstanceVariable in
            InstanceVariable {id;doc;mutable_;virtual_;type_} }
  | METHOD id = method_identifier doc = doc private_ = flag(PRIVATE)
      virtual_ = flag(VIRTUAL) type_ = type_expr CLOSE
        { let open DocOckTypes.ClassSignature in
          let open DocOckTypes.Method in
            Method {id;doc;private_;virtual_;type_} }
  | CONSTRAINT expr1 = type_expr expr2 = type_expr CLOSE
      { DocOckTypes.ClassSignature.Constraint(expr1, expr2) }
  | INHERIT csig = class_type_expr CLOSE
      { DocOckTypes.ClassSignature.Inherit csig }
  | comment = comment
      { DocOckTypes.ClassSignature.Comment comment }

module_decl:
  | ALIAS p = module_path CLOSE
      { DocOckTypes.Module.Alias p }
  | TYPE expr = module_type_expr CLOSE
      { DocOckTypes.Module.ModuleType expr }

substitution:
  | MODULE frag = module_fragment eq = module_decl CLOSE
      { DocOckTypes.ModuleType.ModuleEq(frag, eq) }
  | MODULE_SUBST frag = module_fragment p = module_path CLOSE
      { DocOckTypes.ModuleType.ModuleSubst(frag, p) }
  | TYPE frag = type_fragment eq = type_equation CLOSE
      { DocOckTypes.ModuleType.TypeEq(frag, eq) }
  | TYPE_SUBST frag = type_fragment expr = type_expr CLOSE
      { DocOckTypes.ModuleType.TypeSubst(frag, expr) }

module_argument:
  | Argument name = name expr = module_type_expr CLOSE
      { Some(name, expr) }
  | Argument CLOSE
      { None }

module_type_expr:
  | p = module_type_path
      { DocOckTypes.ModuleType.Ident p }
  | SIGNATURE sg = signature_item* CLOSE
      { DocOckTypes.ModuleType.Signature sg }
  | FUNCTOR args = module_argument+ expr = module_type_expr CLOSE
      { List.fold_right
          (fun s e -> DocOckTypes.ModuleType.Functor(s, e))
          args expr }
  | WITH expr = module_type_expr substs = substitution+ CLOSE
      { DocOckTypes.ModuleType.With(expr, substs) }
  | TYPEOF md = module_decl CLOSE
      { DocOckTypes.ModuleType.TypeOf md }

signature_item:
  | VALUE id = value_identifier doc = doc type_ = type_expr CLOSE
      { let open DocOckTypes.Signature in
        let open DocOckTypes.Value in
          Value {id;doc;type_} }
  | EXTERNAL id = value_identifier doc = doc type_ = type_expr
      primitives = external_primitive+ CLOSE
        { let open DocOckTypes.Signature in
          let open DocOckTypes.External in
            External {id; doc; type_; primitives} }
  | TYPE id = type_identifier doc = doc params = type_parameter*
      private_ = flag(PRIVATE) manifest = type_expr?
        constraints = type_constraint* kind = type_kind? CLOSE
          { let open DocOckTypes.Signature in
            let open DocOckTypes.TypeDecl in
              Type {id; doc; params; private_; manifest; constraints; kind} }
  | EXTENSION type_path = type_path doc = doc type_params = type_parameter*
      private_ = flag(PRIVATE) constructors = extension_constructor+ CLOSE
        { let open DocOckTypes.Signature in
          let open DocOckTypes.Extension in
            TypExt {type_path; doc; type_params; private_; constructors} }
  | EXCEPTION id = exception_identifier doc = doc
      args = constructor_arguments res = constructor_result CLOSE
        { let open DocOckTypes.Signature in
          let open DocOckTypes.Exception in
            Exception {id; doc; args; res} }
  | CLASS id = class_identifier doc = doc params = type_parameter*
      virtual_ = flag(VIRTUAL) type_ = class_decl CLOSE
        { let open DocOckTypes.Signature in
          let open DocOckTypes.Class in
            Class {id; doc; virtual_; params; type_} }
  | CLASS_TYPE id = class_type_identifier doc = doc params = type_parameter*
      virtual_ = flag(VIRTUAL) expr = class_type_expr CLOSE
        { let open DocOckTypes.Signature in
          let open DocOckTypes.ClassType in
            ClassType {id; doc; virtual_; params; expr} }
  | MODULE id = module_identifier doc = doc type_ = module_decl CLOSE
      { let open DocOckTypes.Signature in
        let open DocOckTypes.Module in
           Module {id; doc; type_} }
  | MODULE_TYPE id = module_type_identifier doc = doc
      expr = module_type_expr? CLOSE
      { let open DocOckTypes.Signature in
        let open DocOckTypes.ModuleType in
          ModuleType {id; doc; expr} }
  | INCLUDE expr = module_type_expr CLOSE
      { DocOckTypes.Signature.Include expr }
  | comment = comment
      { DocOckTypes.Signature.Comment comment }

unit_digest:
  | DIGEST data = Data CLOSE
      { try
          Digest.from_hex data
        with Invalid_argument _ -> raise Error }

unit_import:
  | IMPORT data = Data digest = unit_digest? CLOSE
      { DocOckTypes.Unit.Unresolved(data, digest) }
  | IMPORT base = Base CLOSE
      { DocOckTypes.Unit.Resolved base }

unit:
  | UNIT id = module_identifier digest = unit_digest imports = unit_import*
      doc = doc items = signature_item* CLOSE
        { let open DocOckTypes.Unit in
          let open DocOckTypes.ModuleType in
          let open DocOckTypes.Module in
          let module_ = {id; doc; type_ = ModuleType (Signature items)} in
            {module_; digest; imports} }

file:
  | DTD unit = unit EOF
      { unit }
