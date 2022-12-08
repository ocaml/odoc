#if OCAML_VERSION >= (4, 14, 0)
open Types

type uid = Shape.Uid.t

let string_of_uid uid =
  match uid with
  | Shape.Uid.Compilation_unit s -> "compunit-" ^ s
  | Item { comp_unit; id } -> "def-" ^ comp_unit ^ string_of_int id
  | Predef s -> "predef-" ^ s
  | Internal -> "internal"

let of_value_description vd = Some vd.val_uid
let of_type_declaration decl = Some decl.type_uid
let of_extension_constructor ext = Some ext.ext_uid
let of_class_type_declaration cltd = Some cltd.clty_uid
let of_class_declaration cld = Some cld.cty_uid
let of_module_type_declaration mtd = Some mtd.mtd_uid

let of_shape_uid uid = uid

#else

type uid = unit

let string_of_uid () = ""

let of_value_description _vd = None
let of_type_declaration _decl = None
let of_extension_constructor _ext = None
let of_class_type_declaration _cltd = None
let of_class_declaration _cld = None
let of_module_type_declaration _mtd = None

#endif
