
open DocOckTypes
open DocOckNameEnv

class ['a] lookup = object (self)
  val env = empty

  inherit ['a] DocOckMaps.types as super

  method root x = x
  method path_type x = x
  method path_module_type x = x
  method path_module x = x
  method path_class_type x = x
  method identifier_value x = x
  method identifier_type x = x
  method identifier_module_type x = x
  method identifier_module x = x
  method identifier_method x = x
  method identifier_label x = x
  method identifier_instance_variable x = x
  method identifier_field x = x
  method identifier_extension x = x
  method identifier_exception x = x
  method identifier_constructor x = x
  method identifier_class_type x = x
  method identifier_class x = x
  method identifier x = x
  method fragment_type x = x
  method fragment_module x = x

  method reference_module x =
    lookup_module env x
  method reference_module_type x =
    lookup_module_type env x
  method reference_type x =
    lookup_type env x
  method reference_constructor x =
    lookup_constructor env x
  method reference_field x =
    lookup_field env x
  method reference_extension x =
    lookup_extension env x
  method reference_exception x =
    lookup_exception env x
  method reference_value x =
    lookup_value env x
  method reference_class x =
    lookup_class env x
  method reference_class_type x =
    lookup_class_type env x
  method reference_method x =
    lookup_method env x
  method reference_instance_variable x =
    lookup_instance_variable env x
  method reference_label x =
    lookup_label env x
  method reference_any x =
    lookup_element env x

  method super_module md = super#module_ md

  method module_ md =
    let open Module in
    let env = add_module_decl_items md.type_ env in
    let this = {< env = env >} in
      this#super_module md

  method super_module_type mty = super#module_type mty

  method module_type mty =
    let open ModuleType in
    let env =
      match mty.expr with
      | None -> env
      | Some expr -> add_module_type_expr_items expr env
    in
    let this = {< env = env >} in
      this#super_module_type mty

  method super_unit unt = super#unit unt

  method unit unt =
    let open Unit in
    let env =
      match unt.content with
      | Module items -> add_signature_items items env
      | Pack _ -> env
    in
    let this = {< env = env >} in
      this#super_unit unt

  method super_class cl = super#class_ cl

  method class_ cl =
    let open Class in
    let env = add_class_decl_items cl.type_ env in
    let this = {< env = env >} in
      this#super_class cl

  method super_class_type cltyp = super#class_type cltyp

  method class_type cltyp =
    let open ClassType in
    let env = add_class_type_expr_items cltyp.expr env in
    let this = {< env = env >} in
      this#super_class_type cltyp

end

let lookup x =
  let obj = new lookup in
  obj#unit x
