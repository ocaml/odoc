
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
  method identifier_any x = x
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

  method super_signature sg = super#signature sg

  method signature sg =
    let env = add_signature_items sg env in
    let this = {< env = env >} in
      this#super_signature sg

  method super_class_signature sg = super#class_signature sg

  method class_signature clsig =
    let env = add_class_signature_items clsig env in
    let this = {< env = env >} in
      this#super_class_signature clsig

end

let lookup x =
  let obj = new lookup in
  obj#unit x
