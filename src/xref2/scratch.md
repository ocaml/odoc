```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_xref2.Component.Fmt.model_identifier;;
#install_printer Common.value_name_pp;;
#install_printer Common.type_name_pp;;
#install_printer Common.unit_name_pp;;
#print_length 60000;;
#print_depth 200;;

let mt_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.ModuleType.t :> Odoc_model.Paths.Identifier.t);;
#install_printer mt_printer;;
let m_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.Module.t :> Odoc_model.Paths.Identifier.t);;
#install_printer m_printer;;
let t_printer fmt t = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (t : Odoc_model.Paths.Identifier.Type.t :> Odoc_model.Paths.Identifier.t);;
#install_printer t_printer;;
let doc_printer fmt (doc : Odoc_model.Comment.docs) = Format.fprintf fmt "<<docs>>";;
#install_printer doc_printer;;
#install_printer Ident.print_with_scope;;
#install_printer Ident.print;;
```
