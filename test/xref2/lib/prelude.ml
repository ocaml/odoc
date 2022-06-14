(* Prelude file for Mdx tests *)

#require "odoc.xref2";;
#require "odoc.xref_test";;

open Odoc_xref2;;
open Odoc_xref_test;;

(* Printers for names *)

#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ModuleTypeName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.ClassName.fmt;;
#install_printer Odoc_model.Names.ClassTypeName.fmt;;
#install_printer Odoc_model.Names.ExceptionName.fmt;;
#install_printer Odoc_model.Names.FieldName.fmt;;
#install_printer Odoc_model.Names.ConstructorName.fmt;;
#install_printer Odoc_model.Names.ExtensionName.fmt;;
#install_printer Odoc_model.Names.MethodName.fmt;;
#install_printer Odoc_model.Names.InstanceVariableName.fmt;;
#install_printer Odoc_model.Names.LabelName.fmt;;
#install_printer Odoc_model.Names.PageName.fmt;;
#install_printer Odoc_model.Names.SectionName.fmt;;
