(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open DocOckPaths.Identifier

module StringTbl = Map.Make(String)

type 'a t =
  { modules : 'a module_ Ident.tbl;
    module_types : 'a module_type Ident.tbl;
    classes : 'a class_ Ident.tbl;
    class_types : 'a class_type Ident.tbl;
    types : 'a type_ Ident.tbl;
    values : 'a value Ident.tbl;
    fields : 'a field Ident.tbl;
    constructors : 'a constructor Ident.tbl;
    methods : 'a method_ StringTbl.t;
    instance_variables : 'a instance_variable StringTbl.t;
    elements : 'a any StringTbl.t; }

let empty =
  { modules = Ident.empty;
    module_types = Ident.empty;
    classes = Ident.empty;
    class_types = Ident.empty;
    types = Ident.empty;
    values = Ident.empty;
    fields = Ident.empty;
    constructors = Ident.empty;
    methods = StringTbl.empty;
    instance_variables = StringTbl.empty;
    elements = StringTbl.empty; }

let add_module parent env id =
  let name = Ident.name id in
  let identifier = Module(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             modules = Ident.add id identifier env.modules }

let add_argument parent arg env id =
  let name = Ident.name id in
  let identifier = Argument(parent, arg, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             modules = Ident.add id identifier env.modules }

let add_module_type parent env id =
  let name = Ident.name id in
  let identifier = ModuleType(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             module_types = Ident.add id identifier env.module_types }

let add_class parent env id =
  let name = Ident.name id in
  let identifier = Class(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             classes = Ident.add id identifier env.classes }

let add_class_type parent env id =
  let name = Ident.name id in
  let identifier = ClassType(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             class_types = Ident.add id identifier env.class_types }

let add_type parent env id =
  let name = Ident.name id in
  let identifier = Type(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             types = Ident.add id identifier env.types }

let add_value parent env id =
  let name = Ident.name id in
  let identifier = Value(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             values = Ident.add id identifier env.values }

let add_field parent env id =
  let name = Ident.name id in
  let identifier = Field(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             fields = Ident.add id identifier env.fields }

let add_constructor parent env id =
  let name = Ident.name id in
  let identifier = Constructor(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             constructors = Ident.add id identifier env.constructors }

let add_method parent env name =
  let identifier = Method(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             methods = StringTbl.add name identifier env.methods }

let add_instance_variable parent env name =
  let identifier = InstanceVariable(parent, name) in
  { env with elements = StringTbl.add name identifier env.elements;
             instance_variables = StringTbl.add name identifier env.instance_variables }

let find_module env id =
  Ident.find_same id env.modules

let find_module_type env id =
  Ident.find_same id env.module_types

let find_type env id =
  Ident.find_same id env.types

let find_class env id =
  Ident.find_same id env.classes

let find_class_type env id =
  Ident.find_same id env.class_types


let lookup_module env name =
  Ident.find_name name env.modules

let lookup_module_type env name =
  Ident.find_name name env.module_types

let lookup_type env name =
  Ident.find_name name env.types

let lookup_constructor env name =
  Ident.find_name name env.constructors

let lookup_field env name =
  Ident.find_name name env.fields

let lookup_value env name =
  Ident.find_name name env.values

let lookup_class env name =
  Ident.find_name name env.classes

let lookup_class_type env name =
  Ident.find_name name env.class_types

let lookup_method env name =
  StringTbl.find name env.methods

let lookup_instance_variable env name =
  StringTbl.find name env.instance_variables

let lookup_element env name =
  StringTbl.find name env.elements


module Path = struct

  open DocOckPaths.Path

  let read_module_ident env id =
    if Ident.persistent id then Root (Ident.name id)
    else
      try
        ident_module (find_module env id)
      with Not_found -> assert false

  let read_module_type_ident env id =
    try
      ident_module_type (find_module_type env id)
    with Not_found -> assert false

  let read_class_ident env id : 'a class_ =
    try
      ident_class (find_class env id)
    with Not_found -> assert false

  let read_class_type_ident env id : 'a class_type =
    try
      ident_class_type (find_class_type env id)
    with Not_found -> class_type_of_class (read_class_ident env id)

  let read_type_ident env id =
    try
      ident_type (find_type env id)
    with Not_found -> type_of_class_type (read_class_type_ident env id)

  let rec read_module env = function
    | Path.Pident id -> read_module_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(p, arg) -> Apply(read_module env p, read_module env arg)

  let read_module_type env = function
    | Path.Pident id -> read_module_type_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(p, arg)-> assert false

  let read_class env = function
    | Path.Pident id -> read_class_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(p, arg)-> assert false

  let read_class_type env = function
    | Path.Pident id -> read_class_type_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(p, arg)-> assert false

  let read_type env = function
    | Path.Pident id -> read_type_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(p, arg)-> assert false

end

module Fragment = struct

  open DocOckPaths.Fragment.Resolved
  open DocOckPaths.Fragment

  let rec read_module = function
    | Longident.Lident s -> Resolved (Module(Root, s))
    | Longident.Ldot(p, s) -> Dot(read_module p, s)
    | Longident.Lapply _ -> assert false

  let read_type = function
    | Longident.Lident s -> Resolved (Type(Root, s))
    | Longident.Ldot(p, s) -> Dot(read_module p, s)
    | Longident.Lapply _ -> assert false

end

module Reference = struct

  open DocOckPaths.Reference.Resolved
  open DocOckPaths.Reference

  let read_module_ident env name =
    try
      ident_module (lookup_module env name)
    with Not_found -> Root name

  let read_module_type_ident env name =
    try
      ident_module_type (lookup_module_type env name)
    with Not_found -> Root name

  let read_class_ident env name =
    try
      ident_class (lookup_class env name)
    with Not_found -> Root name

  let read_class_type_ident env name =
    try
      ident_class_type (lookup_class_type env name)
    with Not_found -> class_type_of_class (read_class_ident env name)

  let read_type_ident env name =
    try
      ident_type (lookup_type env name)
    with Not_found -> type_of_class_type (read_class_type_ident env name)


  let read_constructor_ident env name =
    try
      ident_constructor (lookup_constructor env name)
    with Not_found -> Root name

  let read_field_ident env name =
    try
      ident_field (lookup_field env name)
    with Not_found -> Root name

  let read_value_ident env name =
    try
      ident_value (lookup_value env name)
    with Not_found -> Root name

  let read_method_ident env name =
    try
      ident_method (lookup_method env name)
    with Not_found -> Root name

  let read_instance_variable_ident env name =
    try
      ident_instance_variable (lookup_instance_variable env name)
    with Not_found -> Root name

  let read_element_ident env name =
    try
      Resolved (Identifier (lookup_element env name))
    with Not_found -> Root name

  let rec read_module_longident env = function
    | Longident.Lident s -> read_module_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_module env s =
    read_module_longident env (Longident.parse s)

  let read_module_type env s =
    match Longident.parse s with
    | Longident.Lident s -> read_module_type_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_class env s =
    match Longident.parse s with
    | Longident.Lident s -> read_class_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_class_type env s =
    match Longident.parse s with
    | Longident.Lident s -> read_class_type_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_type env s =
    match Longident.parse s with
    | Longident.Lident s -> read_type_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_value env s =
    match Longident.parse s with
    | Longident.Lident s -> read_value_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_field env s =
    match Longident.parse s with
    | Longident.Lident s -> read_field_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_constructor env s =
    match Longident.parse s with
    | Longident.Lident s -> read_constructor_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_method env s =
    match Longident.parse s with
    | Longident.Lident s -> read_method_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_instance_variable env s =
    match Longident.parse s with
    | Longident.Lident s -> read_instance_variable_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

  let read_element env s =
    match Longident.parse s with
    | Longident.Lident s -> read_element_ident env s
    | Longident.Ldot(lid, s) -> Dot(read_module_longident env lid, s)
    | Longident.Lapply(_, _) -> assert false

end
