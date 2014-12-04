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

type 'a printer = Xmlm.output -> 'a -> unit

let build p = p

let open_attr attrs tag output =
  Xmlm.output output (`El_start (tag, attrs))

(* Terminals *)
let alias_t output =
  Xmlm.output output (`El_start (("", "alias"), []))

let any_t output =
  Xmlm.output output (`El_start (("", "any"), []))

let apply_t output =
  Xmlm.output output (`El_start (("", "apply"), []))

let arguments_t output =
  Xmlm.output output (`El_start (("", "arguments"), []))

let arrow_t output =
  Xmlm.output output (`El_start (("", "arrow"), []))

let author_t output =
  Xmlm.output output (`El_start (("", "author"), []))

let base_t output =
  Xmlm.output output (`El_start (("", "base"), []))

let before_t output =
  Xmlm.output output (`El_start (("", "before"), []))

let bold_t output =
  Xmlm.output output (`El_start (("", "bold"), []))

let center_t output =
  Xmlm.output output (`El_start (("", "center"), []))

let class_t output =
  Xmlm.output output (`El_start (("", "class"), []))

let class_type_t output =
  Xmlm.output output (`El_start (("", "class_type"), []))

let closed_t output =
  Xmlm.output output (`El_start (("", "closed"), []))

let code_t output =
  Xmlm.output output (`El_start (("", "code"), []))

let comment_t output =
  Xmlm.output output (`El_start (("", "comment"), []))

let constant_t output =
  Xmlm.output output (`El_start (("", "constant"), []))

let constraint_t output =
  Xmlm.output output (`El_start (("", "constraint"), []))

let constructor_t output =
  Xmlm.output output (`El_start (("", "constructor"), []))

let deprecated_t output =
  Xmlm.output output (`El_start (("", "deprecated"), []))

let digest_t output =
  Xmlm.output output (`El_start (("", "digest"), []))

let doc_t output =
  Xmlm.output output (`El_start (("", "doc"), []))

let dot_t output =
  Xmlm.output output (`El_start (("", "dot"), []))

let element_t output =
  Xmlm.output output (`El_start (("", "element"), []))

let emphasize_t output =
  Xmlm.output output (`El_start (("", "emphasize"), []))

let enum_t output =
  Xmlm.output output (`El_start (("", "enum"), []))

let exception_t output =
  Xmlm.output output (`El_start (("", "exception"), []))

let extensible_t output =
  Xmlm.output output (`El_start (("", "extensible"), []))

let extension_t output =
  Xmlm.output output (`El_start (("", "extension"), []))

let external_t output =
  Xmlm.output output (`El_start (("", "external"), []))

let field_t output =
  Xmlm.output output (`El_start (("", "field"), []))

let file_t output =
  Xmlm.output output (`El_start (("", "file"), []))

let fixed_t output =
  Xmlm.output output (`El_start (("", "fixed"), []))

let functor_t output =
  Xmlm.output output (`El_start (("", "functor"), []))

let identifier_t output =
  Xmlm.output output (`El_start (("", "identifier"), []))

let import_t output =
  Xmlm.output output (`El_start (("", "import"), []))

let include_t output =
  Xmlm.output output (`El_start (("", "include"), []))

let index_t output =
  Xmlm.output output (`El_start (("", "index"), []))

let inherit_t output =
  Xmlm.output output (`El_start (("", "inherit"), []))

let instance_variable_t output =
  Xmlm.output output (`El_start (("", "instance_variable"), []))

let italic_t output =
  Xmlm.output output (`El_start (("", "italic"), []))

let item_t output =
  Xmlm.output output (`El_start (("", "item"), []))

let label_t output =
  Xmlm.output output (`El_start (("", "label"), []))

let left_t output =
  Xmlm.output output (`El_start (("", "left"), []))

let link_t output =
  Xmlm.output output (`El_start (("", "link"), []))

let list_t output =
  Xmlm.output output (`El_start (("", "list"), []))

let method_t output =
  Xmlm.output output (`El_start (("", "method"), []))

let module_t output =
  Xmlm.output output (`El_start (("", "module"), []))

let modules_t output =
  Xmlm.output output (`El_start (("", "modules"), []))

let module_subst_t output =
  Xmlm.output output (`El_start (("", "module_subst"), []))

let module_type_t output =
  Xmlm.output output (`El_start (("", "module_type"), []))

let mutable_t output =
  Xmlm.output output (`El_start (("", "mutable"), []))

let name_t output =
  Xmlm.output output (`El_start (("", "name"), []))

let neg_t output =
  Xmlm.output output (`El_start (("", "neg"), []))

let newline_t output =
  Xmlm.output output (`El_start (("", "newline"), []))

let object_t output =
  Xmlm.output output (`El_start (("", "object"), []))

let open_t output =
  Xmlm.output output (`El_start (("", "open"), []))

let optional_t output =
  Xmlm.output output (`El_start (("", "optional"), []))

let package_t output =
  Xmlm.output output (`El_start (("", "package"), []))

let param_t output =
  Xmlm.output output (`El_start (("", "param"), []))

let path_t output =
  Xmlm.output output (`El_start (("", "path"), []))

let poly_t output =
  Xmlm.output output (`El_start (("", "poly"), []))

let poly_variant_t output =
  Xmlm.output output (`El_start (("", "poly_variant"), []))

let pos_t output =
  Xmlm.output output (`El_start (("", "pos"), []))

let precode_t output =
  Xmlm.output output (`El_start (("", "precode"), []))

let primitive_t output =
  Xmlm.output output (`El_start (("", "primitive"), []))

let private_t output =
  Xmlm.output output (`El_start (("", "private"), []))

let raise_t output =
  Xmlm.output output (`El_start (("", "raise"), []))

let record_t output =
  Xmlm.output output (`El_start (("", "record"), []))

let reference_t output =
  Xmlm.output output (`El_start (("", "reference"), []))

let resolved_t output =
  Xmlm.output output (`El_start (("", "resolved"), []))

let result_t output =
  Xmlm.output output (`El_start (("", "result"), []))

let return_t output =
  Xmlm.output output (`El_start (("", "return"), []))

let right_t output =
  Xmlm.output output (`El_start (("", "right"), []))

let root_t output =
  Xmlm.output output (`El_start (("", "root"), []))

let section_t output =
  Xmlm.output output (`El_start (("", "section"), []))

let see_t output =
  Xmlm.output output (`El_start (("", "see"), []))

let signature_t output =
  Xmlm.output output (`El_start (("", "signature"), []))

let since_t output =
  Xmlm.output output (`El_start (("", "since"), []))

let special_t output =
  Xmlm.output output (`El_start (("", "special"), []))

let stop_t output =
  Xmlm.output output (`El_start (("", "stop"), []))

let subscript_t output =
  Xmlm.output output (`El_start (("", "subscript"), []))

let subst_t output =
  Xmlm.output output (`El_start (("", "subst"), []))

let subst_alias_t output =
  Xmlm.output output (`El_start (("", "subst_alias"), []))

let superscript_t output =
  Xmlm.output output (`El_start (("", "superscript"), []))

let tag_t output =
  Xmlm.output output (`El_start (("", "tag"), []))

let tuple_t output =
  Xmlm.output output (`El_start (("", "tuple"), []))

let type_t output =
  Xmlm.output output (`El_start (("", "type"), []))

let typeof_t output =
  Xmlm.output output (`El_start (("", "typeof"), []))

let type_subst_t output =
  Xmlm.output output (`El_start (("", "type_subst"), []))

let unit_t output =
  Xmlm.output output (`El_start (("", "unit"), []))

let url_t output =
  Xmlm.output output (`El_start (("", "url"), []))

let value_t output =
  Xmlm.output output (`El_start (("", "value"), []))

let var_t output =
  Xmlm.output output (`El_start (("", "var"), []))

let variant_t output =
  Xmlm.output output (`El_start (("", "variant"), []))

let verbatim_t output =
  Xmlm.output output (`El_start (("", "verbatim"), []))

let version_t output =
  Xmlm.output output (`El_start (("", "version"), []))

let virtual_t output =
  Xmlm.output output (`El_start (("", "virtual"), []))

let with_t output =
  Xmlm.output output (`El_start (("", "with"), []))

(* Terminals with attributes *)

let argument_t output = function
  | None ->
      Xmlm.output output (`El_start (("", "argument"), []))
  | Some pos ->
      let attr = (("", "pos"), string_of_int pos) in
        Xmlm.output output (`El_start (("", "argument"), [attr]))

let custom_t output tag =
  let attr = (("", "tag"), tag) in
    Xmlm.output output (`El_start (("", "custom"), [attr]))

let target_t output = function
  | None ->
      Xmlm.output output (`El_start (("", "target"), []))
  | Some name ->
      let attr = (("", "pos"), name) in
        Xmlm.output output (`El_start (("", "target"), [attr]))

let title_t output level =
  let attr = (("", "level"), string_of_int level) in
    Xmlm.output output (`El_start (("", "title"), [attr]))

(* Special terminals *)

let close output =
  Xmlm.output output `El_end

let data output s =
  Xmlm.output output (`Data s)

let dtd output d =
  Xmlm.output output (`Dtd d)

(* Utilites *)

let closed t output =
  t output;
  close output

let flag t output b =
  if b then closed t output

let simple t output s =
  t output;
  data output s;
  close output

let opt p base output o =
  match o with
  | None -> ()
  | Some x -> p base output x

let rec list p base output l =
  match l with
  | [] -> ()
  | x :: xs ->
      p base output x;
      list p base output xs

(* Non-terminals *)

let name_p base output n = simple name_t output n

open DocOckPaths

let rec identifier_p: type a. _ -> _ -> (_, a) Identifier.t -> _ =
  fun base output id ->
    let open Identifier in
    let component t sg name =
      t output;
      identifier_p base output sg;
      data output name;
      close output
    in
      match id with
      | Root r ->
          base_t output;
          base output r;
          close output
      | Argument(sg, pos, name) ->
          argument_t output (Some pos);
          identifier_p base output sg;
          data output name;
          close output
      | Module(sg, name) -> component module_t sg name
      | ModuleType(sg, name) -> component module_type_t sg name
      | Type(sg, name) -> component type_t sg name
      | CoreType name -> simple type_t output name
      | Constructor(sg, name) -> component constructor_t sg name
      | Field(sg, name) -> component field_t sg name
      | Extension(sg, name) -> component extension_t sg name
      | Exception(sg, name) -> component exception_t sg name
      | CoreException name -> simple exception_t output name
      | Value(sg, name) -> component value_t sg name
      | Class(sg, name) -> component class_t sg name
      | ClassType(sg, name) -> component class_type_t sg name
      | Method(csig, name) -> component method_t csig name
      | InstanceVariable(csig, name) -> component instance_variable_t csig name
      | Label(cnt, name) -> component label_t cnt name

let rec resolved_path_p : type a. _ -> _ -> (_, a) Path.Resolved.t -> _ =
  fun base output p ->
    let component t m name =
      t output;
      resolved_path_p base output m;
      data output name;
      close output
    in
    let open Path.Resolved in
      match p with
      | Identifier id ->
          identifier_t output;
          identifier_p base output id;
          close output
      | Subst(sub, p) ->
          subst_t output;
          resolved_path_p base output sub;
          resolved_path_p base output p;
          close output
      | SubstAlias(sub, p) ->
          subst_alias_t output;
          resolved_path_p base output sub;
          resolved_path_p base output p;
          close output
      | Apply(m, arg) ->
          apply_t output;
          resolved_path_p base output m;
          path_p base output arg;
          close output
      | Module(m, name) -> component module_t m name
      | ModuleType(m, name) -> component module_type_t m name
      | Type(m, name) -> component type_t m name
      | Class(m, name) -> component class_t m name
      | ClassType(m, name) -> component class_type_t m name

and path_p : type a. _ -> _ -> (_, a) Path.t -> _ =
  fun base output p ->
    let open Path in
      match p with
      | Resolved p ->
          resolved_t output;
          resolved_path_p base output p;
          close output
      | Root name ->
          root_t output;
          data output name;
          close output
      | Dot(m, name) ->
          dot_t output;
          path_p base output m;
          data output name;
          close output
      | Apply(m, arg) ->
          apply_t output;
          path_p base output m;
          path_p base output arg;
          close output

let rec resolved_fragment_p
  : type a b. _ -> _ -> (_, a, b) Fragment.Resolved.raw -> _ =
  fun base output frag ->
    let component t m name =
      t output;
      resolved_fragment_p base output m;
      data output name;
      close output
    in
    let open Fragment.Resolved in
      match frag with
      | Root -> closed root_t output
      | Subst(sub, p) ->
          subst_t output;
          resolved_path_p base output sub;
          resolved_fragment_p base output p;
          close output
      | SubstAlias(sub, p) ->
          subst_alias_t output;
          resolved_path_p base output sub;
          resolved_fragment_p base output p;
          close output
      | Module(m, name) -> component module_t m name
      | Type(m, name) -> component type_t m name
      | Class(m, name) -> component class_t m name
      | ClassType(m, name) -> component class_type_t m name

let rec fragment_p : type a b. _ -> _ -> (_, a, b) Fragment.raw -> _ =
  fun base output frag ->
    let open Fragment in
      match frag with
      | Resolved fragment ->
          resolved_t output;
          resolved_fragment_p base output fragment;
          close output
      | Dot(m, name) ->
          dot_t output;
          fragment_p base output m;
          data output name;
          close output

let rec resolved_reference_p
  : type a. _ -> _ -> (_, a) Reference.Resolved.t -> _ =
  fun base output rf ->
    let component t sg name =
      t output;
      resolved_reference_p base output sg;
      data output name;
      close output
    in
    let open Reference.Resolved in
      match rf with
      | Identifier id ->
          identifier_t output;
          identifier_p base output id;
          close output
      | Module(sg, name) -> component module_t sg name
      | ModuleType(sg, name) -> component module_type_t sg name
      | Type(sg, name) -> component type_t sg name
      | Constructor(sg, name) -> component constructor_t sg name
      | Field(sg, name) -> component field_t sg name
      | Extension(sg, name) -> component extension_t sg name
      | Exception(sg, name) -> component exception_t sg name
      | Value(sg, name) -> component value_t sg name
      | Class(sg, name) -> component class_t sg name
      | ClassType(sg, name) -> component class_type_t sg name
      | Method(csig, name) -> component method_t csig name
      | InstanceVariable(csig, name) -> component instance_variable_t csig name
      | Label(cnt, name) -> component label_t cnt name

and reference_p : type a. _ -> _ -> (_, a) Reference.t -> _ =
  fun base output rf ->
    let open Reference in
      match rf with
      | Resolved rf ->
          resolved_t output;
          resolved_reference_p base output rf;
          close output
      | Root name -> simple root_t output name
      | Dot(m, name) ->
          dot_t output;
          reference_p base output m;
          data output name;
          close output

open DocOckTypes

let doc_reference_p base output rf =
  let open Documentation in
  let reference t rf =
    t output;
    reference_p base output rf;
    close output
  in
    match rf with
    | Module rf -> reference module_t rf
    | ModuleType rf -> reference module_type_t rf
    | Type rf -> reference type_t rf
    | Constructor rf -> reference constructor_t rf
    | Field rf -> reference field_t rf
    | Extension rf -> reference extension_t rf
    | Exception rf -> reference exception_t rf
    | Value rf -> reference value_t rf
    | Class rf -> reference class_t rf
    | ClassType rf -> reference class_type_t rf
    | Method rf -> reference method_t rf
    | InstanceVariable rf -> reference instance_variable_t rf
    | Element rf -> reference element_t rf
    | Section rf -> reference section_t rf
    | Link s -> simple link_t output s
    | Custom(tag, s) ->
        custom_t output tag;
        data output s;
        close output

let special_p base output =
  let open Documentation in function
    | Modules mds ->
        modules_t output;
        list reference_p base output mds;
        close output
    | Index -> closed index_t output

let rec item_p base output txt =
  item_t output;
  text_p base output txt;
  close output

and text_element_p base output elem =
  let open Documentation in
  let style t txt =
    t output;
    text_p base output txt;
    close output
  in
    match elem with
    | Raw s -> data output s
    | Code s -> simple code_t output s
    | PreCode s -> simple precode_t output s
    | Verbatim s -> simple verbatim_t output s
    | Style(Bold, txt) -> style bold_t txt
    | Style(Italic, txt) -> style italic_t txt
    | Style(Emphasize, txt) -> style emphasize_t txt
    | Style(Center, txt) -> style center_t txt
    | Style(Left, txt) -> style left_t txt
    | Style(Right, txt) -> style right_t txt
    | Style(Superscript, txt) -> style superscript_t txt
    | Style(Subscript, txt) -> style subscript_t txt
    | Style(Custom tag, txt) ->
        custom_t output tag;
        text_p base output txt;
        close output
    | List items ->
        list_t output;
        list item_p base output items;
        close output
    | Enum items ->
        enum_t output;
        list item_p base output items;
        close output
    | Newline ->
        newline_t output;
        close output
    | Title(level, lbl, txt) ->
        title_t output level;
        opt identifier_p base output lbl;
        text_p base output txt;
        close output
    | Reference(rf, txt) ->
        reference_t output;
        doc_reference_p base output rf;
        opt text_p base output txt;
        close output
    | Target(target, s) ->
        target_t output target;
        data output s;
        close output
    | Special srf ->
        special_t output;
        special_p base output srf;
        close output

and text_p base output txt =
  list text_element_p base output txt

and see output =
  let open Documentation in function
  | Url s -> simple url_t output s
  | File s -> simple file_t output s
  | Doc s -> simple doc_t output s

and tag_p base output tg =
  let open Documentation in
  let block t txt =
    t output;
    text_p base output txt;
    close output
  in
  let named_block t n txt =
    t output;
    name_p base output n;
    text_p base output txt;
    close output
  in
    match tg with
    | Author s -> simple author_t output s
    | Version s -> simple version_t output s
    | See(s, txt) ->
        see_t output;
        see output s;
        text_p base output txt;
        close output
    | Since s -> simple since_t output s
    | Before(name, txt) -> named_block before_t name txt
    | Deprecated txt -> block deprecated_t txt
    | Param(name, txt) -> named_block param_t name txt
    | Raise(name, txt) -> named_block raise_t name txt
    | Return txt -> block return_t txt
    | Tag(name, txt) -> named_block tag_t name txt

and tags_p base output tgs =
  list tag_p base output tgs

let doc_p base output =
  let open Documentation in function
    | {text = []; tags = []} -> ()
    | {text; tags} ->
        doc_t output;
        text_p base output text;
        tags_p base output tags;
        close output

let comment_p base output =
  let open Documentation in function
    | Documentation {text; tags} ->
        comment_t output;
        text_p base output text;
        tags_p base output tags;
        close output
    | Stop -> closed stop_t output

let rec poly_variant_kind_p base output =
  let open TypeExpr.Variant in function
    | Fixed -> closed fixed_t output
    | Closed names ->
        closed_t output;
        list name_p base output names;
        close output
    | Open -> closed open_t output

and poly_variant_element_p base output =
  let open TypeExpr.Variant in function
    | Type expr ->
        type_t output;
        type_expr_p base output expr;
        close output
    | Constructor(name, constant, types) ->
        constructor_t output;
        data output name;
        flag constant_t output constant;
        list type_expr_p base output types;
        close output

and object_method_p base output m =
  let open TypeExpr.Object in
    name_p base output m.name;
    type_expr_p base output m.type_

and package_substitution_p base output (frag, expr) =
  fragment_p base output frag;
  type_expr_p base output expr

and argument_label_p base output =
  let open TypeExpr in function
    | Label s -> simple label_t output s
    | Optional s -> simple optional_t output s

and type_expr_p base output =
  let open TypeExpr in function
    | Var name -> simple var_t output name
    | Any -> closed any_t output
    | Alias(expr, name) ->
        alias_t output;
        type_expr_p base output expr;
        data output name;
        close output
    | Arrow(lbl, arg, res) ->
        arrow_t output;
        opt argument_label_p base output lbl;
        type_expr_p base output arg;
        type_expr_p base output res;
        close output
    | Tuple types ->
        tuple_t output;
        list type_expr_p base output types;
        close output
    | Constr(p, params) ->
        path_t output;
        path_p base output p;
        list type_expr_p base output params;
        close output
    | Variant pv ->
        let open Variant in
          poly_variant_t output;
          poly_variant_kind_p base output pv.kind;
          list poly_variant_element_p base output pv.elements;
          close output
    | Object obj ->
        let open Object in
          object_t output;
          list object_method_p base output obj.methods;
          flag open_t output obj.open_;
          close output
    | Class(p, params) ->
        class_t output;
        path_p base output p;
        list type_expr_p base output params;
        close output
    | Poly(vars, expr) ->
        poly_t output;
        list name_p base output vars;
        type_expr_p base output expr;
        close output
    | Package pkg ->
        let open Package in
          package_t output;
          path_p base output pkg.path;
          list package_substitution_p base output pkg.substitutions;
          close output

let external_primitive_p base output s = simple primitive_t output s

let constructor_arguments_p base output = function
  | [] -> ()
  | types ->
      arguments_t output;
      list type_expr_p base output types;
      close output

let constructor_result_p base output = function
  | None -> ()
  | Some expr ->
      result_t output;
      type_expr_p base output expr;
      close output

let constructor_p base output cstr =
  let open TypeDecl.Constructor in
    constructor_t output;
    identifier_p base output cstr.id;
    doc_p base output cstr.doc;
    constructor_arguments_p base output cstr.args;
    constructor_result_p base output cstr.res;
    close output

let field_p base output fld =
  let open TypeDecl.Field in
    field_t output;
    identifier_p base output fld.id;
    doc_p base output fld.doc;
    type_expr_p base output fld.type_;
    close output

let type_representation_p base output =
  let open TypeDecl.Representation in function
    | Variant constructors ->
        variant_t output;
        list constructor_p base output constructors;
        close output
    | Record fields ->
        record_t output;
        list field_p base output fields;
        close output
    | Extensible -> closed extensible_t output

let variance_p base output =
  let open TypeDecl in function
    | Pos -> closed pos_t output
    | Neg -> closed neg_t output

let type_parameter_p base output =
  let open TypeDecl in function
    | (Any, v) ->
        param_t output;
        opt variance_p base output v;
        close output
    | (Var name, v) ->
        param_t output;
        data output name;
        opt variance_p base output v;
        close output

let type_subst_parameter_p base output name =
  param_t output;
  data output name;
  close output

let type_constraint_p base output (expr1, expr2) =
  constraint_t output;
  type_expr_p base output expr1;
  type_expr_p base output expr2;
  close output

let type_equation_p base output eq =
  let open TypeDecl.Equation in
    list type_parameter_p base output eq.params;
    flag private_t output eq.private_;
    opt type_expr_p base output eq.manifest;
    list type_constraint_p base output eq.constraints

let extension_constructor_p base output ext =
  let open Extension.Constructor in
    constructor_t output;
    identifier_p base output ext.id;
    doc_p base output ext.doc;
    constructor_arguments_p base output ext.args;
    constructor_result_p base output ext.res;
    close output

let rec class_decl_p base output =
  let open Class in function
    | ClassType clty -> class_type_expr_p base output clty
    | Arrow(lbl, arg, res) ->
        arrow_t output;
        opt argument_label_p base output lbl;
        type_expr_p base output arg;
        class_decl_p base output res;
        close output

and class_type_expr_p base output =
  let open ClassType in function
    | Constr(p, params) ->
        path_t output;
        path_p base output p;
        list type_expr_p base output params;
        close output
    | Signature sg ->
        let open ClassSignature in
          signature_t output;
          opt type_expr_p base output sg.self;
          list class_signature_item_p base output sg.items;
          close output

and class_signature_item_p base output =
  let open ClassSignature in function
    | InstanceVariable inst ->
        let open InstanceVariable in
          instance_variable_t output;
          identifier_p base output inst.id;
          doc_p base output inst.doc;
          flag mutable_t output inst.mutable_;
          flag virtual_t output inst.virtual_;
          type_expr_p base output inst.type_;
          close output
    | Method meth ->
        let open Method in
          method_t output;
          identifier_p base output meth.id;
          doc_p base output meth.doc;
          flag private_t output meth.private_;
          flag virtual_t output meth.virtual_;
          type_expr_p base output meth.type_;
          close output
    | Constraint(expr1, expr2) ->
        constraint_t output;
        type_expr_p base output expr1;
        type_expr_p base output expr2;
        close output
    | Inherit csig ->
        inherit_t output;
        class_type_expr_p base output csig;
        close output
    | Comment com -> comment_p base output com

let rec module_decl_p base output =
  let open Module in function
    | Alias p ->
        alias_t output;
        path_p base output p;
        close output
    | ModuleType mty ->
        type_t output;
        module_type_expr_p base output mty;
        close output

and substitution_p base output =
  let open ModuleType in function
    | ModuleEq(frag, eq) ->
        module_t output;
        fragment_p base output frag;
        module_decl_p base output eq;
        close output
    | ModuleSubst(frag, p) ->
        module_subst_t output;
        fragment_p base output frag;
        path_p base output p;
        close output
    | TypeEq(frag, eq) ->
        type_t output;
        fragment_p base output frag;
        type_equation_p base output eq;
        close output
    | TypeSubst(frag, params, p) ->
        type_subst_t output;
        fragment_p base output frag;
        list type_subst_parameter_p base output params;
        path_p base output p;
        close output

and module_argument_p base output = function
  | None ->
      argument_t output None;
      close output
  | Some(id, expr) ->
      argument_t output None;
      identifier_p base output id;
      module_type_expr_p base output expr;
      close output

and module_type_expr_p base output =
  let open ModuleType in function
    | Path p -> path_p base output p
    | Signature items ->
        signature_t output;
        list signature_item_p base output items;
        close output
    | Functor _ as mty->
        let rec loop = function
          | Functor(arg, expr) ->
              module_argument_p base output arg;
              loop expr
          | mty -> module_type_expr_p base output mty
        in
          functor_t output;
          loop mty;
          close output
    | With(expr, substs) ->
        with_t output;
        module_type_expr_p base output expr;
        list substitution_p base output substs;
        close output
    | TypeOf md ->
        typeof_t output;
        module_decl_p base output md;
        close output

and signature_item_p base output =
  let open Signature in function
    | Value v ->
        let open Value in
          value_t output;
          identifier_p base output v.id;
          doc_p base output v.doc;
          type_expr_p base output v.type_;
          close output
    | External e ->
        let open External in
          external_t output;
          identifier_p base output e.id;
          doc_p base output e.doc;
          type_expr_p base output e.type_;
          list external_primitive_p base output e.primitives;
          close output
    | Type typ ->
        let open TypeDecl in
          type_t output;
          identifier_p base output typ.id;
          doc_p base output typ.doc;
          type_equation_p base output typ.equation;
          opt type_representation_p base output typ.representation;
          close output
    | TypExt typext ->
        let open Extension in
          extension_t output;
          path_p base output typext.type_path;
          doc_p base output typext.doc;
          list type_parameter_p base output typext.type_params;
          flag private_t output typext.private_;
          list extension_constructor_p base output typext.constructors;
          close output
    | Exception exn ->
        let open Exception in
          exception_t output;
          identifier_p base output exn.id;
          doc_p base output exn.doc;
          constructor_arguments_p base output exn.args;
          constructor_result_p base output exn.res;
          close output
    | Class cls ->
        let open Class in
          class_t output;
          identifier_p base output cls.id;
          doc_p base output cls.doc;
          list type_parameter_p base output cls.params;
          flag virtual_t output cls.virtual_;
          class_decl_p base output cls.type_;
          close output
    | ClassType clty ->
        let open ClassType in
          class_type_t output;
          identifier_p base output clty.id;
          doc_p base output clty.doc;
          list type_parameter_p base output clty.params;
          flag virtual_t output clty.virtual_;
          class_type_expr_p base output clty.expr;
          close output
    | Module md ->
        let open Module in
          module_t output;
          identifier_p base output md.id;
          doc_p base output md.doc;
          module_decl_p base output md.type_;
          close output
    | ModuleType mty ->
        let open ModuleType in
          module_type_t output;
          identifier_p base output mty.id;
          doc_p base output mty.doc;
          opt module_type_expr_p base output mty.expr;
          close output
    | Include expr ->
        include_t output;
        module_type_expr_p base output expr;
        close output
    | Comment com -> comment_p base output com

let unit_digest_p base output digest =
  digest_t output;
  data output (Digest.to_hex digest);
  close output

let unit_import_p base output =
  let open Unit in function
    | Unresolved(name, digest) ->
        import_t output;
        data output name;
        opt unit_digest_p base output digest;
        close output
    | Resolved r ->
        import_t output;
        base_t output;
        base output r;
        close output;
        close output

let unit_p base output unit =
  let open Unit in
    unit_t output;
    identifier_p base output unit.id;
    unit_digest_p base output unit.digest;
    list unit_import_p base output unit.imports;
    doc_p base output unit.doc;
    list signature_item_p base output unit.items;
    close output

let file_p base output unit =
  dtd output None;
  unit_p base output unit

let unit = unit_p
let file = file_p
