(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

type 'r t = { f : 'acc. ('acc -> Xmlm.signal -> 'acc) -> 'acc -> 'r -> 'acc }

let ns = DocOckXml.ns

let open_attr attrs tag output acc =
  output acc (`El_start (tag, attrs))

(* Terminals *)
let alias_t output acc =
  output acc (`El_start ((ns, "alias"), []))

let any_t output acc =
  output acc (`El_start ((ns, "any"), []))

let apply_t output acc =
  output acc (`El_start ((ns, "apply"), []))

let arguments_t output acc =
  output acc (`El_start ((ns, "arguments"), []))

let arrow_t output acc =
  output acc (`El_start ((ns, "arrow"), []))

let author_t output acc =
  output acc (`El_start ((ns, "author"), []))

let base_t output acc =
  output acc (`El_start ((ns, "base"), []))

let before_t output acc =
  output acc (`El_start ((ns, "before"), []))

let bold_t output acc =
  output acc (`El_start ((ns, "bold"), []))

let center_t output acc =
  output acc (`El_start ((ns, "center"), []))

let class_t output acc =
  output acc (`El_start ((ns, "class"), []))

let class_type_t output acc =
  output acc (`El_start ((ns, "class_type"), []))

let closed_t output acc =
  output acc (`El_start ((ns, "closed"), []))

let code_t output acc =
  output acc (`El_start ((ns, "code"), []))

let column_t output acc =
  output acc (`El_start ((ns, "column"), []))

let comment_t output acc =
  output acc (`El_start ((ns, "comment"), []))

let constant_t output acc =
  output acc (`El_start ((ns, "constant"), []))

let constraint_t output acc =
  output acc (`El_start ((ns, "constraint"), []))

let constructor_t output acc =
  output acc (`El_start ((ns, "constructor"), []))

let deprecated_t output acc =
  output acc (`El_start ((ns, "deprecated"), []))

let digest_t output acc =
  output acc (`El_start ((ns, "digest"), []))

let dir_t output acc =
  output acc (`El_start ((ns, "dir"), []))

let doc_t output acc =
  output acc (`El_start ((ns, "doc"), []))

let dot_t output acc =
  output acc (`El_start ((ns, "dot"), []))

let element_t output acc =
  output acc (`El_start ((ns, "element"), []))

let emphasize_t output acc =
  output acc (`El_start ((ns, "emphasize"), []))

let enum_t output acc =
  output acc (`El_start ((ns, "enum"), []))

let error_t output acc =
  output acc (`El_start ((ns, "error"), []))

let exception_t output acc =
  output acc (`El_start ((ns, "exception"), []))

let extensible_t output acc =
  output acc (`El_start ((ns, "extensible"), []))

let extension_t output acc =
  output acc (`El_start ((ns, "extension"), []))

let external_t output acc =
  output acc (`El_start ((ns, "external"), []))

let field_t output acc =
  output acc (`El_start ((ns, "field"), []))

let file_t output acc =
  output acc (`El_start ((ns, "file"), []))

let filename_t output acc =
  output acc (`El_start ((ns, "filename"), []))

let fixed_t output acc =
  output acc (`El_start ((ns, "fixed"), []))

let functor_t output acc =
  output acc (`El_start ((ns, "functor"), []))

let hidden_t output acc =
  output acc (`El_start ((ns, "hidden"), []))

let identifier_t output acc =
  output acc (`El_start ((ns, "identifier"), []))

let import_t output acc =
  output acc (`El_start ((ns, "import"), []))

let include_t output acc =
  output acc (`El_start ((ns, "include"), []))

let index_t output acc =
  output acc (`El_start ((ns, "index"), []))

let inherit_t output acc =
  output acc (`El_start ((ns, "inherit"), []))

let instance_variable_t output acc =
  output acc (`El_start ((ns, "instance_variable"), []))

let interface_t output acc =
  output acc (`El_start ((ns, "interface"), []))

let italic_t output acc =
  output acc (`El_start ((ns, "italic"), []))

let item_t output acc =
  output acc (`El_start ((ns, "item"), []))

let label_t output acc =
  output acc (`El_start ((ns, "label"), []))

let left_t output acc =
  output acc (`El_start ((ns, "left"), []))

let line_t output acc =
  output acc (`El_start ((ns, "line"), []))

let link_t output acc =
  output acc (`El_start ((ns, "link"), []))

let list_t output acc =
  output acc (`El_start ((ns, "list"), []))

let location_t output acc =
  output acc (`El_start ((ns, "location"), []))

let method_t output acc =
  output acc (`El_start ((ns, "method"), []))

let module_t output acc =
  output acc (`El_start ((ns, "module"), []))

let modules_t output acc =
  output acc (`El_start ((ns, "modules"), []))

let module_subst_t output acc =
  output acc (`El_start ((ns, "module_subst"), []))

let module_type_t output acc =
  output acc (`El_start ((ns, "module_type"), []))

let mutable_t output acc =
  output acc (`El_start ((ns, "mutable"), []))

let name_t output acc =
  output acc (`El_start ((ns, "name"), []))

let neg_t output acc =
  output acc (`El_start ((ns, "neg"), []))

let newline_t output acc =
  output acc (`El_start ((ns, "newline"), []))

let object_t output acc =
  output acc (`El_start ((ns, "object"), []))

let offset_t output acc =
  output acc (`El_start ((ns, "offset"), []))

let open_t output acc =
  output acc (`El_start ((ns, "open"), []))

let optional_t output acc =
  output acc (`El_start ((ns, "optional"), []))

let pack_t output acc =
  output acc (`El_start ((ns, "pack"), []))

let package_t output acc =
  output acc (`El_start ((ns, "package"), []))

let param_t output acc =
  output acc (`El_start ((ns, "param"), []))

let path_t output acc =
  output acc (`El_start ((ns, "path"), []))

let poly_t output acc =
  output acc (`El_start ((ns, "poly"), []))

let poly_variant_t output acc =
  output acc (`El_start ((ns, "poly_variant"), []))

let pos_t output acc =
  output acc (`El_start ((ns, "pos"), []))

let position_t output acc =
  output acc (`El_start ((ns, "position"), []))

let precode_t output acc =
  output acc (`El_start ((ns, "precode"), []))

let primitive_t output acc =
  output acc (`El_start ((ns, "primitive"), []))

let private_t output acc =
  output acc (`El_start ((ns, "private"), []))

let raise_t output acc =
  output acc (`El_start ((ns, "raise"), []))

let record_t output acc =
  output acc (`El_start ((ns, "record"), []))

let reference_t output acc =
  output acc (`El_start ((ns, "reference"), []))

let resolved_t output acc =
  output acc (`El_start ((ns, "resolved"), []))

let result_t output acc =
  output acc (`El_start ((ns, "result"), []))

let return_t output acc =
  output acc (`El_start ((ns, "return"), []))

let right_t output acc =
  output acc (`El_start ((ns, "right"), []))

let root_t output acc =
  output acc (`El_start ((ns, "root"), []))

let section_t output acc =
  output acc (`El_start ((ns, "section"), []))

let see_t output acc =
  output acc (`El_start ((ns, "see"), []))

let signature_t output acc =
  output acc (`El_start ((ns, "signature"), []))

let since_t output acc =
  output acc (`El_start ((ns, "since"), []))

let source_t output acc =
  output acc (`El_start ((ns, "source"), []))

let special_t output acc =
  output acc (`El_start ((ns, "special"), []))

let stop_t output acc =
  output acc (`El_start ((ns, "stop"), []))

let subscript_t output acc =
  output acc (`El_start ((ns, "subscript"), []))

let subst_t output acc =
  output acc (`El_start ((ns, "subst"), []))

let subst_alias_t output acc =
  output acc (`El_start ((ns, "subst_alias"), []))

let superscript_t output acc =
  output acc (`El_start ((ns, "superscript"), []))

let tag_t output acc =
  output acc (`El_start ((ns, "tag"), []))

let text_t output acc =
  output acc (`El_start ((ns, "text"), []))

let tuple_t output acc =
  output acc (`El_start ((ns, "tuple"), []))

let type_t output acc =
  output acc (`El_start ((ns, "type"), []))

let typeof_t output acc =
  output acc (`El_start ((ns, "typeof"), []))

let type_subst_t output acc =
  output acc (`El_start ((ns, "type_subst"), []))

let unit_t output acc =
  output acc (`El_start ((ns, "unit"), [(Xmlm.ns_xmlns,"xmlns"),DocOckXml.ns]))

let url_t output acc =
  output acc (`El_start ((ns, "url"), []))

let value_t output acc =
  output acc (`El_start ((ns, "value"), []))

let var_t output acc =
  output acc (`El_start ((ns, "var"), []))

let variant_t output acc =
  output acc (`El_start ((ns, "variant"), []))

let verbatim_t output acc =
  output acc (`El_start ((ns, "verbatim"), []))

let version_t output acc =
  output acc (`El_start ((ns, "version"), []))

let virtual_t output acc =
  output acc (`El_start ((ns, "virtual"), []))

let with_t output acc =
  output acc (`El_start ((ns, "with"), []))

(* Terminals with attributes *)

let argument_t output acc = function
  | None ->
      output acc (`El_start ((ns, "argument"), []))
  | Some pos ->
      let attr = (("", "pos"), string_of_int pos) in
        output acc (`El_start ((ns, "argument"), [attr]))

let custom_t output acc tag =
  let attr = (("", "tag"), tag) in
    output acc (`El_start ((ns, "custom"), [attr]))

let target_t output acc = function
  | None ->
      output acc (`El_start ((ns, "target"), []))
  | Some name ->
      let attr = (("", "pos"), name) in
        output acc (`El_start ((ns, "target"), [attr]))

let title_t output acc level =
  let attr = (("", "level"), string_of_int level) in
    output acc (`El_start ((ns, "title"), [attr]))

(* Special terminals *)

let close output acc =
  output acc `El_end

let data output acc s =
  output acc (`Data s)

let dtd output acc d =
  output acc (`Dtd d)

(* Utilites *)

let closed t output acc =
  let acc = t output acc in
  close output acc

let flag t output acc b =
  if b then closed t output acc else acc

let simple t output acc s =
  let acc = t output acc in
  let acc = data output acc s in
  close output acc

let opt p base output acc o =
  match o with
  | None -> acc
  | Some x -> p base output acc x

let rec list p base output acc l =
  match l with
  | [] -> acc
  | x :: xs ->
    let acc = p base output acc x in
    list p base output acc xs

let int t output acc i =
  simple t output acc (string_of_int i)

(* Non-terminals *)

let name_p base output acc n = simple name_t output acc n

open DocOckPaths

let rec identifier_p: type a. _ -> _ -> _ -> (_, a) Identifier.t -> _ =
  fun base output acc id ->
    let open Identifier in
    let component t sg name =
      let acc = t output acc in
      let acc = identifier_p base output acc sg in
      let acc = data output acc name in
      close output acc
    in
      match id with
      | Root(r, name) ->
        let acc = root_t output acc in
        let acc = base_t output acc in
        let acc = base.f output acc r in
        let acc = close output acc in
        let acc = data output acc name in
        close output acc
      | Argument(sg, pos, name) ->
        let acc = argument_t output acc (Some pos) in
        let acc = identifier_p base output acc sg in
        let acc = data output acc name in
        close output acc
      | Module(sg, name) -> component module_t sg name
      | ModuleType(sg, name) -> component module_type_t sg name
      | Type(sg, name) -> component type_t sg name
      | CoreType name -> simple type_t output acc name
      | Constructor(sg, name) -> component constructor_t sg name
      | Field(sg, name) -> component field_t sg name
      | Extension(sg, name) -> component extension_t sg name
      | Exception(sg, name) -> component exception_t sg name
      | CoreException name -> simple exception_t output acc name
      | Value(sg, name) -> component value_t sg name
      | Class(sg, name) -> component class_t sg name
      | ClassType(sg, name) -> component class_type_t sg name
      | Method(csig, name) -> component method_t csig name
      | InstanceVariable(csig, name) -> component instance_variable_t csig name
      | Label(cnt, name) -> component label_t cnt name

let rec resolved_path_p : type a. _ -> _ -> _ -> (_, a) Path.Resolved.t -> _ =
  fun base output acc p ->
    let component t m name =
      let acc = t output acc in
      let acc = resolved_path_p base output acc m in
      let acc = data output acc name in
      close output acc
    in
    let open Path.Resolved in
      match p with
      | Identifier id ->
        let acc = identifier_t output acc in
        let acc = identifier_p base output acc id in
        close output acc
      | Subst(sub, p) ->
        let acc = subst_t output acc in
        let acc = resolved_path_p base output acc sub in
        let acc = resolved_path_p base output acc p in
        close output acc
      | SubstAlias(sub, p) ->
        let acc = subst_alias_t output acc in
        let acc = resolved_path_p base output acc sub in
        let acc = resolved_path_p base output acc p in
        close output acc
      | Apply(m, arg) ->
        let acc = apply_t output acc in
        let acc = resolved_path_p base output acc m in
        let acc = path_p base output acc arg in
        close output acc
      | Module(m, name) -> component module_t m name
      | ModuleType(m, name) -> component module_type_t m name
      | Type(m, name) -> component type_t m name
      | Class(m, name) -> component class_t m name
      | ClassType(m, name) -> component class_type_t m name

and path_p : type a. _ -> _ -> _ -> (_, a) Path.t -> _ =
  fun base output acc p ->
    let open Path in
      match p with
      | Resolved p ->
        let acc = resolved_t output acc in
        let acc = resolved_path_p base output acc p in
        close output acc
      | Root name ->
        let acc = root_t output acc in
        let acc = data output acc name in
        close output acc
      | Dot(m, name) ->
        let acc = dot_t output acc in
        let acc = path_p base output acc m in
        let acc = data output acc name in
        close output acc
      | Apply(m, arg) ->
        let acc = apply_t output acc in
        let acc = path_p base output acc m in
        let acc = path_p base output acc arg in
        close output acc

let rec resolved_fragment_p
  : type a b. _ -> _ -> _ -> (_, a, b) Fragment.Resolved.raw -> _ =
  fun base output acc frag ->
    let component t m name =
      let acc = t output acc in
      let acc = resolved_fragment_p base output acc m in
      let acc = data output acc name in
      close output acc
    in
    let open Fragment.Resolved in
      match frag with
      | Root -> closed root_t output acc
      | Subst(sub, p) ->
        let acc = subst_t output acc in
        let acc = resolved_path_p base output acc sub in
        let acc = resolved_fragment_p base output acc p in
        close output acc
      | SubstAlias(sub, p) ->
        let acc = subst_alias_t output acc in
        let acc = resolved_path_p base output acc sub in
        let acc = resolved_fragment_p base output acc p in
        close output acc
      | Module(m, name) -> component module_t m name
      | Type(m, name) -> component type_t m name
      | Class(m, name) -> component class_t m name
      | ClassType(m, name) -> component class_type_t m name

let rec fragment_p : type a b. _ -> _ -> _ -> (_, a, b) Fragment.raw -> _ =
  fun base output acc frag ->
    let open Fragment in
      match frag with
      | Resolved fragment ->
        let acc = resolved_t output acc in
        let acc = resolved_fragment_p base output acc fragment in
        close output acc
      | Dot(m, name) ->
        let acc = dot_t output acc in
        let acc = fragment_p base output acc m in
        let acc = data output acc name in
        close output acc

let rec resolved_reference_p
  : type a. _ -> _ -> _ -> (_, a) Reference.Resolved.t -> _ =
  fun base output acc rf ->
    let component t sg name =
      let acc = t output acc in
      let acc = resolved_reference_p base output acc sg in
      let acc = data output acc name in
      close output acc
    in
    let open Reference.Resolved in
      match rf with
      | Identifier id ->
        let acc = identifier_t output acc in
        let acc = identifier_p base output acc id in
        close output acc
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

and reference_p : type a. _ -> _ -> _ -> (_, a) Reference.t -> _ =
  fun base output acc rf ->
    let open Reference in
      match rf with
      | Resolved rf ->
        let acc = resolved_t output acc in
        let acc = resolved_reference_p base output acc rf in
        close output acc
      | Root name -> simple root_t output acc name
      | Dot(m, name) ->
        let acc = dot_t output acc in
        let acc = reference_p base output acc m in
        let acc = data output acc name in
        close output acc

open DocOckTypes

let doc_reference_p base output acc rf =
  let open Documentation in
  let reference t rf =
    let acc = t output acc in
    let acc = reference_p base output acc rf in
    close output acc
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
    | Link s -> simple link_t output acc s
    | Custom(tag, s) ->
      let acc = custom_t output acc tag in
      let acc = data output acc s in
      close output acc

let special_p base output acc =
  let open Documentation in function
    | Modules mds ->
      let acc = modules_t output acc in
      let acc = list reference_p base output acc mds in
      close output acc
    | Index -> closed index_t output acc

let rec item_p base output acc txt =
  let acc = item_t output acc in
  let acc = text_p base output acc txt in
  close output acc

and text_element_p base output acc elem =
  let open Documentation in
  let style t txt =
    let acc = t output acc in
    let acc = text_p base output acc txt in
    close output acc
  in
    match elem with
    | Raw s -> data output acc s
    | Code s -> simple code_t output acc s
    | PreCode s -> simple precode_t output acc s
    | Verbatim s -> simple verbatim_t output acc s
    | Style(Bold, txt) -> style bold_t txt
    | Style(Italic, txt) -> style italic_t txt
    | Style(Emphasize, txt) -> style emphasize_t txt
    | Style(Center, txt) -> style center_t txt
    | Style(Left, txt) -> style left_t txt
    | Style(Right, txt) -> style right_t txt
    | Style(Superscript, txt) -> style superscript_t txt
    | Style(Subscript, txt) -> style subscript_t txt
    | Style(Custom tag, txt) ->
      let acc = custom_t output acc tag in
      let acc = text_p base output acc txt in
      close output acc
    | List items ->
      let acc = list_t output acc in
      let acc = list item_p base output acc items in
      close output acc
    | Enum items ->
      let acc = enum_t output acc in
      let acc = list item_p base output acc items in
      close output acc
    | Newline ->
      let acc = newline_t output acc in
      close output acc
    | Title(level, lbl, txt) ->
      let acc = title_t output acc level in
      let acc = opt identifier_p base output acc lbl in
      let acc = text_p base output acc txt in
      close output acc
    | Reference(rf, txt) ->
      let acc = reference_t output acc in
      let acc = doc_reference_p base output acc rf in
      let acc = opt text_p base output acc txt in
      close output acc
    | Target(target, s) ->
      let acc = target_t output acc target in
      let acc = data output acc s in
      close output acc
    | Special srf ->
      let acc = special_t output acc in
      let acc = special_p base output acc srf in
      close output acc

and text_p base output acc txt =
  list text_element_p base output acc txt

and see output acc =
  let open Documentation in function
  | Url s -> simple url_t output acc s
  | File s -> simple file_t output acc s
  | Doc s -> simple doc_t output acc s

and tag_p base output acc tg =
  let open Documentation in
  let block t txt =
    let acc = t output acc in
    let acc = text_p base output acc txt in
    close output acc
  in
  let named_block t n txt =
    let acc = t output acc in
    let acc = name_p base output acc n in
    let acc = text_p base output acc txt in
    close output acc
  in
    match tg with
    | Author s -> simple author_t output acc s
    | Version s -> simple version_t output acc s
    | See(s, txt) ->
      let acc = see_t output acc in
      let acc = see output acc s in
      let acc = text_p base output acc txt in
      close output acc
    | Since s -> simple since_t output acc s
    | Before(name, txt) -> named_block before_t name txt
    | Deprecated txt -> block deprecated_t txt
    | Param(name, txt) -> named_block param_t name txt
    | Raise(name, txt) -> named_block raise_t name txt
    | Return txt -> block return_t txt
    | Tag(name, txt) -> named_block tag_t name txt

and tags_p base output acc tgs =
  list tag_p base output acc tgs

let position_p base output acc offset =
  let open Documentation.Error.Position in
  let acc = position_t output acc in
  let acc = int line_t output acc offset.line in
  let acc = int column_t output acc offset.column in
  close output acc

let offset_p base output acc offset =
  let open Documentation.Error.Offset in
  let acc = offset_t output acc in
  let acc = position_p base output acc offset.start in
  let acc = position_p base output acc offset.finish in
  close output acc

let location_p base output acc loc =
  let open Documentation.Error.Location in
  let acc = location_t output acc in
  let acc = simple filename_t output acc loc.filename in
  let acc = position_p base output acc loc.start in
  let acc = position_p base output acc loc.finish in
  close output acc

let doc_error_p base output acc err =
  let open Documentation.Error in
  let acc = error_t output acc in
  let acc = identifier_p base output acc err.origin in
  let acc = offset_p base output acc err.offset in
  let acc = opt location_p base output acc err.location in
  let acc = data output acc err.message in
  close output acc

let doc_p base output acc =
  let open Documentation in function
    | Ok {text = []; tags = []} -> acc
    | Ok {text; tags} ->
      let acc = doc_t output acc in
      let acc = text_p base output acc text in
      let acc = tags_p base output acc tags in
      close output acc
    | Error err ->
      let acc = doc_t output acc in
      let acc = doc_error_p base output acc err in
      close output acc

let comment_p base output acc =
  let open Documentation in function
    | Documentation (Ok {text; tags}) ->
      let acc = comment_t output acc in
      let acc = text_p base output acc text in
      let acc = tags_p base output acc tags in
      close output acc
    | Documentation (Error err) ->
      let acc = comment_t output acc in
      let acc = doc_error_p base output acc err in
      close output acc
    | Stop -> closed stop_t output acc

let rec poly_variant_kind_p base output acc =
  let open TypeExpr.Variant in function
    | Fixed -> closed fixed_t output acc
    | Closed names ->
      let acc = closed_t output acc in
      let acc = list name_p base output acc names in
      close output acc
    | Open -> closed open_t output acc

and poly_variant_element_p base output acc =
  let open TypeExpr.Variant in function
    | Type expr ->
      let acc = type_t output acc in
      let acc = type_expr_p base output acc expr in
      close output acc
    | Constructor(name, constant, types) ->
      let acc = constructor_t output acc in
      let acc = data output acc name in
      let acc = flag constant_t output acc constant in
      let acc = list type_expr_p base output acc types in
      close output acc

and object_method_p base output acc m =
  let open TypeExpr.Object in
  let acc = name_p base output acc m.name in
  type_expr_p base output acc m.type_

and package_substitution_p base output acc (frag, expr) =
  let acc = fragment_p base output acc frag in
  type_expr_p base output acc expr

and argument_label_p base output acc =
  let open TypeExpr in function
    | Label s -> simple label_t output acc s
    | Optional s -> simple optional_t output acc s

and type_expr_p base output acc =
  let open TypeExpr in function
    | Var name -> simple var_t output acc name
    | Any -> closed any_t output acc
    | Alias(expr, name) ->
      let acc = alias_t output acc in
      let acc = type_expr_p base output acc expr in
      let acc = data output acc name in
      close output acc
    | Arrow(lbl, arg, res) ->
      let acc = arrow_t output acc in
      let acc = opt argument_label_p base output acc lbl in
      let acc = type_expr_p base output acc arg in
      let acc = type_expr_p base output acc res in
      close output acc
    | Tuple types ->
      let acc = tuple_t output acc in
      let acc = list type_expr_p base output acc types in
      close output acc
    | Constr(p, params) ->
      let acc = path_t output acc in
      let acc = path_p base output acc p in
      let acc = list type_expr_p base output acc params in
      close output acc
    | Variant pv ->
      let open Variant in
      let acc = poly_variant_t output acc in
      let acc = poly_variant_kind_p base output acc pv.kind in
      let acc = list poly_variant_element_p base output acc pv.elements in
      close output acc
    | Object obj ->
      let open Object in
      let acc = object_t output acc in
      let acc = list object_method_p base output acc obj.methods in
      let acc = flag open_t output acc obj.open_ in
      close output acc
    | Class(p, params) ->
      let acc = class_t output acc in
      let acc = path_p base output acc p in
      let acc = list type_expr_p base output acc params in
      close output acc
    | Poly(vars, expr) ->
      let acc = poly_t output acc in
      let acc = list name_p base output acc vars in
      let acc = type_expr_p base output acc expr in
      close output acc
    | Package pkg ->
      let open Package in
      let acc = package_t output acc in
      let acc = path_p base output acc pkg.path in
      let acc = list package_substitution_p base output acc pkg.substitutions in
      close output acc

let external_primitive_p base output acc s = simple primitive_t output acc s

let constructor_arguments_p base output acc = function
  | [] -> acc
  | types ->
    let acc = arguments_t output acc in
    let acc = list type_expr_p base output acc types in
    close output acc

let constructor_result_p base output acc = function
  | None -> acc
  | Some expr ->
    let acc = result_t output acc in
    let acc = type_expr_p base output acc expr in
    close output acc

let constructor_p base output acc cstr =
  let open TypeDecl.Constructor in
  let acc = constructor_t output acc in
  let acc = identifier_p base output acc cstr.id in
  let acc = doc_p base output acc cstr.doc in
  let acc = constructor_arguments_p base output acc cstr.args in
  let acc = constructor_result_p base output acc cstr.res in
  close output acc

let field_p base output acc fld =
  let open TypeDecl.Field in
  let acc = field_t output acc in
  let acc = identifier_p base output acc fld.id in
  let acc = doc_p base output acc fld.doc in
  let acc = flag mutable_t output acc fld.mutable_ in
  let acc = type_expr_p base output acc fld.type_ in
  close output acc

let type_representation_p base output acc =
  let open TypeDecl.Representation in function
    | Variant constructors ->
      let acc = variant_t output acc in
      let acc = list constructor_p base output acc constructors in
      close output acc
    | Record fields ->
      let acc = record_t output acc in
      let acc = list field_p base output acc fields in
      close output acc
    | Extensible -> closed extensible_t output acc

let variance_p base output acc =
  let open TypeDecl in function
    | Pos -> closed pos_t output acc
    | Neg -> closed neg_t output acc

let type_parameter_p base output acc =
  let open TypeDecl in function
    | (Any, v) ->
      let acc = param_t output acc in
      let acc = opt variance_p base output acc v in
      close output acc
    | (Var name, v) ->
      let acc = param_t output acc in
      let acc = data output acc name in
      let acc = opt variance_p base output acc v in
      close output acc

let type_subst_parameter_p base output acc name =
  let acc = param_t output acc in
  let acc = data output acc name in
  close output acc

let type_constraint_p base output acc (expr1, expr2) =
  let acc = constraint_t output acc in
  let acc = type_expr_p base output acc expr1 in
  let acc = type_expr_p base output acc expr2 in
  close output acc

let type_equation_p base output acc eq =
  let open TypeDecl.Equation in
  let acc = list type_parameter_p base output acc eq.params in
  let acc = flag private_t output acc eq.private_ in
  let acc = opt type_expr_p base output acc eq.manifest in
  list type_constraint_p base output acc eq.constraints

let extension_constructor_p base output acc ext =
  let open Extension.Constructor in
  let acc = constructor_t output acc in
  let acc = identifier_p base output acc ext.id in
  let acc = doc_p base output acc ext.doc in
  let acc = constructor_arguments_p base output acc ext.args in
  let acc = constructor_result_p base output acc ext.res in
  close output acc

let rec class_decl_p base output acc =
  let open Class in function
    | ClassType clty -> class_type_expr_p base output acc clty
    | Arrow(lbl, arg, res) ->
      let acc = arrow_t output acc in
      let acc = opt argument_label_p base output acc lbl in
      let acc = type_expr_p base output acc arg in
      let acc = class_decl_p base output acc res in
      close output acc

and class_type_expr_p base output acc =
  let open ClassType in function
    | Constr(p, params) ->
      let acc = path_t output acc in
      let acc = path_p base output acc p in
      let acc = list type_expr_p base output acc params in
      close output acc
    | Signature sg ->
      let open ClassSignature in
      let acc = signature_t output acc in
      let acc = opt type_expr_p base output acc sg.self in
      let acc = list class_signature_item_p base output acc sg.items in
      close output acc

and class_signature_item_p base output acc =
  let open ClassSignature in function
    | InstanceVariable inst ->
      let open InstanceVariable in
      let acc = instance_variable_t output acc in
      let acc = identifier_p base output acc inst.id in
      let acc = doc_p base output acc inst.doc in
      let acc = flag mutable_t output acc inst.mutable_ in
      let acc = flag virtual_t output acc inst.virtual_ in
      let acc = type_expr_p base output acc inst.type_ in
      close output acc
    | Method meth ->
      let open Method in
      let acc = method_t output acc in
      let acc = identifier_p base output acc meth.id in
      let acc = doc_p base output acc meth.doc in
      let acc = flag private_t output acc meth.private_ in
      let acc = flag virtual_t output acc meth.virtual_ in
      let acc = type_expr_p base output acc meth.type_ in
      close output acc
    | Constraint(expr1, expr2) ->
      let acc = constraint_t output acc in
      let acc = type_expr_p base output acc expr1 in
      let acc = type_expr_p base output acc expr2 in
      close output acc
    | Inherit csig ->
      let acc = inherit_t output acc in
      let acc = class_type_expr_p base output acc csig in
      close output acc
    | Comment com -> comment_p base output acc com

let rec module_decl_p base output acc =
  let open Module in function
    | Alias p ->
      let acc = alias_t output acc in
      let acc = path_p base output acc p in
      close output acc
    | ModuleType mty ->
      let acc = type_t output acc in
      let acc = module_type_expr_p base output acc mty in
      close output acc

and substitution_p base output acc =
  let open ModuleType in function
    | ModuleEq(frag, eq) ->
      let acc = module_t output acc in
      let acc = fragment_p base output acc frag in
      let acc = module_decl_p base output acc eq in
      close output acc
    | ModuleSubst(frag, p) ->
      let acc = module_subst_t output acc in
      let acc = fragment_p base output acc frag in
      let acc = path_p base output acc p in
      close output acc
    | TypeEq(frag, eq) ->
      let acc = type_t output acc in
      let acc = fragment_p base output acc frag in
      let acc = type_equation_p base output acc eq in
      close output acc
    | TypeSubst(frag, params, p) ->
      let acc = type_subst_t output acc in
      let acc = fragment_p base output acc frag in
      let acc = list type_subst_parameter_p base output acc params in
      let acc = path_p base output acc p in
      close output acc

and module_argument_p base output acc = function
  | None ->
    let acc = argument_t output acc None in
    close output acc
  | Some(id, expr) ->
    let acc = argument_t output acc None in
    let acc = identifier_p base output acc id in
    let acc = module_type_expr_p base output acc expr in
    close output acc

and module_type_expr_p base output acc =
  let open ModuleType in function
    | Path p -> path_p base output acc p
    | Signature items ->
      let acc = signature_t output acc in
      let acc = list signature_item_p base output acc items in
      close output acc
    | Functor _ as mty->
        let rec loop acc = function
          | Functor(arg, expr) ->
            let acc = module_argument_p base output acc arg in
            loop acc expr
          | mty -> module_type_expr_p base output acc mty
        in
        let acc = functor_t output acc in
        let acc = loop acc mty in
        close output acc
    | With(expr, substs) ->
      let acc = with_t output acc in
      let acc = module_type_expr_p base output acc expr in
      let acc = list substitution_p base output acc substs in
      close output acc
    | TypeOf md ->
      let acc = typeof_t output acc in
      let acc = module_decl_p base output acc md in
      close output acc

and signature_item_p base output acc =
  let open Signature in function
    | Value v ->
      let open Value in
      let acc = value_t output acc in
      let acc = identifier_p base output acc v.id in
      let acc = doc_p base output acc v.doc in
      let acc = type_expr_p base output acc v.type_ in
      close output acc
    | External e ->
      let open External in
      let acc = external_t output acc in
      let acc = identifier_p base output acc e.id in
      let acc = doc_p base output acc e.doc in
      let acc = type_expr_p base output acc e.type_ in
      let acc = list external_primitive_p base output acc e.primitives in
      close output acc
    | Type typ ->
      let open TypeDecl in
      let acc = type_t output acc in
      let acc = identifier_p base output acc typ.id in
      let acc = doc_p base output acc typ.doc in
      let acc = type_equation_p base output acc typ.equation in
      let acc = opt type_representation_p base output acc typ.representation in
      close output acc
    | TypExt typext ->
      let open Extension in
      let acc = extension_t output acc in
      let acc = path_p base output acc typext.type_path in
      let acc = doc_p base output acc typext.doc in
      let acc = list type_parameter_p base output acc typext.type_params in
      let acc = flag private_t output acc typext.private_ in
      let acc =
        list extension_constructor_p base output acc typext.constructors
      in
      close output acc
    | Exception exn ->
      let open Exception in
      let acc = exception_t output acc in
      let acc = identifier_p base output acc exn.id in
      let acc = doc_p base output acc exn.doc in
      let acc = constructor_arguments_p base output acc exn.args in
      let acc = constructor_result_p base output acc exn.res in
      close output acc
    | Class cls ->
      let open Class in
      let acc = class_t output acc in
      let acc = identifier_p base output acc cls.id in
      let acc = doc_p base output acc cls.doc in
      let acc = list type_parameter_p base output acc cls.params in
      let acc = flag virtual_t output acc cls.virtual_ in
      let acc = class_decl_p base output acc cls.type_ in
      close output acc
    | ClassType clty ->
      let open ClassType in
      let acc = class_type_t output acc in
      let acc = identifier_p base output acc clty.id in
      let acc = doc_p base output acc clty.doc in
      let acc = list type_parameter_p base output acc clty.params in
      let acc = flag virtual_t output acc clty.virtual_ in
      let acc = class_type_expr_p base output acc clty.expr in
      close output acc
    | Module md ->
      let open Module in
      let acc = module_t output acc in
      let acc = identifier_p base output acc md.id in
      let acc = doc_p base output acc md.doc in
      let acc = module_decl_p base output acc md.type_ in
      close output acc
    | ModuleType mty ->
      let open ModuleType in
      let acc = module_type_t output acc in
      let acc = identifier_p base output acc mty.id in
      let acc = doc_p base output acc mty.doc in
      let acc = opt module_type_expr_p base output acc mty.expr in
      close output acc
    | Include incl ->
      let open Include in
      let acc = include_t output acc in
      let acc = identifier_p base output acc incl.parent in
      let acc = module_type_expr_p base output acc incl.expr in
      close output acc
    | Comment com -> comment_p base output acc com

let digest_p base output acc digest =
  let acc = digest_t output acc in
  let acc = data output acc (Digest.to_hex digest) in
  close output acc

let unit_import_p base output acc =
  let open Unit.Import in function
    | Unresolved(name, digest) ->
      let acc = import_t output acc in
      let acc = data output acc name in
      let acc = opt digest_p base output acc digest in
      close output acc
    | Resolved r ->
      let acc = import_t output acc in
      let acc = base_t output acc in
      let acc = base.f output acc r in
      let acc = close output acc in
      close output acc

let source_file_p base output acc file =
  let acc = file_t output acc in
  let acc = data output acc file in
  close output acc

let source_build_dir_p base output acc build_dir =
  let acc = dir_t output acc in
  let acc = data output acc build_dir in
  close output acc

let source_p base output acc source =
  let open Unit.Source in
  let acc = source_t output acc in
  let acc = source_file_p base output acc source.file in
  let acc = source_build_dir_p base output acc source.build_dir in
  let acc = digest_p base output acc source.digest in
  close output acc

let packed_item_p base output acc item =
  let open Unit.Packed in
  let acc = item_t output acc in
  let acc = identifier_p base output acc item.id in
  let acc = path_p base output acc item.path in
  close output acc

let unit_content_p base output acc =
  let open Unit in function
    | Module items ->
        let acc = module_t output acc in
        let acc = list signature_item_p base output acc items in
        close output acc
    | Pack items ->
        let acc = pack_t output acc in
        let acc = list packed_item_p base output acc items in
        close output acc

let unit_p base output acc unit =
  let open Unit in
  let acc = unit_t output acc in
  let acc = identifier_p base output acc unit.id in
  let acc = doc_p base output acc unit.doc in
  let acc = digest_p base output acc unit.digest in
  let acc = list unit_import_p base output acc unit.imports in
  let acc = opt source_p base output acc unit.source in
  let acc = flag interface_t output acc unit.interface in
  let acc = flag hidden_t output acc unit.hidden in
  let acc = unit_content_p base output acc unit.content in
  close output acc

let file_p base output acc unit =
  let acc = dtd output acc None in
    unit_p base output acc unit

let text_entry_p base output acc text =
  let acc = unit_t output acc in
  let acc = text_p base output acc text in
  close output acc

let text base = {f = fun output acc txt -> text_entry_p base output acc txt}
let unit base = {f = fun output acc unit -> unit_p base output acc unit}
let file base = {f = fun output acc unit -> file_p base output acc unit}
