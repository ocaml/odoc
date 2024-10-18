open Result
open Odoc_model.Paths

module Error : sig
  type nonrec t =
    | Not_linkable of string
    | Uncaught_exn of string
    (* These should basicaly never happen *)
    | Unexpected_anchor of string

  val to_string : t -> string
end

module Path : sig
  type kind =
    [ `Module
    | `Page
    | `LeafPage
    | `ModuleType
    | `Parameter of int
    | `Class
    | `ClassType
    | `File
    | `SourcePage ]

  val pp_kind : Format.formatter -> kind -> unit

  val string_of_kind : kind -> string

  val pp_disambiguating_prefix : Format.formatter -> kind -> unit
  (** Print the ["kind-"] prefix used to disambiguate urls in "flat modes":
      e.g. latex labels and output files in [--flat] HTML and man output *)

  type t = { kind : kind; parent : t option; name : string }

  type any_pv =
    [ Identifier.Page.t_pv
    | Identifier.Signature.t_pv
    | Identifier.ClassSignature.t_pv
    | Identifier.SourcePage.t_pv
    | Identifier.AssetFile.t_pv ]

  and any = any_pv Odoc_model.Paths.Identifier.id

  val from_identifier : [< any_pv ] Odoc_model.Paths.Identifier.id -> t

  val to_list : t -> (kind * string) list

  val of_list : (kind * string) list -> t option

  val split :
    is_dir:(kind -> [ `Always | `Never | `IfNotLast ]) ->
    (kind * string) list ->
    (kind * string) list * (kind * string) list
  (** [split is_dir path] splits the list [path] into a directory
      and filename, based on the [is_dir] function. The function
      [is_dir] should return whether or not the path element [kind]
      should be a directory or not. If the function [is_dir] returns
      [`IfNotLast] then the element will be a directory only if it
      is not the last element in the path. The return value is a tuple
      of directory-type elements and filename-type elements. If the
      [is_dir] function can return [`Always], the caller must be prepared
      to handle the case where the filename part is empty. *)
end

module Anchor : sig
  type kind =
    [ Path.kind
    | `Section
    | `Type
    | `Extension
    | `ExtensionDecl
    | `Exception
    | `Method
    | `Val
    | `Constructor
    | `Field
    | `SourceAnchor ]

  val pp_kind : Format.formatter -> kind -> unit

  val string_of_kind : kind -> string

  type t = {
    page : Path.t;
    anchor : string;
        (** Anchor in {!field-page} where the element is attached *)
    kind : kind;
        (** What kind of element the path points to.
        e.g. "module", "module-type", "exception", ... *)
  }

  val from_identifier : Identifier.t -> (t, Error.t) result

  val polymorphic_variant :
    type_ident:Identifier.t ->
    Odoc_model.Lang.TypeExpr.Polymorphic_variant.element ->
    t

  val extension_decl : Odoc_model.Lang.Extension.t -> t
  (** Anchor for the extension declaration item itself, which doesn't have an
      identifier in the model. *)

  val source_anchor : Path.t -> string -> t
end

type kind = Anchor.kind

type t = Anchor.t

val from_path : Path.t -> t

val from_identifier : stop_before:bool -> Identifier.t -> (t, Error.t) result
(** [from_identifier] turns an identifier to an url.

     Some identifiers can be accessed in different ways. For instance,
     submodules generate a dedicated page, but they can also be linked to at
     their parent page, using a hash.

     The [stop_before] boolean controls that: with [~stop_before:true], the url
     will point to the parent page when applicable.

     There are several wrong ways to use [from_identifier]:
     - Using [~stop_before:false] with a module that does not contain an
     expansion, such as a module alias. This will generate a 404 url.
     - Using [~stop_before:true] with a module that does not contain a parent,
     such as a root module. This will ouput return an [Error _] value.
     - Calling it with an unlinkable id, such as a core type. This will ouput
     return an [Error _] value.

     Please, reader, go and fix this API. Thanks. *)

val from_asset_identifier : Identifier.AssetFile.t -> t

val kind : Identifier.t -> kind

val render_path : Odoc_model.Paths.Path.t -> string
