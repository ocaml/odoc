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

  val string_of_kind : kind -> string

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
    is_flat:bool ->
    allow_empty:bool ->
    (kind * string) list ->
    (kind * string) list * (kind * string) list
  (** [split ~is_flat path] splits the list [path] into a directory
      and filename. Returns a tuple
      of directory-type elements and filename-type elements. If [allow_empty]
      is [true], the filename part will be empty if all components are
      directory-type. If [allow_empty] is [false], the last element will part
      of the filename, even if it is a directory-type. *)
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

val from_asset_identifier : Identifier.AssetFile.t -> t

val kind : Identifier.t -> kind

val render_path : Odoc_model.Paths.Path.t -> string
