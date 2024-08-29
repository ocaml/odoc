open Paths.Identifier

module PageToc : sig
  type title = Comment.link_content
  type children_order = Frontmatter.child list

  type t
  type content = Entry of title | Dir of t

  val of_list : (LeafPage.t * title * children_order option) list -> t
  (** Uses the convention that the [index] children passes its payload to the
      container directory to output a payload *)

  val find : t -> Page.t -> content option
  val contents : t -> (Page.t * content) list

  val dir_payload : t -> (title * LeafPage.t) option
  (** Gets a title and the ID from a potential [index] page *)
end

type toc = PageToc.t

type library = { name : string; units : RootModule.t list }

type page_hierarchy = { hierarchy_name : string; pages : toc }

type t = { pages : page_hierarchy list; libraries : library list }
