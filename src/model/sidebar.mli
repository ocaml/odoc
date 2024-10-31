open Paths.Identifier

module PageToc : sig
  type title = Comment.link_content

  type index = Page.t * title
  type t = index option Odoc_utils.Tree.t

  val of_list :
    (LeafPage.t * title * Frontmatter.children_order option) list -> t
  (** Uses the convention that the [index] children passes its payload to the
      container directory to output a payload *)
end

type library = { name : string; units : RootModule.t list }

type page_hierarchy = { hierarchy_name : string; pages : PageToc.t }

type t = { pages : page_hierarchy list; libraries : library list }
