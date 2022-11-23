(* Type definitions for the HTML renderer *)

type uri = Absolute of string | Relative of Odoc_document.Url.Path.t option

type toc = {
  title : Html_types.flow5_without_interactive Tyxml.Html.elt list;
  title_str : string;
  href : string;
  children : toc list;
}

type breadcrumb = {
  href : string;
  name : string;
  kind : Odoc_document.Url.Path.kind;
}
