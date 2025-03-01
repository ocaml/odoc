(* Type definitions for the HTML renderer *)

type uri = Absolute of string | Relative of Odoc_document.Url.Path.t option

type file_uri = Absolute of string | Relative of Odoc_document.Url.Path.t

type toc = {
  title : Html_types.flow5_without_interactive Tyxml.Html.elt list;
  title_str : string;
  href : string;
  children : toc list;
}

type breadcrumb = {
  href : string option;
  name : Html_types.phrasing_without_interactive Tyxml.Html.elt list;
  kind : Odoc_document.Url.Path.kind;
}

type breadcrumbs = {
  parents : breadcrumb list;
  current : breadcrumb;
  up_url : string option;
}
