(* Type definitions for the HTML renderer *)

type uri = Absolute of string | Relative of Odoc_document.Url.Path.t option

type toc = {
  title : Html_types.flow5_without_interactive Tyxml.Html.elt list;
  title_str : string;
  href : string;
  children : toc list;
}

type src_loc =
  | Token of Syntax_highlighter.token
  | Line of Source_line_splitting.line
