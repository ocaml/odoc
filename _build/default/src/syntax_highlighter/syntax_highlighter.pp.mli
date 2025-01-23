# 1 "src/syntax_highlighter/syntax_highlighter.mli"
type infos = (string * (int * int)) list
val syntax_highlighting_locs : string -> infos
