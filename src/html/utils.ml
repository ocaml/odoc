(* Shared utility functions *)

let optional_elt f ?a = function [] -> [] | l -> [ f ?a l ]
