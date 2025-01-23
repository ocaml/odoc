#if OCAML_VERSION >= (5, 0, 0)
module Format = struct
  include Format

  type formatter_tag_functions = {
    mark_open_tag : tag -> string;
    mark_close_tag : tag -> string;
    print_open_tag : tag -> unit;
    print_close_tag : tag -> unit;
  }

  let pp_set_formatter_tag_functions formatter fns =
    let {mark_open_tag = mot; mark_close_tag = mct;
         print_open_tag = pot; print_close_tag = pct} = fns in
    let wrap f v = function String_tag s -> f s | _ -> v in
    let stag_fns = {
      mark_open_stag = wrap mot "";
      mark_close_stag = wrap mct "";
      print_open_stag = wrap pot ();
      print_close_stag = wrap pct ()
    } in
    Format.pp_set_formatter_stag_functions formatter stag_fns
end
#endif
