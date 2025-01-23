open Types
module O = Codefmt
open O.Infix

module ML = Generator.Make (struct
  module Obj = struct
    let close_tag_closed = " >"

    let close_tag_extendable = ".. >"

    let field_separator = "; "

    let open_tag_closed = "< "

    let open_tag_extendable = "< "
  end

  module Type = struct
    let annotation_separator = " : "

    let handle_params name args = O.span (args ++ O.sp ++ name)

    let handle_constructor_params = handle_params

    let handle_substitution_params name args = O.span (args ++ O.txt " " ++ name)

    let handle_format_params p = p

    let type_def_semicolon = false

    let private_keyword = "private"

    let parenthesize_constructor = false

    module Variant = struct
      let parenthesize_params = false
    end

    module Tuple = struct
      let element_separator = O.sp ++ O.txt "* "

      let always_parenthesize = false
    end

    module Record = struct
      let field_separator = ";"
    end

    let var_prefix = "'"

    let any = "_"

    let arrow = O.span ~attr:"arrow" (O.entity "#45" ++ O.entity "gt")

    module Exception = struct
      let semicolon = false
    end

    module GADT = struct
      let arrow = arrow
    end

    module External = struct
      let semicolon = false

      let handle_primitives prims =
        List.map (fun p -> inline @@ Text ("\"" ^ p ^ "\" ")) prims
    end
  end

  module Mod = struct
    let open_tag = O.keyword "sig"

    let close_tag = O.keyword "end"

    let close_tag_semicolon = false

    let include_semicolon = false

    let functor_keyword = true

    let functor_contraction = true
  end

  module Class = struct
    let open_tag = O.keyword "object"

    let close_tag = O.keyword "end"
  end

  module Value = struct
    let variable_keyword = "val"

    let semicolon = false
  end

  module Comment = struct
    let markers = ("(*", "*)")
  end
end)

include ML
