(* scratch *)


module Library = struct
  type t = {
    name : string
    modules : Module.t list
    findlib_path : Fpath.t
  }

  val inspect_dir path libs =
    
end
