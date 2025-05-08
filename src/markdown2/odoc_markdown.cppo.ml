module Config = Config

#if OCAML_VERSION >= (4, 08, 0)
module Generator = Generator
#else
module Generator = struct
  let render (_ : Config.t) _ = failwith "Markdown generation isn't available"

  let filepath (_ : Config.t) _ = failwith "Markdown generation isn't available"

  let items (_ : Config.t) _ = failwith "Markdown generation isn't available"

  let inline (_ : Config.t) _ = failwith "Markdown generation isn't available"
end
#endif

