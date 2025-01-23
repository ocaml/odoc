# 1 "sherlodoc/cli/serve.unavailable.ml"
let main _ _ =
  Format.fprintf
    Format.err_formatter
    "Webserver unavailable: please install dream and retry.@."

let term = Cmdliner.Term.const main
