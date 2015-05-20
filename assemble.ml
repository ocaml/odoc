open Assemblage

(* No alias dependencies flag *)
let no_alias_deps = Flags.( v (`Compile `Byte) ["-no-alias-deps"]
                     @@@ v (`Compile `Native) ["-no-alias-deps"] )

(* Compilation units *)
let octTypes = unit "octTypes" (`Path ["src"])
let octErrors = unit "octErrors" (`Path ["src"])
let octCommon = unit "octCommon" (`Path ["src"])
let octParser = unit "octParser" (`Path ["src"])
let octLexer = unit "octLexer" (`Path ["src"])
let octPrint = unit "octPrint" (`Path ["src"])
let octavius = unit "octavius" ~flags:no_alias_deps (`Path ["src"])

let units =
  [ octTypes;
    octErrors;
    octCommon;
    (*octParser;
    octLexer;*)
    octPrint;
    octavius ]

(* Library *)
let l = lib "octavius" (`Units units)

(* Assemble *)
let () = assemble (project "octavius" [l])
