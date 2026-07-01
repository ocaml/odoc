
# Module type `Oxcaml.Mode_sig`

Module type containing declarations with modes.

```ocaml
val arg : int @ local -> int
```
Mode on a function argument.

```ocaml
val ret : int -> int @ local
```
Mode on a function return.

```ocaml
val multi : string @ local once -> string @ local unique
```
Multiple modes on argument and return.

```ocaml
type alias = int @ local -> int
```
Type alias for an arrow with a mode.
