
# Module `Include2`

```ocaml
module X : sig ... end
```
Comment about X that should not appear when including X below.

```ocaml
type t = int
```
```ocaml
module Y : sig ... end
```
Top-comment of Y.

```ocaml
module Y_include_synopsis : sig ... end
```
The `include Y` below should have the synopsis from `Y`'s top-comment attached to it.

```ocaml
module Y_include_doc : sig ... end
```