
# Module `Include2`

```
module X : sig ... end
```
Comment about X that should not appear when including X below.

```
type t = int
```
```
module Y : sig ... end
```
Top-comment of Y.

```
module Y_include_synopsis : sig ... end
```
The `include Y` below should have the synopsis from `Y`'s top-comment attached to it.

```
module Y_include_doc : sig ... end
```