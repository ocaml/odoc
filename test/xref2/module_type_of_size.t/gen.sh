#!/bin/sh
for ((i=2; i<15; i++)); do echo "module type M = module type of M$((i-1)) with module N = M1.T" > m$i.mli; echo "module N : sig type t end" >> m$i.mli; done

echo "#!/bin/sh" > compile.sh
echo "set -ex" >> compile.sh
for ((i=1; i<15; i++)); do echo "ocamlc -c m$i.mli -bin-annot" >> compile.sh; done

echo "#!/bin/sh" > odoc.sh
echo "set -ex" >> odoc.sh
for ((i=1; i<15; i++)); do echo "odoc compile -I . m$i.cmti" >> odoc.sh; done

chmod +x compile.sh odoc.sh

