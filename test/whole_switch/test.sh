odig odoc
mkdir -p packages
cd packages
for PKG in $(ls $OPAM_SWITCH_PREFIX/var/cache/odig/odoc)
do
    echo $PKG
    dune exec sherlodoc_index -- --format=marshal --db=$PKG.db $(find $OPAM_SWITCH_PREFIX/var/cache/odig/odoc/$PKG -name "*.odocl") 2> $PKG.stderr > $PKG.stdout
done