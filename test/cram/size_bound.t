This tests the sherlodoc.js is not bigger than 120000 bytes. We test a threshold
of the size because the precise size depends on specific ocaml and dependencies
versions. This test should pass on every version. If it fails, we can either
update the threshold to be a larg enough or forbid certain dependency versions
in the opam file.
  $ sherlodoc js sherlodoc.js
  $ if [ "$(stat --printf="%s" sherlodoc.js)" -gt 120000 ]; then
  >    stat --printf="%s" sherlodoc.js
  > else
  >     echo "All good! ";
  > fi
  All good! 
