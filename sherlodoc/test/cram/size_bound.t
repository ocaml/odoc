This tests that sherlodoc.js is not bigger than 120000 bytes. We test a threshold
of the size because the precise size depends on specific ocaml and dependencies
versions. This test should pass on every version. If it fails, we can either
update the threshold to be large enough or forbid certain dependency versions
in the opam file.
  $ sherlodoc js sherlodoc.js
  $ if [ "$(du sherlodoc.js | cut -f 1)" -gt 120000 ]; then
  >     du sherlodoc.js
  > else
  >     echo "All good! ";
  > fi
  All good! 
