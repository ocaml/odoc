  $ echo "hello" > file.cmti
  $ odoc extract-code -o output.ml --line-directives file.cmti
  ERROR: Error while unmarshalling input file file.cmti:
  End_of_file
  Check that the input file is a valid cmti file
  [1]
  $ touch hello.ext
  $ odoc extract-code -o output.ml --line-directives hello.ext
  ERROR: Input must have either mld or cmti as extension
  [1]
