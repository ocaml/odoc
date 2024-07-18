(**
   {1 Page foo}
   {!//foo} {!/pkg/foo} {!foo}
   {!//page-foo} {!/pkg/page-foo} {!page-foo}
   {1 Page subdir/bar}
   {!//subdir/bar} {!/pkg/subdir/bar} {!bar}
   {1 Page dup}
   {!//dup} {!/pkg/dup}
   {1 Page subdir/dup}
   {!//subdir/dup} {!/pkg/subdir/dup}
   {1 Module Test}
   {!//Test} {!/libname/Test} {!./Test} {!Test}
*)

type t
