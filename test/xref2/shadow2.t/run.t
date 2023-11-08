  $ cat a.mli
  module type B1 = sig
    module A : sig
      type t
    end
  end
  
  include B1
  
  module type B2 = sig
    module A : sig
      include module type of struct include A end
      type u
    end
  end
  
  include B2
  
  module type B3 = sig
    module A : sig
      include module type of struct include A end
      type v
    end
  end
  
  include B3
  

In the above file we can see a chain of `B` modules that each contain
an `A` module extended from the previous. In each case the `A` is shadowed
except the last.

  $ ocamlc -c -bin-annot a.mli
  $ odoc compile a.cmti
  File "a.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(A).{A}2,true) end Unresolved module path identifier(root(A).{A}2,true) (Lookup failure (module): root(A).{A}2)
  File "a.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(A).{A}2,true) end Unresolved module path identifier(root(A).{A}2,true) (Lookup failure (module): root(A).{A}2)
  File "a.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(A).{A}1,true) end Unresolved module path identifier(root(A).{A}1,true) (Lookup failure (module): root(A).{A}1)
  File "a.cmti":
  Warning: Failed to compile expansion for include module type of struct include identifier(root(A).{A}1,true) end Unresolved module path identifier(root(A).{A}1,true) (Lookup failure (module): root(A).{A}1)

Everything should link correctly.

  $ odoc link a.odoc
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 25, character 0
  While resolving the expansion of include at File "a.mli", line 20, character 4
  Failed to lookup type identifier(root(A).{A}2,true).u Parent_module: Lookup failure (module): root(A).{A}2
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 25, character 0
  While resolving the expansion of include at File "a.mli", line 20, character 4
  Failed to lookup type identifier(root(A).{A}1,true).t Parent_module: Lookup failure (module): root(A).{A}1
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 25, character 0
  Failed to resolve module path identifier(root(A).{A}2,true) Lookup failure (module): root(A).{A}2
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 20, character 4
  Failed to lookup type identifier(root(A).{A}2,true).u Parent_module: Lookup failure (module): root(A).{A}2
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 20, character 4
  Failed to lookup type identifier(root(A).{A}1,true).t Parent_module: Lookup failure (module): root(A).{A}1
  File "a.odoc":
  Warning: Failed to resolve module path identifier(root(A).{A}2,true) Lookup failure (module): root(A).{A}2
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 16, character 0
  While resolving the expansion of include at File "a.mli", line 11, character 4
  Failed to lookup type identifier(root(A).{A}1,true).t Parent_module: Lookup failure (module): root(A).{A}1
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 16, character 0
  Failed to resolve module path identifier(root(A).{A}1,true) Lookup failure (module): root(A).{A}1
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 11, character 4
  Failed to lookup type identifier(root(A).{A}1,true).t Parent_module: Lookup failure (module): root(A).{A}1
  File "a.odoc":
  Warning: Failed to resolve module path identifier(root(A).{A}1,true) Lookup failure (module): root(A).{A}1

