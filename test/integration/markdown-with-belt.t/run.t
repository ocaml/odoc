  $ ocamlc -c -bin-annot Belt_Id.mli
  $ ocamlc -c -bin-annot Belt.mli
  $ odoc compile --package Belt -I . Belt.cmti
  $ odoc compile --package Belt -I . Belt_Id.cmti

  $ odoc link Belt.odoc
  $ odoc link Belt_Id.odoc

  $ odoc markdown-generate Belt.odocl -o markdown
  $ odoc markdown-generate Belt_Id.odocl -o markdown

  $ cat markdown/Belt/Belt.md
  
  # Module `Belt`
  
  A stdlib shipped with Melange
  
  This stdlib is still in *beta* but we encourage you to try it out and give us feedback.
  
  **Motivation**
  
  The motivation for creating such library is to provide Melange users a better end-to-end user experience, since the original OCaml stdlib was not written with JS in mind. Below is a list of areas this lib aims to improve:
  
  1. Consistency in name convention: camlCase, and arguments order
  2. Exception thrown functions are all suffixed with *Exn*, e.g, *getExn*
  3. Better performance and smaller code size running on JS platform
  **Name Convention**
  
  For higher order functions, it will be suffixed **U** if it takes uncurried callback.
  
  ```ocaml
    val forEach  : 'a t -> ('a -> unit) -> unit
    val forEachU : 'a t -> ('a -> unit [\@u]) -> unit
  ```
  In general, uncurried version will be faster, but it may be less familiar to people who have a background in functional programming.
  
  **A special encoding for collection safety**
  
  When we create a collection library for a custom data type we need a way to provide a comparator function. Take *Set* for example, suppose its element type is a pair of ints, it needs a custom *compare* function that takes two tuples and returns their order. The *Set* could not just be typed as ` Set.t (int * int) `, its customized *compare* function needs to manifest itself in the signature, otherwise, if the user creates another customized *compare* function, the two collection could mix which would result in runtime error.
  
  The original OCaml stdlib solved the problem using *functor* which creates a big closure at runtime and makes dead code elimination much harder. We use a phantom type to solve the problem:
  
  ```ocaml
    module Comparable1 = Belt.Id.MakeComparable (struct
      type t = int * int
      let cmp (a0, a1) (b0, b1) =
        match Pervasives.compare a0 b0 with
        | 0 -> Pervasives.compare a1 b1
        | c -> c
    end)
  
    let mySet1 = Belt.Set.make ~id:(module Comparable1)
  
    module Comparable2 = Belt.Id.MakeComparable (struct
      type t = int * int
      let cmp (a0, a1) (b0, b1) =
        match Pervasives.compare a0 b0 with
        | 0 -> Pervasives.compare a1 b1
        | c -> c
    end)
  
    let mySet2 = Belt.Set.make ~id:(module Comparable2)
  ```
  Here, the compiler would infer `mySet1` and `mySet2` having different type, so e.g. a \`merge\` operation that tries to merge these two sets will correctly fail.
  
  ```ocaml
    val mySet1 : (int * int, Comparable1.identity) t
    val mySet2 : (int * int, Comparable2.identity) t
  ```
  `Comparable1.identity` and `Comparable2.identity` are not the same using our encoding scheme.
  
  **Collection Hierarchy**
  
  In general, we provide a generic collection module, but also create specialized modules for commonly used data type. Take *Belt.Set* for example, we provide:
  
  ```ocaml
    Belt.Set
    Belt.Set.Int
    Belt.Set.String
  ```
  The specialized modules *Belt.Set.Int*, *Belt.Set.String* are in general more efficient.
  
  Currently, both *Belt\_Set* and *Belt.Set* are accessible to users for some technical reasons, we **strongly recommend** users stick to qualified import, *Belt.Set*, we may hide the internal, *i.e*, *Belt\_Set* in the future
  
  ```
  module Id = Belt_Id
  ```
  [`Belt.Id`](./Belt_Id.md)
  
  Provide utilities to create identified comparators or hashes for data structures used below.
  
  It create a unique identifier per module of functions so that different data structures with slightly different comparison functions won't mix
