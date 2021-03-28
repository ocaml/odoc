(** {0 Module Ocamlary}
*)

(**
  This is an {i interface} with {b all} of the {e module system} features. This documentation demonstrates:
*)

(** -
      comment formatting
  -
    unassociated comments
  -
    documentation sections
  -
    module system documentation including
    1)
      submodules
    2)
      module aliases
    3)
      module types
    4)
      module type aliases
    5)
      modules with signatures
    6)
      modules with aliased signatures
*)

(** A numbered list:
*)

(** 1)
      32)
         23)
            1
*)

(** David Sheets is the author.
*)

(** @author:
      David Sheets
*)

(**
  You may find more information about this HTML documentation renderer at 
  {{: https://github.com/dsheets/ocamlary}github.com/dsheets/ocamlary}.
*)

(** This is some verbatim text:{v verbatim
                               v}
*)

(** This is some verbatim text:{v [][df[]]}}
                               v}
*)

(** Here is some raw LaTeX: 
*)

(** Here is an index table of Empty modules:@{!Empty}:
                                              A plain, empty module
  @{!EmptyAlias}:
    A plain module alias of Empty
*)

(** Here is a table of links to indexes: indexlist
*)

(** Here is some superscript: x{^ 2}
*)

(** Here is some subscript: x{_ 0}
*)

(** Here are some escaped brackets: { [ @ ] }
*)

(** Here is some {e emphasis} followed by code.
*)

(** An unassociated comment
*)

(** {1:level-1 Level 1}
*)

(** {2:level-2 Level 2}
*)

(** {3:level-3 Level 3}
*)

(** {4:level-4 Level 4}
*)

(** {3:basic-module-stuff Basic module stuff}
*)

(**
  A plain, empty module
*)
module Empty : sig ... end

(**
  An ambiguous, misnamed module type
*)
module type Empty = sig
  
  type t
  end

(**
  An ambiguous, misnamed module type
*)
module type MissingComment = sig
  
  type t
  end

(** {1:s9000 Section 9000}
*)

(**
  A plain module alias of Empty
*)
module EmptyAlias = Empty

(** {3:emptySig EmptySig}
*)

(**
  A plain, empty module signature
*)
module type EmptySig = sig
  end

(**
  A plain, empty module signature alias of
*)
module type EmptySigAlias = sig
  end

(**
  A plain module of a signature of {!EmptySig} (reference)
*)
module ModuleWithSignature : EmptySig

(**
  A plain module with an alias signature
*)
module ModuleWithSignatureAlias : EmptySigAlias

module One : sig ... end

(**
  There's a signature in a module in this signature.
*)
module type SigForMod = sig
  
  module Inner : sig
    
    module type Empty = sig
      end
    end
  end

module type SuperSig = sig
  
  module type SubSigA = sig
    
    (** {7:subSig A Labeled Section Header Inside of a Signature}
    *)
    
    type t
    
    module SubSigAMod : sig
      
      type sub_sig_a_mod
      end
    end
  
  module type SubSigB = sig
    
    (** {7:subSig Another Labeled Section Header Inside of a Signature}
    *)
    
    type t
    end
  
  module type EmptySig = sig
    
    type not_actually_empty
    end
  
  module type One = sig
    
    type two
    end
  
  module type SuperSig = sig
    end
  end

(**
  For a good time, see SuperSig.SubSigA.subSig or SuperSig.SubSigB.subSig or 
  {!SuperSig.EmptySig}. Section {!Section 9000} is also interesting. 
  {!EmptySig} is the section and {!EmptySig} is the module signature.
*)

(**
  Buffer.t
*)
module Buffer : sig ... end

(** Some text before exception title.
*)

(** {3:basic-exception-stuff Basic exception stuff}
*)

(** After exception title.
*)

(**
  Unary exception constructor
*)
exception Kaboom of unit

(**
  Binary exception constructor
*)
exception Kablam of unit * unit

(**
  Unary exception constructor over binary tuple
*)
exception Kapow of unit * unit

(**
  {!EmptySig} is a module and {!EmptySig} is this exception.
*)
exception EmptySig

(**
  {!EmptySigAlias} is this exception.
*)
exception EmptySigAlias

(**
  {3:basic-type-and-value-stuff-with-advanced-doc-comments Basic type and value stuff with advanced doc comments}
*)

(**
  {!a_function} is this type and {!a_function} is the value below.
*)
type ('a, 'b) a_function = 'a -> 'b

(**
  This is a_function with param and return type.
  @parameter x:
    the x coordinate
  @returns:
    the y coordinate
*)
val a_function : x:int -> int

val fun_fun_fun : ((int, int) a_function, (unit, unit) a_function) a_function

val fun_maybe : ?yes:unit -> unit -> int

(**
  @raises Not_found:
    That's all it does
*)
val not_found : unit -> unit

(**
  @see {{: http://ocaml.org/}http://ocaml.org/}:
    The OCaml Web site
*)
val ocaml_org : string

(**
  @see some_file:
    The file called some_file
*)
val some_file : string

(**
  @see some_doc:
    The document called some_doc
*)
val some_doc : string

(**
  This value was introduced in the Mesozoic era.
  @since:
    mesozoic
*)
val since_mesozoic : unit

(**
  This value has had changes in 1.0.0, 1.1.0, and 1.2.0.
  @before 1.0.0:
    before 1.0.0
  @before 1.1.0:
    before 1.1.0
  @version:
    1.2.0
*)
val changing : unit

(** {3:some-operators Some Operators}
*)

val (~-) : unit

val (!) : unit

val (@) : unit

val ($) : unit

val (%) : unit

val (&) : unit

val (*) : unit

val (-) : unit

val (+) : unit

val (-?) : unit

val (/) : unit

val (:=) : unit

val (=) : unit

val (land) : unit

(** {3:advanced-module-stuff Advanced Module Stuff}
*)

(**
  This comment is for CollectionModule.
*)
module CollectionModule : sig ... end

(**
  module type of
*)
module type COLLECTION = sig
  
  (** This comment is for CollectionModule.
  *)
  
  (**
    This comment is for collection.
  *)
  type collection
  
  type element
  
  (**
    This comment is for InnerModuleA.
  *)
  module InnerModuleA : sig
    
    (**
      This comment is for t.
    *)
    type t = collection
    
    (**
      This comment is for InnerModuleA'.
    *)
    module InnerModuleA' : sig
      
      (**
        This comment is for t.
      *)
      type t = (unit, unit) a_function
      end
    
    (**
      This comment is for InnerModuleTypeA'.
    *)
    module type InnerModuleTypeA' = sig
      
      (**
        This comment is for t.
      *)
      type t = InnerModuleA'.t
      end
    end
  
  (**
    This comment is for InnerModuleTypeA.
  *)
  module type InnerModuleTypeA = sig
    
    (**
      This comment is for t.
    *)
    type t = InnerModuleA.InnerModuleA'.t
    end
  end

module Recollection (C : COLLECTION) : COLLECTION with type collection = C.element list and type element = C.collection

module type MMM = sig
  
  module C : sig
    
    (** This comment is for CollectionModule.
    *)
    
    (**
      This comment is for collection.
    *)
    type collection
    
    type element
    
    (**
      This comment is for InnerModuleA.
    *)
    module InnerModuleA : sig
      
      (**
        This comment is for t.
      *)
      type t = collection
      
      (**
        This comment is for InnerModuleA'.
      *)
      module InnerModuleA' : sig
        
        (**
          This comment is for t.
        *)
        type t = (unit, unit) a_function
        end
      
      (**
        This comment is for InnerModuleTypeA'.
      *)
      module type InnerModuleTypeA' = sig
        
        (**
          This comment is for t.
        *)
        type t = InnerModuleA'.t
        end
      end
    
    (**
      This comment is for InnerModuleTypeA.
    *)
    module type InnerModuleTypeA = sig
      
      (**
        This comment is for t.
      *)
      type t = InnerModuleA.InnerModuleA'.t
      end
    end
  end

module type RECOLLECTION = sig
  
  module C = Recollection(CollectionModule)
  end

module type RecollectionModule = sig
  
  type collection = CollectionModule.element list
  
  type element = CollectionModule.collection
  
  (**
    This comment is for InnerModuleA.
  *)
  module InnerModuleA : sig
    
    (**
      This comment is for t.
    *)
    type t = collection
    
    (**
      This comment is for InnerModuleA'.
    *)
    module InnerModuleA' : sig
      
      (**
        This comment is for t.
      *)
      type t = (unit, unit) a_function
      end
    
    (**
      This comment is for InnerModuleTypeA'.
    *)
    module type InnerModuleTypeA' = sig
      
      (**
        This comment is for t.
      *)
      type t = InnerModuleA'.t
      end
    end
  
  (**
    This comment is for InnerModuleTypeA.
  *)
  module type InnerModuleTypeA = sig
    
    (**
      This comment is for t.
    *)
    type t = InnerModuleA.InnerModuleA'.t
    end
  end

module type A = sig
  
  type t
  
  module Q : sig
    
    (** This comment is for CollectionModule.
    *)
    
    (**
      This comment is for collection.
    *)
    type collection
    
    type element
    
    (**
      This comment is for InnerModuleA.
    *)
    module InnerModuleA : sig
      
      (**
        This comment is for t.
      *)
      type t = collection
      
      (**
        This comment is for InnerModuleA'.
      *)
      module InnerModuleA' : sig
        
        (**
          This comment is for t.
        *)
        type t = (unit, unit) a_function
        end
      
      (**
        This comment is for InnerModuleTypeA'.
      *)
      module type InnerModuleTypeA' = sig
        
        (**
          This comment is for t.
        *)
        type t = InnerModuleA'.t
        end
      end
    
    (**
      This comment is for InnerModuleTypeA.
    *)
    module type InnerModuleTypeA = sig
      
      (**
        This comment is for t.
      *)
      type t = InnerModuleA.InnerModuleA'.t
      end
    end
  end

module type B = sig
  
  type t
  
  module Q : sig
    
    (** This comment is for CollectionModule.
    *)
    
    (**
      This comment is for collection.
    *)
    type collection
    
    type element
    
    (**
      This comment is for InnerModuleA.
    *)
    module InnerModuleA : sig
      
      (**
        This comment is for t.
      *)
      type t = collection
      
      (**
        This comment is for InnerModuleA'.
      *)
      module InnerModuleA' : sig
        
        (**
          This comment is for t.
        *)
        type t = (unit, unit) a_function
        end
      
      (**
        This comment is for InnerModuleTypeA'.
      *)
      module type InnerModuleTypeA' = sig
        
        (**
          This comment is for t.
        *)
        type t = InnerModuleA'.t
        end
      end
    
    (**
      This comment is for InnerModuleTypeA.
    *)
    module type InnerModuleTypeA = sig
      
      (**
        This comment is for t.
      *)
      type t = InnerModuleA.InnerModuleA'.t
      end
    end
  end

(**
  This module type includes two signatures.
*)
module type C = sig
  
  type t
  
  module Q : sig
    
    (** This comment is for CollectionModule.
    *)
    
    (**
      This comment is for collection.
    *)
    type collection
    
    type element
    
    (**
      This comment is for InnerModuleA.
    *)
    module InnerModuleA : sig
      
      (**
        This comment is for t.
      *)
      type t = collection
      
      (**
        This comment is for InnerModuleA'.
      *)
      module InnerModuleA' : sig
        
        (**
          This comment is for t.
        *)
        type t = (unit, unit) a_function
        end
      
      (**
        This comment is for InnerModuleTypeA'.
      *)
      module type InnerModuleTypeA' = sig
        
        (**
          This comment is for t.
        *)
        type t = InnerModuleA'.t
        end
      end
    
    (**
      This comment is for InnerModuleTypeA.
    *)
    module type InnerModuleTypeA = sig
      
      (**
        This comment is for t.
      *)
      type t = InnerModuleA.InnerModuleA'.t
      end
    end
  
  
  end

(**
  This comment is for FunctorTypeOf.
*)
module FunctorTypeOf (Collection : module type of CollectionModule) : sig ... end

(**
  This comment is for IncludeModuleType.
*)
module type IncludeModuleType = sig
  
  
  end

module type ToInclude = sig
  
  module IncludedA : sig
    
    type t
    end
  
  module type IncludedB = sig
    
    type s
    end
  end

module IncludedA : sig ... end

module type IncludedB = sig
  
  type s
  end

(** {3:advanced-type-stuff Advanced Type Stuff}
*)

(**
  This comment is for record.
  This comment is also for record.
*)
type record = {
  field1 : int;
  (**
    This comment is for field1.
  *)
  field2 : int;
  (**
    This comment is for field2.
  *)}

type mutable_record = {
  mutable a : int;
  (**
    a is first and mutable
  *)
  b : unit;
  (**
    b is second and immutable
  *)
  mutable c : int;
  (**
    c is third and mutable
  *)}

type universe_record = {
  nihilate : a. 'a -> unit;}

(**
  This comment is for variant.
  This comment is also for variant.
*)
type variant = 
  | TagA
  (**
    This comment is for TagA.
  *)
  | ConstrB of int
  (**
    This comment is for ConstrB.
  *)
  | ConstrC of int * int
  (**
    This comment is for binary ConstrC.
  *)
  | ConstrD of int * int
  (**
    This comment is for unary ConstrD of binary tuple.
  *)

(**
  This comment is for poly_variant.
  Wow! It was a polymorphic variant!
*)
type poly_variant = [ 
  | `TagA
  | `ConstrB of int ]

(**
  This comment is for full_gadt.
  Wow! It was a GADT!
*)
type (_, _) full_gadt = 
  | Tag : (unit, unit) full_gadt
  | First : 'a -> ('a, unit) full_gadt
  | Second : 'a -> (unit, 'a) full_gadt
  | Exist : 'a * 'b -> ('b, unit) full_gadt

(**
  This comment is for partial_gadt.
  Wow! It was a mixed GADT!
*)
type 'a partial_gadt = 
  | AscribeTag : 'a partial_gadt
  | OfTag of 'a partial_gadt
  | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt

(**
  This comment is for alias.
*)
type alias = variant

(**
  This comment is for tuple.
*)
type tuple = (alias * alias) * alias * (alias * alias)

(**
  This comment is for variant_alias.
*)
type variant_alias = variant = 
  | TagA
  | ConstrB of int
  | ConstrC of int * int
  | ConstrD of int * int

(**
  This comment is for record_alias.
*)
type record_alias = record = {
  field1 : int;
  field2 : int;}

(**
  This comment is for poly_variant_union.
*)
type poly_variant_union = [ 
  | poly_variant
  | `TagC ]

type 'a poly_poly_variant = [ 
  | `TagA of 'a ]

type ('a, 'b) bin_poly_poly_variant = [ 
  | `TagA of 'a
  | `ConstrB of 'b ]

type 'a open_poly_variant = [> `TagA ] as 'a

type 'a open_poly_variant2 = [> `ConstrB of int ] as 'a

type 'a open_poly_variant_alias = 'a open_poly_variant open_poly_variant2

type 'a poly_fun = [> `ConstrB of int ] as 'a -> 'a

type 'a poly_fun_constraint = 'a -> 'a constraint 'a = [> `TagA ]

type 'a closed_poly_variant = [< `One | `Two ] as 'a

type 'a clopen_poly_variant = [< `One | `Two of int | `Three Two Three ] as 'a

type nested_poly_variant = [ 
  | `A
  | `B of [ `B1 | `B2 ]
  | `C
  | `D of [ `D1 of [ `D1a ] ] ]

(**
  This comment is for full_gadt_alias.
*)
type ('a, 'b) full_gadt_alias = ('a, 'b) full_gadt = 
  | Tag : (unit, unit) full_gadt_alias
  | First : 'a -> ('a, unit) full_gadt_alias
  | Second : 'a -> (unit, 'a) full_gadt_alias
  | Exist : 'a * 'b -> ('b, unit) full_gadt_alias

(**
  This comment is for partial_gadt_alias.
*)
type 'a partial_gadt_alias = 'a partial_gadt = 
  | AscribeTag : 'a partial_gadt_alias
  | OfTag of 'a partial_gadt_alias
  | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt_alias

(**
  This comment is for {!Exn_arrow}.
*)
exception Exn_arrow : unit -> exn

(**
  This comment is for {!mutual_constr_a} then {!mutual_constr_b}.
*)
type mutual_constr_a = 
  | A
  | B_ish of mutual_constr_b
  (**
    This comment is between {!mutual_constr_a} and {!mutual_constr_b}.
  *)

(**
  This comment is for {!mutual_constr_b} then {!mutual_constr_a}.
*)
and mutual_constr_b = 
  | B
  | A_ish of mutual_constr_a
  (**
    This comment must be here for the next to associate correctly.
  *)

type rec_obj = < f : int; g : unit -> unit; h : rec_obj; >

type 'a open_obj = < f : int; g : unit -> unit; .. > as 'a

type 'a oof = < a : unit; .. > as 'a -> 'a

type 'a any_obj = < .. > as 'a

type empty_obj = < >

type one_meth = < meth : unit; >

(**
  A mystery wrapped in an ellipsis
*)
type ext = ..

type ext += 
  | ExtA

type ext += 
  | ExtB

type ext += 
  | ExtC of unit
  | ExtD of ext

type ext += 
  | ExtE

type ext += 
  | ExtF

(**
  'a poly_ext
*)
type 'a poly_ext = ..

type poly_ext += 
  | Foo of 'b
  | Bar of 'b * 'b
  (**
    'b poly_ext
  *)

type poly_ext += 
  | Quux of 'c
  (**
    'c poly_ext
  *)

module ExtMod : sig ... end

type ExtMod.t += 
  | ZzzTop0
  (**
    It's got the rock
  *)

type ExtMod.t += 
  | ZzzTop of unit
  (**
    and it packs a unit.
  *)

(**
  Rotate keys on my mark...
*)
val launch_missiles : unit -> unit

(**
  A brown paper package tied up with string
*)
type my_mod = (module COLLECTION)

class  empty_class : object ... end

class  one_method_class : object ... end

class  two_method_class : object ... end

class 'a param_class : 'a -> object ... end

type my_unit_object = unit param_class

type 'a my_unit_class = unit param_class as 'a

module Dep1 : sig ... end

module Dep2 (Arg : sig ... end) : sig ... end

type dep1 = Dep2(Dep1).B.c

module Dep3 : sig ... end

module Dep4 : sig ... end

module Dep5 (Arg : sig ... end) : sig ... end

type dep2 = Dep5(Dep4).Z.X.b

type dep3 = Dep5(Dep4).Z.Y.a

module Dep6 : sig ... end

module Dep7 (Arg : sig ... end) : sig ... end

type dep4 = Dep7(Dep6).M.Y.d

module Dep8 : sig ... end

module Dep9 (X : sig ... end) : sig ... end

module type Dep10 = sig
  
  type t = int
  end

module Dep11 : sig ... end

module Dep12 (Arg : sig ... end) : sig ... end

module Dep13 : Dep12(Dep11).T

type dep5 = Dep13.c

module type With1 = sig
  
  module M : sig
    
    module type S
    end
  
  module N : M.S
  end

module With2 : sig ... end

module With3 : With1 with module M = With2

type with1 = With3.N.t

module With4 : With1 with module M := With2

type with2 = With4.N.t

module With5 : sig ... end

module With6 : sig ... end

module With7 (X : sig ... end) : sig ... end

module type With8 = sig
  
  module M : sig
    
    module type S = sig
      
      type t
      end
    
    module N : sig
      
      type t = With5.N.t
      end
    end
  end

module With9 : sig ... end

module With10 : sig ... end

module type With11 = sig
  
  module M = With9
  
  module N : sig
    
    type t = int
    end
  end

module type NestedInclude1 = sig
  
  module type NestedInclude2 = sig
    
    type nested_include
    end
  end

module type NestedInclude2 = sig
  
  type nested_include
  end

type nested_include = int

module DoubleInclude1 : sig ... end

module DoubleInclude3 : sig ... end

type double_include

module IncludeInclude1 : sig ... end

module type IncludeInclude2 = sig
  
  type include_include
  end

type include_include

(** {1:indexmodules Trying the {!modules: ...} command.}
*)

(**
  With ocamldoc, toplevel units will be linked and documented, while submodules will behave as simple references.
  With odoc, everything should be resolved (and linked) but only toplevel units will be documented.
  @{!Dep1.X}:
    @DocOckTypes:
      @{!Ocamlary.IncludeInclude1}:
        @{!Ocamlary}:
          This is an {i interface} with {b all} of the {e module system} features. This documentation demonstrates:
*)

(**
  {3:weirder-usages-involving-module-types Weirder usages involving module types}
*)

(** @IncludeInclude1.IncludeInclude2:
      @Dep4.T:
        @{!A.Q}:
          
*)

(** {1:playing-with-@canonical-paths Playing with @canonical paths}
*)

module CanonicalTest : sig ... end

(**
  Some ref to {!CanonicalTest.Base__Tests.C.t} and {!CanonicalTest.Base__Tests.L.id}. But also to 
  {!CanonicalTest.Base__.List} and {!CanonicalTest.Base__.List.t}
*)
val test : 'a CanonicalTest.Base.List.t -> unit

(** {1:aliases Aliases again}
*)

(**
  Let's imitate jst's layout.
*)
module Aliases : sig ... end

(** {1:section-title-splicing Section title splicing}
*)

(** I can refer to
  -
    {!section:indexmodules} : {!Trying the {!modules: ...} command.}
  -
    {!aliases} : {!Aliases again}
  But also to things in submodules:
  -
    {!section:SuperSig.SubSigA.subSig} : SuperSig.SubSigA.subSig
  -
    {!Aliases.incl} : {!Aliases:incl}
  And just to make sure we do not mess up:-
                                            {{!section:indexmodules}A} : 
                                            {!A}
  -
    {{!aliases}B} : {!B}
  -
    {{!section:SuperSig.SubSigA.subSig}C} : {!C}
  -
    {{!Aliases.incl}D} : {!D}
*)

(** {1:new-reference-syntax New reference syntax}
*)

module type M = sig
  
  type t
  end

module M : sig ... end

(** Here goes:-
                {!module-M.t} : {!M.t}-
                                        {!module-type-M.t} : {!M.t}
*)

module Only_a_module : sig ... end

(** Some here should fail:-
                            {!Only_a_module.t} : {!Only_a_module.t}
  -
    {!module-Only_a_module.t} : {!Only_a_module.t}
  -
    {!module-type-Only_a_module.t} : Only_a_module.t : {!test}
*)

module type TypeExt = sig
  
  type t = ..
  
  type t += 
    | C
  
  val f : t -> unit
  end

type new_t = ..

type new_t += 
  | C

module type TypeExtPruned = sig
  
  type new_t += 
    | C
  
  val f : new_t -> unit
  end
