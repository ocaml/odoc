(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *                    Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** This is an {i interface} with {b all} of the {e module system} features.
    This documentation demonstrates:
- comment formatting
- unassociated comments
- documentation sections
- module system documentation including
 {ol
 {- submodules}
 {- module aliases}
 {- module types}
 {- module type aliases}
 {- modules with signatures}
 {- modules with aliased signatures}
}

A numbered list:
+ 3
+ 2
+ 1

    David Sheets is the author.
    @author David Sheets
*)

(**
    You may find more information about this HTML documentation renderer
    at {{:https://github.com/dsheets/ocamlary} github.com/dsheets/ocamlary }.
*)

(**
   This is some verbatim text:
   {v verbatim v}
*)

(**
    This is some verbatim text:
    {v [][df[]]}} v}
*)

(**
    Here is some raw LaTeX:
    {%latex: $e^{i\pi} = -1$ %}
*)

(**
    Here is an index table of [Empty] modules:
    {!modules: Empty EmptyAlias}
*)

(**
    Odoc doesn't support [{!indexlist}].
*)

(**
    Here is some superscript: x{^2}
*)

(**
    Here is some subscript: x{_0}
*)

(**
    Here are some escaped brackets: \{ \[ \@ \] \}
*)

(** Here is some {e emphasis} [followed by code]. *)

(** An unassociated comment *)

(******************************************************************************)

(** {1 Level 1 } *)

(** {2 Level 2 } *)

(** {3 Level 3 } *)

(** {4 Level 4 } *)

(** {3 Basic module stuff} *)

(** A plain, empty module *)
module Empty : sig end
(** This module has a signature without any members. *)

(** An ambiguous, misnamed module type *)
module type Empty = sig
  type t
end

(** An ambiguous, misnamed module type *)
module type MissingComment = sig
  type t
end

(** {1:s9000 Section 9000 } *)

module EmptyAlias = Empty
(** A plain module alias of [Empty] *)

(** {3:emptySig EmptySig} *)

module type EmptySig = sig end
(** A plain, empty module signature *)

module type EmptySigAlias = EmptySig
(** A plain, empty module signature alias of
    {[EmptySig]}
    (preformatted). *)

module ModuleWithSignature : EmptySig
(** A plain module of a signature of {!module-type:EmptySig} (reference) *)

module ModuleWithSignatureAlias : EmptySigAlias
(** A plain module with an alias signature
    @deprecated I don't like this element any more.
*)

module One : sig
  type one
end

(** There's a signature in a module in this signature. *)
module type SigForMod = sig
  module Inner : sig
    module type Empty = sig end
  end
end

module type SuperSig = sig
  module type SubSigA = sig
    (** {3:subSig A Labeled Section Header Inside of a Signature} *)

    type t

    module SubSigAMod : sig
      type sub_sig_a_mod
    end
  end

  module type SubSigB = sig
    (** {3:subSig Another Labeled Section Header Inside of a Signature} *)

    type t
  end

  module type EmptySig = sig
    type not_actually_empty
  end

  module type One = sig
    type two
  end

  module type SuperSig = sig end
end

(** For a good time, see
    {!SuperSig.SubSigA.subSig} or {!SuperSig.SubSigB.subSig} or
    {!SuperSig.EmptySig}. Section {!s9000} is also
    interesting.
    {!section:emptySig} is the section and {!module-type:EmptySig} is the
    module signature. *)

(** References are resolved after everything, so [{!Buffer.t}] won't resolve. *)
module Buffer : sig
  val f : int -> unit
end

(** Some text before exception title.

 {3 Basic exception stuff}

 After exception title. *)

exception Kaboom of unit
(** Unary exception constructor *)

exception Kablam of unit * unit
(** Binary exception constructor *)

exception Kapow of (unit * unit)
(** Unary exception constructor over binary tuple *)

exception EmptySig
(** {!module-type:EmptySig} is a module and
    {!exception:EmptySig} is this exception. *)

exception EmptySigAlias
(** {!exception:EmptySigAlias} is this exception. *)

(** {3 Basic type and value stuff with advanced doc comments } *)

type ('a, 'b) a_function = 'a -> 'b
(** {!type:a_function} is this type and
    {!val:a_function} is the value below. *)

val a_function : x:int -> int
(**
   This is [a_function] with param and return type.
   @param x the [x] coordinate
   @return the [y] coordinate
*)

val fun_fun_fun : ((int, int) a_function, (unit, unit) a_function) a_function

val fun_maybe : ?yes:unit -> unit -> int

val not_found : unit -> unit
(** @raise Not_found That's all it does *)

val kaboom : unit -> unit
(** @raise Kaboom That's all it does *)

val ocaml_org : string
(** @see <http://ocaml.org/> The OCaml Web site *)

val some_file : string
(** @see 'some_file' The file called [some_file] *)

val some_doc : string
(** @see "some_doc" The document called [some_doc] *)

val since_mesozoic : unit
(**
   This value was introduced in the Mesozoic era.
   @since mesozoic
*)

val changing : unit
(**
   This value has had changes in 1.0.0, 1.1.0, and 1.2.0.
   @before 1.0.0 before 1.0.0
   @before 1.1.0 before 1.1.0
   @version 1.2.0
*)

(** {3 Some Operators } *)

val ( ~- ) : unit

val ( ! ) : unit

val ( @ ) : unit

val ( $ ) : unit

val ( % ) : unit

(* Disabling the following four until we figure out what to do about
   https://github.com/ocsigen/tyxml/issues/264

   val ( ^ ) : unit

   val ( < ) : unit

   val ( > ) : unit

   val ( -| ) : unit *)

val ( & ) : unit

val ( * ) : unit

val ( - ) : unit

val ( + ) : unit

val ( -? ) : unit

val ( / ) : unit

val ( := ) : unit

val ( = ) : unit

val ( land ) : unit

(**/**)

(** I'm hidden *)

(**/**)

(** {3 Advanced Module Stuff} *)

(** This comment is for [CollectionModule]. *)
module CollectionModule : sig
  type collection
  (** This comment is for [collection]. *)

  type element

  (** This comment is for [InnerModuleA]. *)
  module InnerModuleA : sig
    type t = collection
    (** This comment is for [t]. *)

    (** This comment is for [InnerModuleA']. *)
    module InnerModuleA' : sig
      type t = (unit, unit) a_function
      (** This comment is for [t]. *)
    end

    (** This comment is for [InnerModuleTypeA']. *)
    module type InnerModuleTypeA' = sig
      type t = InnerModuleA'.t
      (** This comment is for [t]. *)
    end
  end

  module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
  (** This comment is for [InnerModuleTypeA]. *)
end

module type COLLECTION = module type of CollectionModule
(** module type of *)

module Recollection : functor (C : COLLECTION) ->
  COLLECTION
    with type collection = C.element list
     and type element = C.collection

module type MMM = sig
  module C : COLLECTION
end

module type RECOLLECTION = MMM with module C = Recollection(CollectionModule)

module type RecollectionModule = sig
  include module type of Recollection (CollectionModule)
end

module type A = sig
  type t

  module Q : COLLECTION
end

module type B = sig
  type t

  module Q : COLLECTION
end

(** This module type includes two signatures.
   {ul
   {- it includes {!module-type:A}}
   {- it includes {!module-type:B} with some substitution}} *)
module type C = sig
  include A

  include B with type t := t and module Q := Q
end

(* TODO: figure out why this doesn't work

   (** This comment is for [Functor]. *)
   module Functor(EmptyAlias : EmptySigAlias) : sig
     (** This comment is for [FunctorInner]. *)
     module FunctorInner = EmptyAlias
   end
*)

(** This comment is for [FunctorTypeOf]. *)
module FunctorTypeOf (Collection : module type of CollectionModule) : sig
  type t = Collection.collection
  (** This comment is for [t]. *)
end

(** This comment is for [IncludeModuleType]. *)
module type IncludeModuleType = sig
  include EmptySigAlias
  (** This comment is for [include EmptySigAlias]. *)
end

module type ToInclude = sig
  module IncludedA : sig
    type t
  end

  module type IncludedB = sig
    type s
  end
end

include ToInclude

(** {3 Advanced Type Stuff} *)

(** This comment is for [record]. *)
type record = {
  field1 : int;  (** This comment is for [field1]. *)
  field2 : int;  (** This comment is for [field2]. *)
}
(** This comment is also for [record]. *)

type mutable_record = {
  mutable a : int;  (** [a] is first and mutable *)
  b : unit;  (** [b] is second and immutable *)
  mutable c : int;  (** [c] is third and mutable *)
}

type universe_record = { nihilate : 'a. 'a -> unit }

(** This comment is for [variant]. *)
type variant =
  | TagA  (** This comment is for [TagA]. *)
  | ConstrB of int  (** This comment is for [ConstrB]. *)
  | ConstrC of int * int  (** This comment is for binary [ConstrC]. *)
  | ConstrD of (int * int)
      (** This comment is for unary [ConstrD] of binary tuple. *)
(** This comment is also for [variant]. *)

(** This comment is for [poly_variant]. *)
type poly_variant =
  [ `TagA
    (* This is a comment for [`TagA]. Disabled for compatibility
       with 4.02. *)
  | `ConstrB of int (* This is a comment for [`ConstrB] *) ]
(** Wow! It was a polymorphic variant! *)

(** This comment is for [full_gadt]. *)
type (_, _) full_gadt =
  | Tag : (unit, unit) full_gadt
  | First : 'a -> ('a, unit) full_gadt
  | Second : 'a -> (unit, 'a) full_gadt
  | Exist : 'a * 'b -> ('b, unit) full_gadt  (** *)
(** Wow! It was a GADT! *)

(** This comment is for [partial_gadt]. *)
type 'a partial_gadt =
  | AscribeTag : 'a partial_gadt
  | OfTag of 'a partial_gadt
  | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt  (** *)
(** Wow! It was a mixed GADT! *)

type alias = variant
(** This comment is for [alias]. *)

type tuple = (alias * alias) * alias * (alias * alias)
(** This comment is for [tuple]. *)

(** This comment is for [variant_alias]. *)
type variant_alias = variant =
  | TagA
  | ConstrB of int
  | ConstrC of int * int
  | ConstrD of (int * int)

type record_alias = record = { field1 : int; field2 : int }
(** This comment is for [record_alias]. *)

type poly_variant_union = [ poly_variant | `TagC ]
(** This comment is for [poly_variant_union]. *)

type 'a poly_poly_variant = [ `TagA of 'a ]

type ('a, 'b) bin_poly_poly_variant = [ `TagA of 'a | `ConstrB of 'b ]

(* TODO: figure out how to spec a conjunctive type
   type amb_poly_variant = [
   | unit poly_poly_variant
   | (int,unit) bin_poly_poly_variant
   | `TagC
   ]
*)

type 'a open_poly_variant = [> `TagA ] as 'a

type 'a open_poly_variant2 = [> `ConstrB of int ] as 'a

type 'a open_poly_variant_alias = 'a open_poly_variant open_poly_variant2

type 'a poly_fun = ([> `ConstrB of int ] as 'a) -> 'a

type 'a poly_fun_constraint = 'a -> 'a constraint 'a = [> `TagA ]

type 'a closed_poly_variant = [< `One | `Two ] as 'a

type 'a clopen_poly_variant =
  [< `One | `Two of int | `Three > `Two `Three ] as 'a

type nested_poly_variant =
  [ `A | `B of [ `B1 | `B2 ] | `C | `D of [ `D1 of [ `D1a ] ] ]

(** This comment is for [full_gadt_alias]. *)
type ('a, 'b) full_gadt_alias = ('a, 'b) full_gadt =
  | Tag : (unit, unit) full_gadt_alias
  | First : 'a -> ('a, unit) full_gadt_alias
  | Second : 'a -> (unit, 'a) full_gadt_alias
  | Exist : 'a * 'b -> ('b, unit) full_gadt_alias

(** This comment is for [partial_gadt_alias]. *)
type 'a partial_gadt_alias = 'a partial_gadt =
  | AscribeTag : 'a partial_gadt_alias
  | OfTag of 'a partial_gadt_alias
  | ExistGadtTag : ('a -> 'b) -> 'a partial_gadt_alias

exception Exn_arrow : unit -> exn
(** This comment is for {!Exn_arrow}. *)

(** This comment is for {!mutual_constr_a} then {!mutual_constr_b}. *)
type mutual_constr_a =
  | A
  | B_ish of mutual_constr_b
      (** This comment is between {!mutual_constr_a} and {!mutual_constr_b}. *)

(** This comment is for {!mutual_constr_b} then {!mutual_constr_a}. *)
and mutual_constr_b =
  | B
  | A_ish of mutual_constr_a
      (** This comment must be here for the next to associate correctly. *)

type rec_obj = < f : int ; g : unit -> unit ; h : rec_obj >

type 'a open_obj = < f : int ; g : unit -> unit ; .. > as 'a

type 'a oof = (< a : unit ; .. > as 'a) -> 'a

type 'a any_obj = < .. > as 'a

type empty_obj = < >

type one_meth = < meth : unit >

type ext = ..
(** A mystery wrapped in an ellipsis *)

type ext += ExtA

type ext += ExtB

type ext += ExtC of unit | ExtD of ext

type ext += ExtE

type ext += private ExtF

type 'a poly_ext = ..
(** 'a poly_ext *)

type 'b poly_ext += Foo of 'b | Bar of 'b * 'b  (** 'b poly_ext *)

type 'c poly_ext += Quux of 'c  (** 'c poly_ext *)

module ExtMod : sig
  type t = ..

  type t += Leisureforce
end

type ExtMod.t += ZzzTop0  (** It's got the rock *)

type ExtMod.t += ZzzTop of unit  (** and it packs a unit. *)

external launch_missiles : unit -> unit = "tetris"
(** Rotate keys on my mark... *)

type my_mod = (module COLLECTION)
(** A brown paper package tied up with string*)

class empty_class : object end

class one_method_class : object
  method go : unit
end

class two_method_class : object
  method one : one_method_class

  method undo : unit
end

class ['a] param_class : 'a -> object
  method v : 'a
end

type my_unit_object = unit param_class

type 'a my_unit_class = unit #param_class as 'a

(* Bug in compiler breaks this example on cmi's *)
(* class type my_unit_class_type = [unit] param_class *)

(* TODO: classes, class types, ...? *)

(* Test resolution of dependently typed modules *)
module Dep1 : sig
  module type S = sig
    class c : object
      method m : int
    end
  end

  module X : sig
    module Y : S
  end
end

module Dep2 : functor
  (Arg : sig
     module type S

     module X : sig
       module Y : S
     end
   end)
  -> sig
  module A : sig
    module Y : Arg.S
  end

  module B = A.Y
end

type dep1 = Dep2(Dep1).B.c

module Dep3 : sig
  type a
end

module Dep4 : sig
  module type T = sig
    type b
  end

  module type S = sig
    module X : T

    module Y : sig end
  end

  module X : T
end

module Dep5 : functor
  (Arg : sig
     module type T

     module type S = sig
       module X : T

       module Y : sig end
     end

     module X : T
   end)
  -> sig
  module Z : Arg.S with module Y = Dep3
end

type dep2 = Dep5(Dep4).Z.X.b

type dep3 = Dep5(Dep4).Z.Y.a

module Dep6 : sig
  module type S = sig
    type d
  end

  module type T = sig
    module type R = S

    module Y : R
  end

  module X : T
end

module Dep7 : functor
  (Arg : sig
     module type S

     module type T = sig
       module type R = S

       module Y : R
     end

     module X : T
   end)
  -> sig
  module M : Arg.T
end

type dep4 = Dep7(Dep6).M.Y.d

module Dep8 : sig
  module type T = sig
    type t
  end
end

module Dep9 : functor
  (X : sig
     module type T
   end)
  -> sig
  module type T = X.T
end

module type Dep10 = Dep9(Dep8).T with type t = int

module Dep11 : sig
  module type S = sig
    class c : object
      method m : int
    end
  end
end

module Dep12 : functor
  (Arg : sig
     module type S
   end)
  -> sig
  module type T = Arg.S
end

module Dep13 : Dep12(Dep11).T

type dep5 = Dep13.c

(* Test resolution of difficult with examples *)

module type With1 = sig
  module M : sig
    module type S
  end

  module N : M.S
end

module With2 : sig
  module type S = sig
    type t
  end
end

module With3 : With1 with module M = With2

type with1 = With3.N.t

module With4 : With1 with module M := With2

type with2 = With4.N.t

module With5 : sig
  module type S = sig
    type t
  end

  module N : S
end

module With6 : sig
  module type T = sig
    module M : sig
      module type S

      module N : S
    end
  end
end

module With7 : functor
  (X : sig
     module type T
   end)
  -> sig
  module type T = X.T
end

module type With8 =
  With7(With6).T with module M = With5 and type M.N.t = With5.N.t

module With9 : sig
  module type S = sig
    type t
  end
end

module With10 : sig
  (** {!With10.T} is a submodule type. *)
  module type T = sig
    module M : sig
      module type S
    end

    module N : M.S
  end
end

module type With11 = With7(With10).T with module M = With9 and type N.t = int

module type NestedInclude1 = sig
  module type NestedInclude2 = sig
    type nested_include
  end
end

include NestedInclude1

include NestedInclude2 with type nested_include = int

module DoubleInclude1 : sig
  module DoubleInclude2 : sig
    type double_include
  end
end

module DoubleInclude3 : sig
  include module type of DoubleInclude1
end

include module type of DoubleInclude3.DoubleInclude2

module IncludeInclude1 : sig
  module type IncludeInclude2 = sig
    type include_include
  end

  module IncludeInclude2_M : sig end
end

include module type of IncludeInclude1

include IncludeInclude2

(** {1:indexmodules Trying the \{!modules: ...\} command.}

    With ocamldoc, toplevel units will be linked and documented, while
    submodules will behave as simple references.

    With odoc, everything should be resolved (and linked) but only toplevel
    units will be documented.

    {!modules: Dep1.X Ocamlary.IncludeInclude1 Ocamlary}

    {3 Weirder usages involving module types}

    {!modules: IncludeInclude1.IncludeInclude2_M Dep4.X}
*)

(** {1 Playing with \@canonical paths} *)

module CanonicalTest : sig
  module Base__List : sig
    type 'a t

    val id : 'a t -> 'a t
  end

  module Base__ : sig
    module List = Base__List
    (** @canonical Ocamlary.CanonicalTest.Base.List *)
  end

  module Base : sig
    module List = Base__.List
  end

  module Base_Tests : sig
    module C : module type of Base__.List

    open Base__
    module L = List

    val foo : int L.t -> float L.t

    val bar : 'a List.t -> 'a List.t
    (* This is just {!List.id}, or rather {!L.id} *)

    val baz : 'a Base__.List.t -> unit
    (* We can't reference [Base__] because it's hidden.
        {!List.t} ([List.t]) should resolve. *)
  end

  module List_modif : module type of Base.List with type 'c t = 'c Base__.List.t
end

(* val test : 'a CanonicalTest.Base__.List.t -> unit *)
(** Some ref to {!CanonicalTest.Base_Tests.C.t} and {!CanonicalTest.Base_Tests.L.id}.
    But also to {!CanonicalTest.Base.List} and {!CanonicalTest.Base.List.t} *)

(** {1:aliases Aliases again} *)

module Aliases : sig
  (** Let's imitate jst's layout. *)

  module Foo__A : sig
    type t

    val id : t -> t
  end

  module Foo__B : sig
    type t

    val id : t -> t
  end

  module Foo__C : sig
    type t

    val id : t -> t
  end

  module Foo__D : sig
    type t

    val id : t -> t
  end

  module Foo__E : sig
    type t

    val id : t -> t
  end

  module Foo__ : sig
    module A = Foo__A
    (** @canonical Ocamlary.Aliases.Foo.A *)

    module B = Foo__B
    (** @canonical Ocamlary.Aliases.Foo.B *)

    module C = Foo__C
    (** @canonical Ocamlary.Aliases.Foo.C *)

    module D = Foo__D
    (** @canonical Ocamlary.Aliases.Foo.D *)

    module E = Foo__E
  end

  module Foo : sig
    open Foo__
    module A = A
    module B = B
    module C = C
    module D = D
    module E = E
  end

  module A' = Foo.A

  type tata = Foo.A.t

  type tbtb = Foo__.B.t

  type tete = Foo__.E.t

  type tata' = A'.t

  type tete2 = Foo.E.t

  module Std : sig
    module A = Foo.A
    module B = Foo.B
    module C = Foo.C
    module D = Foo.D
    module E = Foo.E
  end

  type stde = Std.E.t

  (** {3:incl include of Foo}

      Just for giggle, let's see what happens when we include {!module-Foo}. *)

  include module type of Foo

  type testa = A.t

  (** And also, let's refer to {!type:module-A.t} and {!Foo.B.id} *)

  module P1 : sig
    (** @canonical Ocamlary.Aliases.P2.Z *)
    module Y : sig
      type t

      val id : t -> t
    end
  end

  module P2 : sig
    module Z = P1.Y
  end

  module X1 = P1.Y
  module X2 = P2.Z

  type p1 = X1.t

  type p2 = X2.t
end

(** {1 Section title splicing}

    I can refer to
    - [{!section:indexmodules}] : {!section:indexmodules}
    - [{!aliases}] : {!aliases}

    But also to things in submodules:
    - [{!section:SuperSig.SubSigA.subSig}] : {!section:SuperSig.SubSigA.subSig}
    - [{!Aliases.incl}] : {!Aliases.incl}

    And just to make sure we do not mess up:
    - [{{!section:indexmodules}A}] : {{!section:indexmodules}A}
    - [{{!aliases}B}] : {{!aliases}B}
    - [{{!section:SuperSig.SubSigA.subSig}C}] :
    {{!section:SuperSig.SubSigA.subSig}C}
    - [{{!Aliases.incl}D}] : {{!Aliases.incl}D}
*)

(** {1 New reference syntax} *)

module type M = sig
  type t
end

module M : sig
  type t
end

(** Here goes:
    - [{!module-M.t}] : {!module-M.t}
    - [{!module-type-M.t}] : {!module-type-M.t} *)

module Only_a_module : sig
  type t
end

(** - [{!Only_a_module.t}] : {!Only_a_module.t}
    - [{!module-Only_a_module.t}] : {!module-Only_a_module.t}
    - [{!module-Only_a_module.type-t}] : {!module-Only_a_module.type-t}
    - [{!type:Only_a_module.t}] : {!type:Only_a_module.t} *)

module type TypeExt = sig
  type t = ..

  type t += C

  val f : t -> unit
end

type new_t = ..

type new_t += C

module type TypeExtPruned = TypeExt with type t := new_t

module Op : sig
  val ( let* ) : int
  val ( and* ) : int
  val ( .%{} ) : int
  val ( .%{}<- ) : int
  val ( .%{;..} ) : int
  val ( .%{;..}<- ) : int
  val ( !~ ) : int
  val ( #~ ) : int
end
