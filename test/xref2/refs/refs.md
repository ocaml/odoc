Testing resolution of references

## Env

Helpers:

```ocaml
open Odoc_model

let parse_ref ref_str =
  match Semantics.parse_reference ref_str |> Common.handle_warnings with
  | Ok ref -> ref
  | Error e -> failwith (Error.to_string e)

(* Shorten type for nicer output *)
type ref = Paths.Reference.Resolved.t

let resolve_ref' env ref_str : ref =
  let unresolved = parse_ref ref_str in
  let resolve () =
    Ref_tools.resolve_reference env unresolved |> Error.raise_warnings
  in
  match
    Lookup_failures.catch_failures ~filename:"<test>" resolve
    |> Common.handle_warnings
  with
  | Error e ->
      Format.kasprintf failwith "resolve_reference: %a"
        Errors.Tools_error.pp_reference_lookup_error e
  | Ok r -> r

let resolve_ref_of_mli mli =
  let sg = Common.signature_of_mli_string mli in
  let env = Env.open_signature sg Env.empty in
  resolve_ref' env
```

## Resolving

Env:

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  (** First comment *)

  (** {1:L1 title} *)

  type t1 = C1
  type r1 = { rf1 : int }
  val f1 : unit -> unit
  module type T1 = sig end
  exception E1
  external e1 : unit -> unit = "e1"
  class c1 : object
    val v1 : unit
    method m1 : unit
  end
  class type ct1 = object
    val tv1 : unit
    method tm1 : unit
  end
  type x1 = ..
  type x1 += X1

  module M : sig

    (** {1:L2 title2} *)

    type t2 = C2
    type r2 = { rf2 : int }
    val f2 : unit -> unit
    module type T2 = sig end
    exception E2
    external e2 : unit -> unit = "e2"
    class c2 : object
      val v2 : unit
      method m2 : unit
    end
    class type ct2 = object
      val tv2 : unit
      method tm2 : unit
    end
    type x2 = ..
    type x2 += X2
    module N : sig end
  end

|}
```

Explicit, root:

```ocaml
# resolve_ref "module:M" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Module
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       M);
   ihash = 716453475; ikey = "m_M.r_Root.p_None"}
# resolve_ref "module:M.N" ;;
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   N)
# resolve_ref "val:f1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       f1);
   ihash = 10878772; ikey = "v_f1.r_Root.p_None"}
# resolve_ref "type:t1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t1);
   ihash = 491588764; ikey = "t_t1.r_Root.p_None"}
# resolve_ref "module-type:T1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ModuleType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       T1);
   ihash = 936384557; ikey = "mt_T1.r_Root.p_None"}
# resolve_ref "exception:E1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Exception
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       E1);
   ihash = 509481329; ikey = "exn_E1.r_Root.p_None"}
# resolve_ref "constructor:C1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Constructor
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            t1);
        ihash = 491588764; ikey = "t_t1.r_Root.p_None"},
       C1);
   ihash = 1004467392; ikey = "ctor_C1.t_t1.r_Root.p_None"}
# resolve_ref "val:e1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       e1);
   ihash = 708293633; ikey = "v_e1.r_Root.p_None"}
# resolve_ref "class:c1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Class
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       c1);
   ihash = 391456819; ikey = "c_c1.r_Root.p_None"}
# resolve_ref "class-type:ct1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ClassType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       ct1);
   ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"}
# resolve_ref "type:x1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       x1);
   ihash = 421257688; ikey = "t_x1.r_Root.p_None"}
# resolve_ref "constructor:X1" (* X1 is an extension constructor *) ;;
Exception: Failure "resolve_reference: Couldn't find \"X1\"".
# resolve_ref "extension:X1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Extension
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       X1);
   ihash = 711917142; ikey = "extn_X1.r_Root.p_None"}
# resolve_ref "method:c1.m1" ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          c1);
      ihash = 391456819; ikey = "c_c1.r_Root.p_None"},
   m1)
# resolve_ref "instance-variable:c1.v1" ;;
- : ref =
`InstanceVariable
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          c1);
      ihash = 391456819; ikey = "c_c1.r_Root.p_None"},
   v1)
# resolve_ref "method:ct1.tm1" (* ct1 is a class type *) ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ClassType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          ct1);
      ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"},
   tm1)
# resolve_ref "instance-variable:ct1.tv1" ;;
- : ref =
`InstanceVariable
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ClassType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          ct1);
      ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"},
   tv1)
# resolve_ref "field:rf1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Field
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            r1);
        ihash = 489530714; ikey = "t_r1.r_Root.p_None"},
       rf1);
   ihash = 85235953; ikey = "fld_rf1.t_r1.r_Root.p_None"}
# resolve_ref "field:r1.rf1" ;;
- : ref =
`Field
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          r1);
      ihash = 489530714; ikey = "t_r1.r_Root.p_None"},
   rf1)
# resolve_ref "section:L1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Label
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       L1);
   ihash = 612985552; ikey = "l_L1.r_Root.p_None"}
```

Explicit, in sig:

```ocaml
# resolve_ref "val:M.f2" ;;
- : ref =
`Value
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   f2)
# resolve_ref "type:M.t2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   t2)
# resolve_ref "module-type:M.T2" ;;
- : ref =
`ModuleType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   T2)
# resolve_ref "exception:M.E2" ;;
- : ref =
`Exception
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   E2)
# resolve_ref "constructor:M.C2" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      t2),
   C2)
# resolve_ref "val:M.e2" ;;
- : ref =
`Value
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   e2)
# resolve_ref "class:M.c2" ;;
- : ref =
`Class
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   c2)
# resolve_ref "class-type:M.ct2" ;;
- : ref =
`ClassType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   ct2)
# resolve_ref "type:M.x2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   x2)
# resolve_ref "constructor:M.X2" (* X2 is an extension constructor *) ;;
Exception: Failure "resolve_reference: Couldn't find \"X2\"".
# resolve_ref "extension:M.X2" ;;
- : ref =
`Extension
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   X2)
# resolve_ref "method:M.c2.m2" ;;
- : ref =
`Method
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   m2)
# resolve_ref "instance-variable:M.c2.v2" ;;
- : ref =
`InstanceVariable
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   v2)
# resolve_ref "method:M.ct2.tm2" (* ct2 is a class type *) ;;
- : ref =
`Method
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tm2)
# resolve_ref "instance-variable:M.ct2.tv2" ;;
- : ref =
`InstanceVariable
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tv2)
# resolve_ref "field:M.rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "field:M.r2.rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "section:M.L2" ;;
- : ref =
`Label
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   L2)
```

Implicit, root:

```ocaml
# resolve_ref "M" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Module
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       M);
   ihash = 716453475; ikey = "m_M.r_Root.p_None"}
# resolve_ref "M.N" ;;
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   N)
# resolve_ref "f1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       f1);
   ihash = 10878772; ikey = "v_f1.r_Root.p_None"}
# resolve_ref "t1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t1);
   ihash = 491588764; ikey = "t_t1.r_Root.p_None"}
# resolve_ref "T1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ModuleType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       T1);
   ihash = 936384557; ikey = "mt_T1.r_Root.p_None"}
# resolve_ref "E1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Exception
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       E1);
   ihash = 509481329; ikey = "exn_E1.r_Root.p_None"}
# resolve_ref "C1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Constructor
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            t1);
        ihash = 491588764; ikey = "t_t1.r_Root.p_None"},
       C1);
   ihash = 1004467392; ikey = "ctor_C1.t_t1.r_Root.p_None"}
# resolve_ref "e1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       e1);
   ihash = 708293633; ikey = "v_e1.r_Root.p_None"}
# resolve_ref "c1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Class
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       c1);
   ihash = 391456819; ikey = "c_c1.r_Root.p_None"}
# resolve_ref "ct1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ClassType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       ct1);
   ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"}
# resolve_ref "x1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       x1);
   ihash = 421257688; ikey = "t_x1.r_Root.p_None"}
# resolve_ref "X1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Extension
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       X1);
   ihash = 711917142; ikey = "extn_X1.r_Root.p_None"}
# resolve_ref "c1.m1" ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          c1);
      ihash = 391456819; ikey = "c_c1.r_Root.p_None"},
   m1)
# resolve_ref "c1.v1" ;;
- : ref =
`InstanceVariable
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          c1);
      ihash = 391456819; ikey = "c_c1.r_Root.p_None"},
   v1)
# resolve_ref "ct1.tm1" (* ct1 is a class type *) ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ClassType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          ct1);
      ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"},
   tm1)
# resolve_ref "ct1.tv1" ;;
- : ref =
`InstanceVariable
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ClassType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          ct1);
      ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"},
   tv1)
# resolve_ref "rf1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Field
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            r1);
        ihash = 489530714; ikey = "t_r1.r_Root.p_None"},
       rf1);
   ihash = 85235953; ikey = "fld_rf1.t_r1.r_Root.p_None"}
# resolve_ref "r1.rf1" ;;
- : ref =
`Field
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          r1);
      ihash = 489530714; ikey = "t_r1.r_Root.p_None"},
   rf1)
# resolve_ref "L1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Label
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       L1);
   ihash = 612985552; ikey = "l_L1.r_Root.p_None"}
```

Implicit, in sig:

```ocaml
# resolve_ref "M.f2" ;;
- : ref =
`Value
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   f2)
# resolve_ref "M.t2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   t2)
# resolve_ref "M.T2" ;;
- : ref =
`ModuleType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   T2)
# resolve_ref "M.E2" ;;
- : ref =
`Exception
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   E2)
# resolve_ref "M.C2" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      t2),
   C2)
# resolve_ref "M.e2" ;;
- : ref =
`Value
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   e2)
# resolve_ref "M.c2" ;;
- : ref =
`Class
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   c2)
# resolve_ref "M.ct2" ;;
- : ref =
`ClassType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   ct2)
# resolve_ref "M.x2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   x2)
# resolve_ref "M.X2" ;;
- : ref =
`Extension
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   X2)
# resolve_ref "M.c2.m2" ;;
- : ref =
`Method
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   m2)
# resolve_ref "M.c2.v2" ;;
- : ref =
`InstanceVariable
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   v2)
# resolve_ref "M.ct2.tm2" (* ct2 is a class type *) ;;
- : ref =
`Method
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tm2)
# resolve_ref "M.ct2.tv2" ;;
- : ref =
`InstanceVariable
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tv2)
# resolve_ref "M.rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "M.r2.rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "M.L2" ;;
- : ref =
`Label
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   L2)
```

Known kind:

```ocaml
# resolve_ref "module-M" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Module
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       M);
   ihash = 716453475; ikey = "m_M.r_Root.p_None"}
# resolve_ref "module-M.N" ;;
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   N)
# resolve_ref "M.module-N" ;;
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   N)
# resolve_ref "module-M.module-N" ;;
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   N)
# resolve_ref "type-t1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t1);
   ihash = 491588764; ikey = "t_t1.r_Root.p_None"}
# resolve_ref "module-type-T1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ModuleType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       T1);
   ihash = 936384557; ikey = "mt_T1.r_Root.p_None"}
# resolve_ref "class-c1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Class
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       c1);
   ihash = 391456819; ikey = "c_c1.r_Root.p_None"}
# resolve_ref "class-type-ct1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ClassType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       ct1);
   ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"}
# resolve_ref "type-x1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       x1);
   ihash = 421257688; ikey = "t_x1.r_Root.p_None"}
# resolve_ref "class-c1.m1" ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          c1);
      ihash = 391456819; ikey = "c_c1.r_Root.p_None"},
   m1)
# resolve_ref "class-c1.v1" ;;
- : ref =
`InstanceVariable
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          c1);
      ihash = 391456819; ikey = "c_c1.r_Root.p_None"},
   v1)
# resolve_ref "class-type-ct1.tm1" ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ClassType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          ct1);
      ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"},
   tm1)
# resolve_ref "class-type-ct1.tv1" ;;
- : ref =
`InstanceVariable
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ClassType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          ct1);
      ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"},
   tv1)
# resolve_ref "field-rf1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Field
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            r1);
        ihash = 489530714; ikey = "t_r1.r_Root.p_None"},
       rf1);
   ihash = 85235953; ikey = "fld_rf1.t_r1.r_Root.p_None"}
# resolve_ref "type-r1.rf1" ;;
- : ref =
`Field
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          r1);
      ihash = 489530714; ikey = "t_r1.r_Root.p_None"},
   rf1)
# resolve_ref "type-r1.field-rf1" ;;
- : ref =
`Field
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          r1);
      ihash = 489530714; ikey = "t_r1.r_Root.p_None"},
   rf1)
# resolve_ref "section-L1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Label
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       L1);
   ihash = 612985552; ikey = "l_L1.r_Root.p_None"}
# resolve_ref "M.type-t2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   t2)
# resolve_ref "M.module-type-T2" ;;
- : ref =
`ModuleType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   T2)
# resolve_ref "M.class-c2" ;;
- : ref =
`Class
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   c2)
# resolve_ref "M.class-type-ct2" ;;
- : ref =
`ClassType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   ct2)
# resolve_ref "M.type-x2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   x2)
# resolve_ref "M.class-c2.m2" ;;
- : ref =
`Method
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   m2)
# resolve_ref "M.class-c2.v2" ;;
- : ref =
`InstanceVariable
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   v2)
# resolve_ref "M.class-type-ct2.tm2" ;;
- : ref =
`Method
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tm2)
# resolve_ref "M.class-type-ct2.tv2" ;;
- : ref =
`InstanceVariable
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tv2)
# resolve_ref "M.type-r2.rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "M.r2.field-rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "M.type-r2.field-rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "M.field-rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "M.section-L2" ;;
- : ref =
`Label
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   L2)
# resolve_ref "module-M.type-t2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   t2)
# resolve_ref "module-M.module-type-T2" ;;
- : ref =
`ModuleType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   T2)
# resolve_ref "module-M.class-c2" ;;
- : ref =
`Class
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   c2)
# resolve_ref "module-M.class-type-ct2" ;;
- : ref =
`ClassType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   ct2)
# resolve_ref "module-M.type-x2" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   x2)
# resolve_ref "module-M.class-c2.m2" ;;
- : ref =
`Method
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   m2)
# resolve_ref "module-M.class-c2.v2" ;;
- : ref =
`InstanceVariable
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      c2),
   v2)
# resolve_ref "module-M.class-type-ct2.tm2" ;;
- : ref =
`Method
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tm2)
# resolve_ref "module-M.class-type-ct2.tv2" ;;
- : ref =
`InstanceVariable
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      ct2),
   tv2)
# resolve_ref "module-M.type-r2.rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "module-M.field-rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
# resolve_ref "module-M.section-L2" ;;
- : ref =
`Label
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   L2)
# resolve_ref "module-M.field-rf2" ;;
- : ref =
`Field
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      r2),
   rf2)
```

Class and class type as type:

```ocaml
# resolve_ref "type:c1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Class
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       c1);
   ihash = 391456819; ikey = "c_c1.r_Root.p_None"}
# resolve_ref "type:ct1" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ClassType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       ct1);
   ihash = 611389945; ikey = "ct_ct1.r_Root.p_None"}
```

Constructors in type parent:

```ocaml
# resolve_ref "t1.C1" ;;
- : ref =
`Constructor
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          t1);
      ihash = 491588764; ikey = "t_t1.r_Root.p_None"},
   C1)
# resolve_ref "constructor:t1.C1" ;;
- : ref =
`Constructor
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          t1);
      ihash = 491588764; ikey = "t_t1.r_Root.p_None"},
   C1)
# resolve_ref "t1.constructor-C1" ;;
- : ref =
`Constructor
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          t1);
      ihash = 491588764; ikey = "t_t1.r_Root.p_None"},
   C1)
# resolve_ref "constructor:type-t1.C1" ;;
- : ref =
`Constructor
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Type
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          t1);
      ihash = 491588764; ikey = "t_t1.r_Root.p_None"},
   C1)
# resolve_ref "M.t2.C2" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      t2),
   C2)
# resolve_ref "constructor:M.t2.C2" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      t2),
   C2)
# resolve_ref "M.t2.constructor-C2" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      t2),
   C2)
# resolve_ref "constructor:M.type-t2.C2" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      t2),
   C2)
```

Signature parent:

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module M : sig
    module N : sig
      type t
    end
    module type T = sig
      type t
    end
  end
  module type MT = sig
    type t
  end
|}
```

```ocaml
# resolve_ref "M.module-N.t" ;;
- : ref =
`Type
  (`Module
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      N),
   t)
# resolve_ref "M.module-type-T.t" ;;
- : ref =
`Type
  (`ModuleType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      T),
   t)
# resolve_ref "M.N.type-t" ;;
- : ref =
`Type
  (`Module
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      N),
   t)
# resolve_ref "M.T.type-t" ;;
- : ref =
`Type
  (`ModuleType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      T),
   t)
# resolve_ref "type:M.N.t" ;;
- : ref =
`Type
  (`Module
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      N),
   t)
# resolve_ref "type:MT.t" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ModuleType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          MT);
      ihash = 848305771; ikey = "mt_MT.r_Root.p_None"},
   t)
```

Substitutions are only available in 4.08 onwards:

<!-- $MDX version>=4.08 -->
```ocaml
let resolve_ref = resolve_ref_of_mli {|
  type r1 = { rf1 : int }
  type s1 := r1

  module M : sig
    type r2 = { rf2 : int }
    type s2 := r2
  end
|}
```

<!-- $MDX version>=4.08 -->
```ocaml
# resolve_ref "s1" ;;
Exception: Failure "resolve_reference: Couldn't find \"s1\"".
# resolve_ref "s1.rf1" ;;
Exception: Failure "resolve_reference: Couldn't find \"s1\"".
# resolve_ref "M.s2" ;;
Exception: Failure "resolve_reference: Couldn't find \"s2\"".
# resolve_ref "M.s2.rf2" ;;
Exception: Failure "resolve_reference: Couldn't find \"s2\"".
```

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module type T1 = sig
    type t
  end

  module type T2 = sig
    module N : T1
  end

  module A : T1
  module B = A
  module C : module type of A
  module D : T1 with type t = A.t
  module E : T2 with module N = A
|}
```

```ocaml
# resolve_ref "A.t" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          A);
      ihash = 353272258; ikey = "m_A.r_Root.p_None"},
   t)
# resolve_ref "B.t" (* get_module_path_modifiers is [`Aliased] *) ;;
- : ref =
`Type
  (`Alias
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             A);
         ihash = 353272258; ikey = "m_A.r_Root.p_None"},
      `Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             B);
         ihash = 814134997; ikey = "m_B.r_Root.p_None"}),
   t)
# resolve_ref "C.t" ;;
Exception:
Failure
 "resolve_reference: Parent_sig: Unexpanded `module type of` expression: module type of identifier((root Root).A, false)".
# resolve_ref "D.t" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          D);
      ihash = 200317817; ikey = "m_D.r_Root.p_None"},
   t)
# resolve_ref "E.N.t" ;;
- : ref =
`Type
  (`Alias
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             A);
         ihash = 353272258; ikey = "m_A.r_Root.p_None"},
      `Module
        (`Identifier
           {Odoc_model__Paths_types.iv =
             `Module
               ({Odoc_model__Paths_types.iv =
                  `Root
                    (Some
                      {Odoc_model__Paths_types.iv = `Page (None, None);
                       ihash = 236059787; ikey = "p_None"},
                     Root);
                 ihash = 818126955; ikey = "r_Root.p_None"},
                E);
            ihash = 359355766; ikey = "m_E.r_Root.p_None"},
         N)),
   t)
```

Classes as type references:

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module M : sig
    class cl : object method m : int end
    class type clt = object method m : int end
  end

  class cl : object method m : int end
  class type clt = object method m : int end
|}
```

```ocaml
# resolve_ref "type:M.cl" (* Type reference resolves to class *) ;;
- : ref =
`Class
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   cl)
# resolve_ref "type:M.clt" ;;
- : ref =
`ClassType
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          M);
      ihash = 716453475; ikey = "m_M.r_Root.p_None"},
   clt)
# resolve_ref "type:cl" (* Root TType reference resolves to class *) ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Class
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       cl);
   ihash = 538739416; ikey = "c_cl.r_Root.p_None"}
# resolve_ref "type:clt" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `ClassType
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       clt);
   ihash = 201394109; ikey = "ct_clt.r_Root.p_None"}
# resolve_ref "M.type-cl.m" (* Type label parent resolves to class *) ;;
- : ref =
`Method
  (`Class
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      cl),
   m)
# resolve_ref "M.type-clt.m" ;;
- : ref =
`Method
  (`ClassType
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             M);
         ihash = 716453475; ikey = "m_M.r_Root.p_None"},
      clt),
   m)
# resolve_ref "type-cl.m" (* Root TType label parent resolves to class *) ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          cl);
      ihash = 538739416; ikey = "c_cl.r_Root.p_None"},
   m)
# resolve_ref "type-clt.m" ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `ClassType
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          clt);
      ihash = 201394109; ikey = "ct_clt.r_Root.p_None"},
   m)
# resolve_ref "method:cl.m" ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          cl);
      ihash = 538739416; ikey = "c_cl.r_Root.p_None"},
   m)
```

It is not possible to reference to methods through type references:

```ocaml
# let resolve_ref = resolve_ref_of_mli {|
    class c : object method m : int end
    type t = < m : int >
  |} ;;
val resolve_ref : string -> ref = <fun>
# resolve_ref "type-c.m" ;;
- : ref =
`Method
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Class
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          c);
      ihash = 943327246; ikey = "c_c.r_Root.p_None"},
   m)
# resolve_ref "type-c.method-m" ;;
Exception:
Failure "Expected 'class-', 'class-type-', or an unqualified reference.".
# resolve_ref "type-t.m" ;;
Exception: Failure "resolve_reference: Couldn't find \"m\"".
# resolve_ref "type-t.method-m" ;;
Exception:
Failure "Expected 'class-', 'class-type-', or an unqualified reference.".
```

## Failures

Test some error paths.

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module M : sig
    type t = C
    type u = { f : t }
    type obj_sig = < m : int >
  end

  type obj_env = < m : int >
|}
```

```ocaml
# (* Lookup a field but find a constructor *)
  resolve_ref "M.field-C" ;;
Exception: Failure "resolve_reference: Couldn't find field \"C\"".
# resolve_ref "M.t.field-C" ;;
Exception: Failure "resolve_reference: Couldn't find field \"C\"".
# (* Lookup a class but find a type *)
  resolve_ref "M.class-t" ;;
Exception: Failure "resolve_reference: is of kind type but expected class".
# (* Lookup a constructor but find a field *)
  resolve_ref "M.constructor-f" ;;
Exception: Failure "resolve_reference: Couldn't find constructor \"f\"".
# resolve_ref "M.u.constructor-f" ;;
Exception: Failure "resolve_reference: Couldn't find constructor \"f\"".
```

Lookup classes but get types

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module M : sig
    type t = < m : int >
  end
  type u = < m : int >
  class c : object end
|}
```

```ocaml
# resolve_ref "m" (* in env *) ;;
Exception: Failure "resolve_reference: Couldn't find \"m\"".
# resolve_ref "method-m" ;;
Exception: Failure "resolve_reference: Couldn't find \"m\"".
# resolve_ref "u.method-m" (* Parent is type in env *) ;;
Exception:
Failure "resolve_reference: is of kind type but expected class or class type".
# resolve_ref "M.method-m" (* Parent is sig *) ;;
Exception:
Failure
 "resolve_reference: is of kind signature but expected type or class or class type".
# resolve_ref "M.t.method-m" ;;
Exception:
Failure "resolve_reference: is of kind type but expected class or class type".
# resolve_ref "c.constructor-C" (* Type in env but find class (parent of constructor is "parent") *) ;;
Exception: Failure "resolve_reference: Couldn't find \"c\"".
# resolve_ref "c.field-f" (* Field in class (parent of field is "label_parent") *) ;;
Exception: Failure "resolve_reference: Couldn't find \"c\"".
```

## Ambiguous references

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  type t = X
  val t : t

  module X : sig
    type u = Y
    val u : u

    module Y : sig end
  end

  module Everything_ambiguous_in_sig : sig
    include sig type t end
    (* include sig class r : object end end *) (* assertion failure in the compiler *)
    (* include sig class type r = object end end *)
    include sig module type t end
    (** {1:t Label} *)
    include sig type r = { t : t } end
    include sig val t : t end
    include sig external t : t -> t = "t" end
    include sig type t := t end

    include sig module T : sig end end
    include sig module T := X end
    include sig exception T end
    include sig type r = .. type r += T end
  end


|}
```

Ambiguous in env:

```ocaml
# resolve_ref "t" ;;
File "<test>":
Warning: Reference to 't' is ambiguous. Please specify its kind: type-t, val-t.
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t);
   ihash = 937245702; ikey = "v_t.r_Root.p_None"}
# resolve_ref "X" ;;
File "<test>":
Warning: Reference to 'X' is ambiguous. Please specify its kind: constructor-X, module-X.
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Module
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       X);
   ihash = 895481052; ikey = "m_X.r_Root.p_None"}
```

Ambiguous in sig:

```ocaml
# resolve_ref "X.u" ;;
File "<test>":
Warning: Reference to 'u' is ambiguous. Please specify its kind: type-u, val-u.
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          X);
      ihash = 895481052; ikey = "m_X.r_Root.p_None"},
   u)
# resolve_ref "X.Y" ;;
File "<test>":
Warning: Reference to 'Y' is ambiguous. Please specify its kind: constructor-Y, module-Y.
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             X);
         ihash = 895481052; ikey = "m_X.r_Root.p_None"},
      u),
   Y)
# resolve_ref "Everything_ambiguous_in_sig.t" (* Some kinds are missing: label, type subst (would be "type-") *) ;;
File "<test>":
Warning: Reference to 't' is ambiguous. Please specify its kind: field-t, module-type-t, type-t, val-t.
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          Everything_ambiguous_in_sig);
      ihash = 290472412;
      ikey = "m_Everything_ambiguous_in_sig.r_Root.p_None"},
   t)
# resolve_ref "Everything_ambiguous_in_sig.T" (* Missing kind: module subst (would be "module-") *) ;;
File "<test>":
Warning: Reference to 'T' is ambiguous. Please specify its kind: exception-T, extension-T, module-T.
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          Everything_ambiguous_in_sig);
      ihash = 290472412;
      ikey = "m_Everything_ambiguous_in_sig.r_Root.p_None"},
   T)
```

Unambiguous:

```ocaml
# resolve_ref "type-t" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t);
   ihash = 1016576344; ikey = "t_t.r_Root.p_None"}
# resolve_ref "val-t" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t);
   ihash = 937245702; ikey = "v_t.r_Root.p_None"}
# resolve_ref "constructor-X" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Constructor
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            t);
        ihash = 1016576344; ikey = "t_t.r_Root.p_None"},
       X);
   ihash = 255074693; ikey = "ctor_X.t_t.r_Root.p_None"}
# resolve_ref "module-X" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Module
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       X);
   ihash = 895481052; ikey = "m_X.r_Root.p_None"}
# resolve_ref "X.type-u" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          X);
      ihash = 895481052; ikey = "m_X.r_Root.p_None"},
   u)
# resolve_ref "X.val-u" ;;
- : ref =
`Value
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          X);
      ihash = 895481052; ikey = "m_X.r_Root.p_None"},
   u)
# resolve_ref "X.constructor-Y" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             X);
         ihash = 895481052; ikey = "m_X.r_Root.p_None"},
      u),
   Y)
# resolve_ref "X.module-Y" ;;
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          X);
      ihash = 895481052; ikey = "m_X.r_Root.p_None"},
   Y)
```

Unambiguous 2:

```ocaml
# resolve_ref "type:t" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Type
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t);
   ihash = 1016576344; ikey = "t_t.r_Root.p_None"}
# resolve_ref "val:t" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Value
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       t);
   ihash = 937245702; ikey = "v_t.r_Root.p_None"}
# resolve_ref "constructor:X" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Constructor
      ({Odoc_model__Paths_types.iv =
         `Type
           ({Odoc_model__Paths_types.iv =
              `Root
                (Some
                  {Odoc_model__Paths_types.iv = `Page (None, None);
                   ihash = 236059787; ikey = "p_None"},
                 Root);
             ihash = 818126955; ikey = "r_Root.p_None"},
            t);
        ihash = 1016576344; ikey = "t_t.r_Root.p_None"},
       X);
   ihash = 255074693; ikey = "ctor_X.t_t.r_Root.p_None"}
# resolve_ref "module:X" ;;
- : ref =
`Identifier
  {Odoc_model__Paths_types.iv =
    `Module
      ({Odoc_model__Paths_types.iv =
         `Root
           (Some
             {Odoc_model__Paths_types.iv = `Page (None, None);
              ihash = 236059787; ikey = "p_None"},
            Root);
        ihash = 818126955; ikey = "r_Root.p_None"},
       X);
   ihash = 895481052; ikey = "m_X.r_Root.p_None"}
# resolve_ref "type:X.u" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          X);
      ihash = 895481052; ikey = "m_X.r_Root.p_None"},
   u)
# resolve_ref "val:X.u" ;;
- : ref =
`Value
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          X);
      ihash = 895481052; ikey = "m_X.r_Root.p_None"},
   u)
# resolve_ref "constructor:X.Y" ;;
- : ref =
`Constructor
  (`Type
     (`Identifier
        {Odoc_model__Paths_types.iv =
          `Module
            ({Odoc_model__Paths_types.iv =
               `Root
                 (Some
                   {Odoc_model__Paths_types.iv = `Page (None, None);
                    ihash = 236059787; ikey = "p_None"},
                  Root);
              ihash = 818126955; ikey = "r_Root.p_None"},
             X);
         ihash = 895481052; ikey = "m_X.r_Root.p_None"},
      u),
   Y)
# resolve_ref "module:X.Y" ;;
- : ref =
`Module
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          X);
      ihash = 895481052; ikey = "m_X.r_Root.p_None"},
   Y)
```

# Functor

```ocaml
let resolve_ref = resolve_ref_of_mli {|
  module F (X : sig type t end) : sig
    type t = X.t
  end
|}
```

Element in functor body:

```ocaml
# resolve_ref "F.t" ;;
- : ref =
`Type
  (`Identifier
     {Odoc_model__Paths_types.iv =
       `Module
         ({Odoc_model__Paths_types.iv =
            `Root
              (Some
                {Odoc_model__Paths_types.iv = `Page (None, None);
                 ihash = 236059787; ikey = "p_None"},
               Root);
           ihash = 818126955; ikey = "r_Root.p_None"},
          F);
      ihash = 748202139; ikey = "m_F.r_Root.p_None"},
   t)
```
