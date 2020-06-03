open Odoc_xref2
open Odoc_xref_test

(** Resolution is the process by which we take a value of type {!type:Lang.t} and
    look up details of the internal cross references and make sure we know exactly
    which component is being referred to. Much of the work has been done by the
    compiler but we need to do a little more. For example, given
    
    {[
    module M : sig
            type t
        end
    type u = M.t 
    ]}

    in the definition of [u], the compiler tells us precisely which [M] is on the
    right hand side but doesn't we need to which which [t] it is referring to.

    *)

type test = {
    name : string;
    description : string;
    test_data : string;
    test_fn : test -> unit;
}

let test_resolve test =
    Odoc_xref2.Tools.reset_caches ();
    let _, _, sg = Common.model_of_string test.test_data in
    let open Format in
    fprintf std_formatter "%s\n%s\n%!" test.name test.description;
    fprintf std_formatter "CODE\n====\n%!%s\n%!" test.test_data;
    fprintf std_formatter "BEFORE\n======\n%!%a\n%!" Common.LangUtils.Fmt.signature sg;
    let sg' = Compile.signature Env.empty Common.root_with_name sg in
    fprintf std_formatter "AFTER \n===== \n%!%a\n%!" Common.LangUtils.Fmt.signature sg'

(**
The simplest resolution is where we simply look up a type and check it's there. Given the signature
{[
    type t
    type u = t
]}
the resolution process will check the right-hand side of the [type u] declaration and look up the
definition of [t].

The first thing that happens is that we construct an {{!type:Env.t}environment}, which is a mapping
of {{!type:Model.Path.Identifier.t}Identifier} to {{!module:Component}Component}, where the 
component is a value of type specific to the type of component (e.g. {!type:Component.Module.t} or
{!type:Component.Type.t}). For the case above, the environment looks like this:

{[
{ ident_max = 0
; modules = []
; module_types = []
; types =
    [(`Type (`Root root, "u"),
       { id = ("u", 1)
       ; manifest = Some (Constr (`Global (`Resolved (`Identifier (`Type (`Root root, "t")))), []))});
     (`Type (`Root root, "t"),
       { id = ("t", 0)
       ; manifest = None})
    ]
}
]}

here we can see there are two types in the environment and nothing else. [u] has identifier 
[`Type (`Root root, "u")], and has an {{!type:Ident.t}internal identifier} of [("u",1)]. [t] has
no manifest, as it's abstract, but [u] has a manifest that points to the [`Global] path that
has already been [`Resolved] to [`Identifier] [`Type (`Root root, "u")]. So there won't be much
for us to do.

Once we've set up the environment we enter the function {!value:Resolve.signature} which takes
a {{!type:Model.Lang.Signature.t}Lang.Signature.t} and runs the resolve mapping functions over
the tree. For this example the interesting point comes when we get to looking at the manifest
for type [u]. We see that we have a [Constr] that has a path in it, so we look up the component
from the path via the function {!value:Tools.lookup_type_from_model_path}. This returns us
a [Result.result] containing the resolved path and the {!type:Component.Type.t} that represents
the type [t]. We don't particularly care about this, but the returned path we use in place of
the path we had before.
*)
let basic1 =
  { name = "Basic resolution 1"
  ; description = "Simplest possible resolution"
  ; test_data = {|
type t
type u = t
    |}
  ; test_fn = test_resolve }


(**
Let's look at a marginally more complicated example. In this case, our type [t] is now
inside a module:

{[
module M : sig
    type t
end
type u = M.t  
]}

The OCaml compiler find the module [M] exactly, but everything after that is left to us
to identify precisely. So the manifest of [u] is now:

{[
Some (Constr (`Dot (`Resolved (`Identifier (`Module (`Root root, "M"))), "t"))
]}

Note that this is the manifest from the {!type:Lang.TypeDecl.t} rather than the {!type:Component.Type.t},
and so the argument to [Constr] is a {!type:Model.Paths.Path.t} rather than a {!type:Cpath.t}, but this
distinction is unimportant here and amounts to the reason why there is no [`Global] here where there is
above.

What _is_ important is that the path is not completely resolved. The [M] bit is resolved, but the [t]
bit is not. So we have to do a bit more work when we look up the type in [lookup_type_from_model_path].

Let's look in more detail at that process. The first thing that happens is that the path is matched
the [`Dot] is found. This implies that the thing before the dot is a module, so we call 
[lookup_module_from_model_path].  This is a resolved identifier so we can simply look this up from
the environment. This gets us back the path and {!type:Component.Module.t} representing the module M,
which are:

{[
  `Identifier (`Module (`Root root, "M")))  
]}

and

{[
    { id = ("M", 0)
    ; type_ = ModuleType (Signature [ Type {id = ("t", 1); manifest = None } ])
]}

we then convert this into a signature, which in this case is simply extrating the [Signature] from
within the [type_] field, and then we can call [Component.Find.type_in_sig] to find the representation
of the type [t]. Now we know it's there we can construct a resolved path that points precisely at 
this type declaration. We take the path returned when looking up [M], and use that as the parent
in the declaration of the new path for [t]. As a comparison, we were called with  

{[
    `Dot (`Resolved (`Identifier (`Module (`Root root, "M"))), "t"))
]}

but we return

{[
    `Resolved (`Type (`Identifier (`Module (`Root root, "M")), "t"))
]}

and this is replaced as the path in the representation of the signature.
*)
let basic2 =
  { name = "Basic resolution 2"
  ; description = "Environment lookup"
  ; test_data = {|
module M : sig
    type t
end
type u = M.t
    |}
  ; test_fn = test_resolve }


(** Now we have a bit of indirection. Take the example:

{[
        module type M = sig
            type t
        end
        module N : M
        type u = N.t

]}

This follows a similar pattern to the above, but now when we look up [N] from the environment we 
get back

{[
    { id = ("N", 2)
    ; type_ = ModuleType (Path (`Global (`Resolved (`Identifier (`ModuleType root, "M"))))))}
]}

so we can't directly extract the signature from this module to look up the type [t] as before.
Instead we must look up the module type "M" from the environment, then convert that into a
signature, then look up the type in that. when we look up "M" we get back

{[
{ id = ("M", 9)
   ; expr = Some (Signature
              [ Module
                  { id = ("N", 10)
                  ; type_ = ModuleType (Signature [Type
                         { id = ("t", 11);
                           manifest = None}])}])})
]}

We can turn this into a signature as before, and then we proceed in a similar fashion to the previous example.
*)
let basic3 =
  { name = "Basic resolution 3"
  ; description = "Module type"
  ; test_data = {|
module type M = sig
    type t
end
module N : M
type u = N.t
    |}
  ; test_fn = test_resolve }

(** 
This example is very similar but there is one more level of nesting of the modules:

{[
        module type M = sig
            module N : sig
                type t
            end
        end
        module A : M
        type u = A.N.t
]}
*)
let basic4 =
  { name = "Basic resolution 4"
  ; description = "Module type"
  ; test_data = {|
module type M = sig
    module N : sig
        type t
    end
end
module A : M
type u = A.N.t
    |}
  ; test_fn = test_resolve }

(**
This example is rather more interesting:

{[
module type M = sig
    module type N = sig
        type t
    end
    module B : N
end
module A : M
type u = A.B.t
]}

Once again we look up [A], then look up [M]. The representation of [M] is as follows:

{[
{ id = ("M", 0);
; expr = Some (Signature
    [ ModuleType
        { id = ("N", 1)
        ; expr = Some (Signature [Type {Type.id = ("t", 3); manifest = None}])};
      Module
        { id = ("B", 2)
        ; type_ = ModuleType (Path (`Local ("N", 1)))}])})];
]}

The interesting thing here is that the type of "B" has a {b Local} path in it.
Eventually we need to have fully qualified paths for all items so this needs some work.
The way this is handled is that when we want to look up an element within a module,
we don't just convert it blindly to a signature. Since we have the fully qualified
path to the module, we prefix all the identifiers bound in that signature
with that path to turn them into global paths.

In the above example, wherever we see [`Local ("N",1)], we substitute instead
[`Global (`Resolved (`ModuleType (a, "N")))] where [a] is the path to [A] that we
found when looking up the module [A] previously.

Once this is done when we look up the module [B] we discover a module whose type
is 

{[
    `Global (`Resolved (`ModuleType (`Identifier (`Module (`Root root, "A")), "N")))
]}

So once again we call lookup_module_type_from_model_path, which recurses down 
until it finds the identifier. We look up [A], find the module type "N" within
it, then convert _that_ into a signature, prefixing it with the path [A.B], 
and then we can look up the type [t]. 
 *)
let basic5 =
  { name = "Basic resolution 4"
  ; description = "Module type"
  ; test_data = {|
module type M = sig
    module type N = sig
        type t
    end
    module B : N
end
module A : M
type u = A.B.t
    |}
  ; test_fn = test_resolve }


let basic6 =
  { name = "Basic resolution 4"
  ; description = "Module type"
  ; test_data = {|
module type M = sig
    module type N = sig
        type t
    end
    module X : sig
        module B : N
    end
end
module A : M
type u = A.X.B.t
    |}
  ; test_fn = test_resolve }


let module_subst =
  { name = "Module substitution"
  ; description = "Ensure a substitution is taken into account during resolution"
  ; test_data = {|
module type A = sig
module M : sig module type S end
module N : M.S
end

module B : sig module type S = sig type t end end

module C : A with module M = B

type t = C.N.t
    |}
  ; test_fn = test_resolve }

let module_subst2 =
  { name = "Module substitution2"
  ; description = "Ensure a destructive substitution is taken into account during resolution"
  ; test_data = {|
module type A = sig
module M : sig module type S end
module N : M.S
end

module B : sig module type S = sig type t end end

module C : A with module M := B

type t = C.N.t
    |}
  ; test_fn = test_resolve }

let module_alias =
  { name = "Module alias"
  ; description = "Resolve a module alias"
  ; test_data = {|
module A : sig
    type t
end
module B = A
type t = B.t
|}
  ; test_fn = test_resolve }

let module_alias2 =
  { name = "Module alias"
  ; description = "Resolve a module alias"
  ; test_data = {|
module A : sig
    type t
end
module B = A
module C = B
type t = C.t
|}
  ; test_fn = test_resolve }

let functor1 =
  { name = "Functor"
  ; description = "Resolve a functor"
  ; test_data = {|
module type S = sig
  type t
end

module F ( X : S ) ( Y : S ) : sig
  type x_t = X.t
  type y_t = Y.t
  type f_t = x_t
end
|}
  ; test_fn = test_resolve }

let functor_all =
  { name = "Functor"
  ; description = "Resolve a functor"
  ; test_data = {|
module type S =
sig
  type t
end

module type S1 = functor (_ : S) -> S

module F1 : functor (Arg : S) -> S

module F2 : functor (Arg : S) -> (S with type t = Arg.t)

module F3 : functor (Arg : S) ->
sig
  type t = Arg.t
end

module F4 (Arg : S) : S

module F5 (Arg1 : S) (Arg2 : S) (Arg3 : S) : sig
        type t = Arg1.t
        type u = Arg2.t
        type v = Arg3.t
        type z = t
end

module F6 : S1

module type F7 = functor (Arg : S) -> sig
  type t = Arg.t
  type u = t
end|}
  ; test_fn = test_resolve }
(*
let functor_app =
  { name = "Functor"
  ; description = "Resolve a functor"
  ; test_data = {|
module type ARG = sig
  module type S
end

module F : functor (X : ARG) -> sig
  module N : X.S 
end

module M : sig
  module type S = sig
    type t
  end
end

type t = F(M).N.t
|}
  ; test_fn = test_resolve }
*)
(*
  Identifier of F:
  
    ident_f = `Identifier (`Module (`Root (Common.root, "Root"), "F")))
  
  Identifier of M:
  
    ident_m = `Identifier (`Module (`Root (Common.root, "Root"), "M")))


  type t = `Type (`Subst (`ModuleType (ident_m, "S"), `Module (`Apply (ident_f, ident_m), "N"))), "t")
*)

let functor_app_ugh =
{ name = "Functor app nightmare"
; description = "Horrible"
; test_data = {|
module type Type = sig module type T end
module App : functor (T : Type) (F : Type -> Type) (M : F(T).T) -> F(T).T
module Bar : sig module type T = sig type bar end end
module Foo :
  functor (T : Type) -> sig module type T = sig module Foo : T.T end end
module FooBarInt : sig module Foo : sig type bar = int end end
type t = App(Bar)(Foo)(FooBarInt).Foo.bar
(* Note: I think correct result is:
type t = resolved[(global(Bar).T subst-> global(App)(resolved[global(Bar)])(resolved[global(Foo)])(resolved[global(FooBarInt)]).Foo).bar]
*)
|}
; test_fn = test_resolve }



let tests =
  [ basic1
  ; basic2
  ; basic3
  ; basic4
  ; basic5
  ; basic6
  ; module_subst
  ; module_subst2
  ; module_alias
  ; module_alias2
  ; functor1
  ; functor_all
  (* ; functor_app *)
  (*; functor_app_ugh *) ]

let _ =
    List.iter (fun test -> test.test_fn test) tests
