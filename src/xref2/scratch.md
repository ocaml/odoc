```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ParameterName.fmt;;
#install_printer Common.value_name_pp;;
#install_printer Common.unit_name_pp;;

#print_length 60000;;
#print_depth 200;;
```

```ocaml env=e1
let test_data = {|
module M
  (S : sig
     module E : sig
       type 'a t
     end
  end)
  (D : sig type element end)
= struct
  include S
  module E = struct

    include E
    include struct
      open D
      type 'a t = element E.t
      let foo : unit -> element E.t = fun () -> failwith "foo"
    end
  end
end
|}
let sg = Common.model_of_string_impl test_data;;
```

```ocaml env=e1
# sg;;
- : Odoc_model.Paths_types.Identifier.reference_module *
    Odoc_model.Comment.docs * Odoc_model.Lang.Signature.t
=
(`Root (Common.root, <abstr>), [],
 [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (Common.root, <abstr>), M);
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Odoc_model.Lang.FunctorParameter.Named
          {Odoc_model.Lang.FunctorParameter.id =
            `Parameter (`Module (`Root (Common.root, <abstr>), M), S);
           expr =
            Odoc_model.Lang.ModuleType.Signature
             [Odoc_model.Lang.Signature.Module
               (Odoc_model.Lang.Signature.Ordinary,
               {Odoc_model.Lang.Module.id =
                 `Module
                   (`Parameter (`Module (`Root (Common.root, <abstr>), M), S),
                    E);
                doc = [];
                type_ =
                 Odoc_model.Lang.Module.ModuleType
                  (Odoc_model.Lang.ModuleType.Signature
                    [Odoc_model.Lang.Signature.Type
                      (Odoc_model.Lang.Signature.Ordinary,
                      {Odoc_model.Lang.TypeDecl.id =
                        `Type
                          (`Module
                             (`Parameter
                                (`Module (`Root (Common.root, <abstr>), M),
                                 S),
                              E),
                           <abstr>);
                       doc = [];
                       equation =
                        {Odoc_model.Lang.TypeDecl.Equation.params =
                          [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                         private_ = false; manifest = None; constraints = []};
                       representation = None})]);
                canonical = None; hidden = false; display_type = None;
                expansion = Some Odoc_model.Lang.Module.AlreadyASig})];
           display_expr = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig},
        Odoc_model.Lang.ModuleType.Functor
         (Odoc_model.Lang.FunctorParameter.Named
           {Odoc_model.Lang.FunctorParameter.id =
             `Parameter
               (`Result (`Module (`Root (Common.root, <abstr>), M)), D);
            expr =
             Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`Parameter
                       (`Result (`Module (`Root (Common.root, <abstr>), M)),
                        D),
                     <abstr>);
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})];
            display_expr = None;
            expansion = Some Odoc_model.Lang.Module.AlreadyASig},
         Odoc_model.Lang.ModuleType.Signature
          [Odoc_model.Lang.Signature.Include
            {Odoc_model.Lang.Include.parent =
              `Result (`Result (`Module (`Root (Common.root, <abstr>), M)));
             doc = [];
             decl =
              Odoc_model.Lang.Module.Alias
               (`Resolved
                  (`Identifier
                     (`Parameter
                        (`Module (`Root (Common.root, <abstr>), M), S))));
             inline = false;
             expansion =
              {Odoc_model.Lang.Include.resolved = false;
               content =
                [Odoc_model.Lang.Signature.Module
                  (Odoc_model.Lang.Signature.Ordinary,
                  {Odoc_model.Lang.Module.id =
                    `Module
                      (`Result
                         (`Result (`Module (`Root (Common.root, <abstr>), M))),
                       $E);
                   doc = [];
                   type_ =
                    Odoc_model.Lang.Module.ModuleType
                     (Odoc_model.Lang.ModuleType.Signature
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`Module
                                (`Result
                                   (`Result
                                      (`Module
                                         (`Root (Common.root, <abstr>), M))),
                                 $E),
                              <abstr>);
                          doc = [];
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params =
                             [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                            private_ = false;
                            manifest =
                             Some
                              (Odoc_model.Lang.TypeExpr.Constr
                                (`Dot
                                   (`Dot
                                      (`Resolved
                                         (`Identifier
                                            (`Parameter
                                               (`Module
                                                  (`Root
                                                     (Common.root, <abstr>),
                                                   M),
                                                S))),
                                       "E"),
                                    "t"),
                                [Odoc_model.Lang.TypeExpr.Var "a"]));
                            constraints = []};
                          representation = None})]);
                   canonical = None; hidden = false; display_type = None;
                   expansion = Some Odoc_model.Lang.Module.AlreadyASig})]}};
           Odoc_model.Lang.Signature.Module
            (Odoc_model.Lang.Signature.Ordinary,
            {Odoc_model.Lang.Module.id =
              `Module
                (`Result
                   (`Result (`Module (`Root (Common.root, <abstr>), M))),
                 E);
             doc = [];
             type_ =
              Odoc_model.Lang.Module.ModuleType
               (Odoc_model.Lang.ModuleType.Signature
                 [Odoc_model.Lang.Signature.Include
                   {Odoc_model.Lang.Include.parent =
                     `Module
                       (`Result
                          (`Result
                             (`Module (`Root (Common.root, <abstr>), M))),
                        E);
                    doc = [];
                    decl =
                     Odoc_model.Lang.Module.Alias
                      (`Resolved
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Result
                                     (`Result
                                        (`Module
                                           (`Root (Common.root, <abstr>), M))),
                                   $E)))));
                    inline = false;
                    expansion =
                     {Odoc_model.Lang.Include.resolved = false;
                      content =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`Module
                                (`Result
                                   (`Result
                                      (`Module
                                         (`Root (Common.root, <abstr>), M))),
                                 E),
                              <abstr>);
                          doc = [];
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params =
                             [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                            private_ = false;
                            manifest =
                             Some
                              (Odoc_model.Lang.TypeExpr.Constr
                                (`Dot
                                   (`Dot
                                      (`Resolved
                                         (`Identifier
                                            (`Parameter
                                               (`Module
                                                  (`Root
                                                     (Common.root, <abstr>),
                                                   M),
                                                S))),
                                       "E"),
                                    "t"),
                                [Odoc_model.Lang.TypeExpr.Var "a"]));
                            constraints = []};
                          representation = None})]}};
                  Odoc_model.Lang.Signature.Include
                   {Odoc_model.Lang.Include.parent =
                     `Module
                       (`Result
                          (`Result
                             (`Module (`Root (Common.root, <abstr>), M))),
                        E);
                    doc = [];
                    decl =
                     Odoc_model.Lang.Module.ModuleType
                      (Odoc_model.Lang.ModuleType.Signature
                        [Odoc_model.Lang.Signature.Open
                          {Odoc_model.Lang.Open.expansion = []};
                         Odoc_model.Lang.Signature.Type
                          (Odoc_model.Lang.Signature.Ordinary,
                          {Odoc_model.Lang.TypeDecl.id =
                            `Type
                              (`Module
                                 (`Result
                                    (`Result
                                       (`Module
                                          (`Root (Common.root, <abstr>), M))),
                                  E),
                               <abstr>);
                           doc = [];
                           equation =
                            {Odoc_model.Lang.TypeDecl.Equation.params =
                              [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                             private_ = false;
                             manifest =
                              Some
                               (Odoc_model.Lang.TypeExpr.Constr
                                 (`Dot
                                    (`Resolved
                                       (`Hidden
                                          (`Identifier
                                             (`Module
                                                (`Result
                                                   (`Result
                                                      (`Module
                                                         (`Root
                                                            (Common.root,
                                                             <abstr>),
                                                          M))),
                                                 $E)))),
                                     "t"),
                                 [Odoc_model.Lang.TypeExpr.Constr
                                   (`Dot
                                      (`Resolved
                                         (`Identifier
                                            (`Parameter
                                               (`Result
                                                  (`Module
                                                     (`Root
                                                        (Common.root,
                                                         <abstr>),
                                                      M)),
                                                D))),
                                       "element"),
                                   [])]));
                             constraints = []};
                           representation = None});
                         Odoc_model.Lang.Signature.Value
                          {Odoc_model.Lang.Value.id =
                            `Value
                              (`Module
                                 (`Result
                                    (`Result
                                       (`Module
                                          (`Root (Common.root, <abstr>), M))),
                                  E),
                               <abstr>);
                           doc = [];
                           type_ =
                            Odoc_model.Lang.TypeExpr.Arrow (None,
                             Odoc_model.Lang.TypeExpr.Constr
                              (`Resolved (`Identifier (`CoreType <abstr>)),
                              []),
                             Odoc_model.Lang.TypeExpr.Constr
                              (`Dot
                                 (`Resolved
                                    (`Hidden
                                       (`Identifier
                                          (`Module
                                             (`Result
                                                (`Result
                                                   (`Module
                                                      (`Root
                                                         (Common.root,
                                                          <abstr>),
                                                       M))),
                                              $E)))),
                                  "t"),
                              [Odoc_model.Lang.TypeExpr.Constr
                                (`Dot
                                   (`Resolved
                                      (`Identifier
                                         (`Parameter
                                            (`Result
                                               (`Module
                                                  (`Root
                                                     (Common.root, <abstr>),
                                                   M)),
                                             D))),
                                    "element"),
                                [])]))}]);
                    inline = false;
                    expansion =
                     {Odoc_model.Lang.Include.resolved = false;
                      content =
                       [Odoc_model.Lang.Signature.Type
                         (Odoc_model.Lang.Signature.Ordinary,
                         {Odoc_model.Lang.TypeDecl.id =
                           `Type
                             (`Module
                                (`Result
                                   (`Result
                                      (`Module
                                         (`Root (Common.root, <abstr>), M))),
                                 E),
                              <abstr>);
                          doc = [];
                          equation =
                           {Odoc_model.Lang.TypeDecl.Equation.params =
                             [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                            private_ = false;
                            manifest =
                             Some
                              (Odoc_model.Lang.TypeExpr.Constr
                                (`Dot
                                   (`Resolved
                                      (`Hidden
                                         (`Identifier
                                            (`Module
                                               (`Result
                                                  (`Result
                                                     (`Module
                                                        (`Root
                                                           (Common.root,
                                                            <abstr>),
                                                         M))),
                                                $E)))),
                                    "t"),
                                [Odoc_model.Lang.TypeExpr.Constr
                                  (`Dot
                                     (`Resolved
                                        (`Identifier
                                           (`Parameter
                                              (`Result
                                                 (`Module
                                                    (`Root
                                                       (Common.root, <abstr>),
                                                     M)),
                                               D))),
                                      "element"),
                                  [])]));
                            constraints = []};
                          representation = None});
                        Odoc_model.Lang.Signature.Value
                         {Odoc_model.Lang.Value.id =
                           `Value
                             (`Module
                                (`Result
                                   (`Result
                                      (`Module
                                         (`Root (Common.root, <abstr>), M))),
                                 E),
                              <abstr>);
                          doc = [];
                          type_ =
                           Odoc_model.Lang.TypeExpr.Arrow (None,
                            Odoc_model.Lang.TypeExpr.Constr
                             (`Resolved (`Identifier (`CoreType <abstr>)),
                             []),
                            Odoc_model.Lang.TypeExpr.Constr
                             (`Dot
                                (`Resolved
                                   (`Hidden
                                      (`Identifier
                                         (`Module
                                            (`Result
                                               (`Result
                                                  (`Module
                                                     (`Root
                                                        (Common.root,
                                                         <abstr>),
                                                      M))),
                                             $E)))),
                                 "t"),
                             [Odoc_model.Lang.TypeExpr.Constr
                               (`Dot
                                  (`Resolved
                                     (`Identifier
                                        (`Parameter
                                           (`Result
                                              (`Module
                                                 (`Root
                                                    (Common.root, <abstr>),
                                                  M)),
                                            D))),
                                   "element"),
                               [])]))}]}}]);
             canonical = None; hidden = false; display_type = None;
             expansion = Some Odoc_model.Lang.Module.AlreadyASig})])));
    canonical = None; hidden = false; display_type = None; expansion = None})])
```


Subst nodes
===========

module type Foo = sig

module X : functor (Y : sig module type S end) ->
  sig
    module Z : Y.S
    module type S = Y.S
  end

module A : sig
  module type S = sig
    type t
  end
end

module M : X(A).S with type t = int

end

