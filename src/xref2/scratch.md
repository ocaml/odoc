```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ParameterName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_xref2.Component.Fmt.model_identifier;;
#install_printer Common.value_name_pp;;
#install_printer Common.type_name_pp;;
#install_printer Common.unit_name_pp;;
#print_length 60000;;
#print_depth 200;;

let mt_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.ModuleType.t :> Odoc_model.Paths.Identifier.t);;
#install_printer mt_printer;;
let m_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.Module.t :> Odoc_model.Paths.Identifier.t);;
#install_printer m_printer;;
let t_printer fmt t = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (t : Odoc_model.Paths.Identifier.Type.t :> Odoc_model.Paths.Identifier.t);;
#install_printer t_printer;;
let doc_printer fmt (doc : Odoc_model.Comment.docs) = Format.fprintf fmt "<<docs>>";;
#install_printer doc_printer;;
#install_printer Ident.print_with_scope;;
#install_printer Ident.print;;
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
let sg = Common.cmt_of_string test_data;;
```

```ocaml env=e1
let test_data = {|
module M : sig
  type t
  val id : t -> t  
end

module Mextended : sig
  include module type of struct include M end
  type t
  val ignore : t -> unit
end
|};;
```

```ocaml env=e1
# let sg = Common.model_of_string test_data;;
val sg :
  Odoc_model.Paths_types.Identifier.reference_module *
  Odoc_model.Comment.docs * Odoc_model.Lang.Signature.t =
  ((root Root), <<docs>>,
   [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
     {Odoc_model.Lang.Module.id = (root Root).M; doc = <<docs>>;
      type_ =
       Odoc_model.Lang.Module.ModuleType
        (Odoc_model.Lang.ModuleType.Signature
          [Odoc_model.Lang.Signature.Type
            (Odoc_model.Lang.Signature.Ordinary,
            {Odoc_model.Lang.TypeDecl.id = (root Root).M.t; doc = <<docs>>;
             equation =
              {Odoc_model.Lang.TypeDecl.Equation.params = [];
               private_ = false; manifest = None; constraints = []};
             representation = None});
           Odoc_model.Lang.Signature.Value
            {Odoc_model.Lang.Value.id =
              `Value (`Module (`Root (Common.root, <abstr>), M), id);
             doc = <<docs>>;
             type_ =
              Odoc_model.Lang.TypeExpr.Arrow (None,
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved
                   (`Identifier
                      (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
                []),
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved
                   (`Identifier
                      (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
                []))}]);
      canonical = None; hidden = false; display_type = None;
      expansion = Some Odoc_model.Lang.Module.AlreadyASig});
    Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
     {Odoc_model.Lang.Module.id = (root Root).Mextended; doc = <<docs>>;
      type_ =
       Odoc_model.Lang.Module.ModuleType
        (Odoc_model.Lang.ModuleType.Signature
          [Odoc_model.Lang.Signature.Include
            {Odoc_model.Lang.Include.parent =
              `Module (`Root (Common.root, <abstr>), Mextended);
             doc = <<docs>>;
             decl =
              Odoc_model.Lang.Module.ModuleType
               (Odoc_model.Lang.ModuleType.TypeOf
                 (Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     [Odoc_model.Lang.Signature.Include
                       {Odoc_model.Lang.Include.parent =
                         `Module (`Root (Common.root, <abstr>), Mextended);
                        doc = <<docs>>;
                        decl =
                         Odoc_model.Lang.Module.Alias
                          (`Resolved (`Identifier (root Root).M));
                        inline = false;
                        expansion =
                         {Odoc_model.Lang.Include.resolved = false;
                          shadowed = [];
                          content =
                           [Odoc_model.Lang.Signature.Type
                             (Odoc_model.Lang.Signature.Ordinary,
                             {Odoc_model.Lang.TypeDecl.id =
                               (root Root).Mextended.t;
                              doc = <<docs>>;
                              equation =
                               {Odoc_model.Lang.TypeDecl.Equation.params = [];
                                private_ = false;
                                manifest =
                                 Some
                                  (Odoc_model.Lang.TypeExpr.Constr
                                    (`Dot
                                       (`Resolved (`Identifier (root Root).M),
                                        "t"),
                                    []));
                                constraints = []};
                              representation = None});
                            Odoc_model.Lang.Signature.Value
                             {Odoc_model.Lang.Value.id =
                               `Value
                                 (`Module
                                    (`Root (Common.root, <abstr>), Mextended),
                                  id);
                              doc = <<docs>>;
                              type_ =
                               Odoc_model.Lang.TypeExpr.Arrow (None,
                                Odoc_model.Lang.TypeExpr.Constr
                                 (`Resolved
                                    (`Identifier
                                       (`Type
                                          (`Module
                                             (`Root (Common.root, <abstr>),
                                              Mextended),
                                           t))),
                                 []),
                                Odoc_model.Lang.TypeExpr.Constr
                                 (`Resolved
                                    (`Identifier
                                       (`Type
                                          (`Module
                                             (`Root (Common.root, <abstr>),
                                              Mextended),
                                           t))),
                                 []))}]}}])));
             inline = false;
             expansion =
              {Odoc_model.Lang.Include.resolved = false;
               shadowed = [("t", (root Root).Mextended.$t$1)];
               content =
                [Odoc_model.Lang.Signature.Type
                  (Odoc_model.Lang.Signature.Ordinary,
                  {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.$t$1;
                   doc = <<docs>>;
                   equation =
                    {Odoc_model.Lang.TypeDecl.Equation.params = [];
                     private_ = false;
                     manifest =
                      Some
                       (Odoc_model.Lang.TypeExpr.Constr
                         (`Dot (`Resolved (`Identifier (root Root).M), "t"),
                         []));
                     constraints = []};
                   representation = None});
                 Odoc_model.Lang.Signature.Value
                  {Odoc_model.Lang.Value.id =
                    `Value
                      (`Module (`Root (Common.root, <abstr>), Mextended), id);
                   doc = <<docs>>;
                   type_ =
                    Odoc_model.Lang.TypeExpr.Arrow (None,
                     Odoc_model.Lang.TypeExpr.Constr
                      (`Resolved
                         (`Identifier
                            (`Type
                               (`Module
                                  (`Root (Common.root, <abstr>), Mextended),
                                $t$1))),
                      []),
                     Odoc_model.Lang.TypeExpr.Constr
                      (`Resolved
                         (`Identifier
                            (`Type
                               (`Module
                                  (`Root (Common.root, <abstr>), Mextended),
                                $t$1))),
                      []))}]}};
           Odoc_model.Lang.Signature.Type
            (Odoc_model.Lang.Signature.Ordinary,
            {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.t;
             doc = <<docs>>;
             equation =
              {Odoc_model.Lang.TypeDecl.Equation.params = [];
               private_ = false; manifest = None; constraints = []};
             representation = None});
           Odoc_model.Lang.Signature.Value
            {Odoc_model.Lang.Value.id =
              `Value
                (`Module (`Root (Common.root, <abstr>), Mextended), ignore);
             doc = <<docs>>;
             type_ =
              Odoc_model.Lang.TypeExpr.Arrow (None,
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved
                   (`Identifier
                      (`Type
                         (`Module (`Root (Common.root, <abstr>), Mextended),
                          t))),
                []),
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType unit)), []))}]);
      canonical = None; hidden = false; display_type = None;
      expansion = Some Odoc_model.Lang.Module.AlreadyASig})])
# sg;;
- : Odoc_model.Paths_types.Identifier.reference_module *
    Odoc_model.Comment.docs * Odoc_model.Lang.Signature.t
=
((root Root), <<docs>>,
 [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = (root Root).M; doc = <<docs>>;
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id = (root Root).M.t; doc = <<docs>>;
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Value
          {Odoc_model.Lang.Value.id =
            `Value (`Module (`Root (Common.root, <abstr>), M), id);
           doc = <<docs>>;
           type_ =
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
              []),
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
              []))}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = (root Root).Mextended; doc = <<docs>>;
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `Module (`Root (Common.root, <abstr>), Mextended);
           doc = <<docs>>;
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.TypeOf
               (Odoc_model.Lang.Module.ModuleType
                 (Odoc_model.Lang.ModuleType.Signature
                   [Odoc_model.Lang.Signature.Include
                     {Odoc_model.Lang.Include.parent =
                       `Module (`Root (Common.root, <abstr>), Mextended);
                      doc = <<docs>>;
                      decl =
                       Odoc_model.Lang.Module.Alias
                        (`Resolved (`Identifier (root Root).M));
                      inline = false;
                      expansion =
                       {Odoc_model.Lang.Include.resolved = false;
                        shadowed = [];
                        content =
                         [Odoc_model.Lang.Signature.Type
                           (Odoc_model.Lang.Signature.Ordinary,
                           {Odoc_model.Lang.TypeDecl.id =
                             (root Root).Mextended.t;
                            doc = <<docs>>;
                            equation =
                             {Odoc_model.Lang.TypeDecl.Equation.params = [];
                              private_ = false;
                              manifest =
                               Some
                                (Odoc_model.Lang.TypeExpr.Constr
                                  (`Dot
                                     (`Resolved (`Identifier (root Root).M),
                                      "t"),
                                  []));
                              constraints = []};
                            representation = None});
                          Odoc_model.Lang.Signature.Value
                           {Odoc_model.Lang.Value.id =
                             `Value
                               (`Module
                                  (`Root (Common.root, <abstr>), Mextended),
                                id);
                            doc = <<docs>>;
                            type_ =
                             Odoc_model.Lang.TypeExpr.Arrow (None,
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`Module
                                           (`Root (Common.root, <abstr>),
                                            Mextended),
                                         t))),
                               []),
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`Module
                                           (`Root (Common.root, <abstr>),
                                            Mextended),
                                         t))),
                               []))}]}}])));
           inline = false;
           expansion =
            {Odoc_model.Lang.Include.resolved = false;
             shadowed = [("t", (root Root).Mextended.$t$1)];
             content =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.$t$1;
                 doc = <<docs>>;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false;
                   manifest =
                    Some
                     (Odoc_model.Lang.TypeExpr.Constr
                       (`Dot (`Resolved (`Identifier (root Root).M), "t"),
                       []));
                   constraints = []};
                 representation = None});
               Odoc_model.Lang.Signature.Value
                {Odoc_model.Lang.Value.id =
                  `Value
                    (`Module (`Root (Common.root, <abstr>), Mextended), id);
                 doc = <<docs>>;
                 type_ =
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Root (Common.root, <abstr>), Mextended),
                              $t$1))),
                    []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Root (Common.root, <abstr>), Mextended),
                              $t$1))),
                    []))}]}};
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.t;
           doc = <<docs>>;
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Value
          {Odoc_model.Lang.Value.id =
            `Value
              (`Module (`Root (Common.root, <abstr>), Mextended), ignore);
           doc = <<docs>>;
           type_ =
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type
                       (`Module (`Root (Common.root, <abstr>), Mextended), t))),
              []),
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType unit)), []))}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig})])
```

```ocaml env=e1
# sg;;
- : Odoc_model.Paths_types.Identifier.reference_module *
    Odoc_model.Comment.docs * Odoc_model.Lang.Signature.t
=
((root Root), <<docs>>,
 [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = (root Root).M; doc = <<docs>>;
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id = (root Root).M.t; doc = <<docs>>;
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Value
          {Odoc_model.Lang.Value.id =
            `Value (`Module (`Root (Common.root, <abstr>), M), id);
           doc = <<docs>>;
           type_ =
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
              []),
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Module (`Root (Common.root, <abstr>), M), t))),
              []))}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = (root Root).Mextended; doc = <<docs>>;
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `Module (`Root (Common.root, <abstr>), Mextended);
           doc = <<docs>>;
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.TypeOf
               (Odoc_model.Lang.Module.ModuleType
                 (Odoc_model.Lang.ModuleType.Signature
                   [Odoc_model.Lang.Signature.Include
                     {Odoc_model.Lang.Include.parent =
                       `Module (`Root (Common.root, <abstr>), Mextended);
                      doc = <<docs>>;
                      decl =
                       Odoc_model.Lang.Module.Alias
                        (`Resolved (`Identifier (root Root).M));
                      inline = false;
                      expansion =
                       {Odoc_model.Lang.Include.resolved = false;
                        shadowed = [];
                        content =
                         [Odoc_model.Lang.Signature.Type
                           (Odoc_model.Lang.Signature.Ordinary,
                           {Odoc_model.Lang.TypeDecl.id =
                             (root Root).Mextended.t;
                            doc = <<docs>>;
                            equation =
                             {Odoc_model.Lang.TypeDecl.Equation.params = [];
                              private_ = false;
                              manifest =
                               Some
                                (Odoc_model.Lang.TypeExpr.Constr
                                  (`Dot
                                     (`Resolved (`Identifier (root Root).M),
                                      "t"),
                                  []));
                              constraints = []};
                            representation = None});
                          Odoc_model.Lang.Signature.Value
                           {Odoc_model.Lang.Value.id =
                             `Value
                               (`Module
                                  (`Root (Common.root, <abstr>), Mextended),
                                id);
                            doc = <<docs>>;
                            type_ =
                             Odoc_model.Lang.TypeExpr.Arrow (None,
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`Module
                                           (`Root (Common.root, <abstr>),
                                            Mextended),
                                         t))),
                               []),
                              Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`Module
                                           (`Root (Common.root, <abstr>),
                                            Mextended),
                                         t))),
                               []))}]}}])));
           inline = false;
           expansion =
            {Odoc_model.Lang.Include.resolved = false;
             shadowed = [("t", (root Root).Mextended.$t$1)];
             content =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.$t$1;
                 doc = <<docs>>;
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false;
                   manifest =
                    Some
                     (Odoc_model.Lang.TypeExpr.Constr
                       (`Dot (`Resolved (`Identifier (root Root).M), "t"),
                       []));
                   constraints = []};
                 representation = None});
               Odoc_model.Lang.Signature.Value
                {Odoc_model.Lang.Value.id =
                  `Value
                    (`Module (`Root (Common.root, <abstr>), Mextended), id);
                 doc = <<docs>>;
                 type_ =
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Root (Common.root, <abstr>), Mextended),
                              $t$1))),
                    []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved
                       (`Identifier
                          (`Type
                             (`Module
                                (`Root (Common.root, <abstr>), Mextended),
                              $t$1))),
                    []))}]}};
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id = (root Root).Mextended.t;
           doc = <<docs>>;
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Value
          {Odoc_model.Lang.Value.id =
            `Value
              (`Module (`Root (Common.root, <abstr>), Mextended), ignore);
           doc = <<docs>>;
           type_ =
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type
                       (`Module (`Root (Common.root, <abstr>), Mextended), t))),
              []),
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType unit)), []))}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig})])
```
