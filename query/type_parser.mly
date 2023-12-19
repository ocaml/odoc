(* This is a parser for type expressions. It is written in a weird style to
   allow for incomplete queries to be reasonably answered. It also has conflicts
   for the same reason. They are impossible to solve.
   Its behaviour on correct types is tested in [query/test/test_type_parser.ml]
   and its behaviour on incomplete types is tested in [test/cram/query_syntax.t/run.t] *)

%{
  open Db.Typexpr
%}

%token EOF
%token PARENS_OPEN PARENS_CLOSE
%token ARROW COMMA ANY STAR
%token<string> WORD
%token<string> POLY

%start main
%type<Db.Typexpr.t> main

%%

main:
  | t=typ EOF { t }
  | EOF { any }
  ;

typ:
  | a=typ1 ARROW b=typ { arrow a b }
  | a=typ1 ARROW EOF { arrow a any }
  | ARROW b=typ { arrow any b }
  | ARROW EOF { arrow any any }
  | t=typ1 { t }
  ;

typ1:
  | x=typ0 xs=tups { match xs with [] -> x | xs -> tuple (x::xs) }
  ;

tups:
  | STAR x=typ0 xs=tups { x::xs }
  | STAR { [any] }
  | EOF { [] }
  | { [] }
  ;

typ0:
  | ANY { any }
  | w=POLY { poly w }
  | w=WORD { constr w [] }
  | t=typ0 w=WORD { constr w [t] }
  | PARENS_OPEN ts=typ_list PARENS_CLOSE w=WORD { constr w ts }
  | PARENS_OPEN t=typ PARENS_CLOSE { t }
  | PARENS_OPEN t=typ EOF { t }
  | PARENS_OPEN EOF { any }
  ;

typ_list: ts=separated_list(COMMA, typ) { ts } ;
