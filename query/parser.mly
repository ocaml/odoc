(* This parser parses types as inputed by the user in a query.
   It is made in weird way because it is able to correctly parse incomplete
   types. It has conflicts because of this, which are impossible to resolve
   without losing functionnality. *)

%{
  open Db.Typepath
%}

%token EOF
%token PARENS_OPEN PARENS_CLOSE
%token ARROW COMMA ANY STAR
%token<string> WORD
%token<string> POLY

%start main
%type< Db.Typepath.typ> main

%left EOF
%%

separated_twolong_list(sep, elt):
  | e1=elt sep e2=elt sep li=separated_list(sep, elt) { e1 :: e2 :: li }

main:
  | t=typ EOF { t }
  | EOF { Any }
  ;

typ:
  | a=typ1 ARROW b=typ { Arrow (a, b) }
  | a=typ1 ARROW { Arrow (a, Any) }
  | ARROW b=typ { Arrow (Any, b) }
  | ARROW EOF { Arrow (Any, Any) }
  | t=typ1 { t }
  ;

typ1:
  | x=typ0 xs=tups { match xs with [] -> x | xs -> Tuple (x::xs) }
  ;

tups:
  | STAR x=typ0 xs=tups { x::xs }
  | STAR { [Any] }
  | EOF { [] }
  | { [] }
  ;

typ0:
  | ANY { Any }
  | w=POLY { Poly w }
  | w=WORD { Constr (w, []) }
  | t=typ0 w=WORD { Constr (w, [t]) }
  | PARENS_OPEN ts=typ_list PARENS_CLOSE w=WORD { Constr (w, ts) }
  | PARENS_OPEN t=typ PARENS_CLOSE { t }
  | PARENS_OPEN t=typ EOF { t }
  ;

(* ( int EOF EOF *)

typ_list: ts=separated_twolong_list(COMMA, typ) { ts } ;
