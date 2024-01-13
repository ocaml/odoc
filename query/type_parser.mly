(* Type expressions parser, with error correction
   to support incomplete / partially written user queries. *)

%{
  module Printf = struct
    (* Without the following placeholder, [menhir_fail] induces
       a large dependency to [camlinternalFormat] in the js bundle. *)
    let eprintf _ = ()
  end

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
  ;

typ:
  | t=typ2 { t }
  | a=typ2 ARROW b=typ { Arrow (a, b) }
  ;

typ2:
  | xs=list1(typ1, STAR) { tuple xs }
  ;

typ1:
  | { Any }
  | ts=typs { tuple ts }
  | ts=typs w=WORD ws=list(WORD) {
      List.fold_left (fun acc w -> Constr (w, [acc])) (Constr (w, ts)) ws
    }
  ;

typ0:
  | ANY { Any }
  | w=POLY { Poly w }
  | w=WORD { Constr (w, []) }
  ;

typs:
  | t=typ0 { [t] }
  | PARENS_OPEN ts=list1(typ, COMMA) PARENS_CLOSE { ts }
  ;

list1(term, separator):
  | x=term { [x] }
  | x=term separator xs=list1(term, separator) { x::xs }
  ;
