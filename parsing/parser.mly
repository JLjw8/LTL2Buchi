%{
open Ast
%}

%token <bool> BOOL
%token <string> APR
%token NEG 
%token AND
%token OR 
%token IMPLIES
%token EQV
%token UNTIL   
%token WEAKU
%token RELEASE
%token ALWAYS
%token FINALLY 
%token LPAREN
%token RPAREN
%token EOF


%start <Ast.formula> prog

%%

prog: 
  |f = formula; EOF { f }
  ;

formula:
  | b = BOOL { Bool b }
	| a = APR { AP a }
  | f1 = formula; lb = logic_bin; f2 = formula { LOP_bin (lb, ref f1, ref f2) } 
  | lu = logic_un; f = formula { LOP_un (lu, ref f) } 
  | f1 = formula; tb = temporal_bin; f2 = formula { TOP_bin (ref f1, tb, ref f2) } 
  | tu = temporal_un; f = formula { TOP_un (tu, ref f) } 
  | LPAREN; f1 = formula; RPAREN {f1}
  ;

  %inline logic_bin:
    | AND { And }
    | OR { Or }
    | IMPLIES { Implies }
    | EQV {Eqv}
    ;

  %inline logic_un:
    | NEG { Neg }
    ;

  %inline temporal_bin:
    | UNTIL { Until }
    | WEAKU {WeakU}
    | RELEASE { Release }
    ;

  %inline temporal_un:
    | FINALLY { Finally }
    | ALWAYS { Always }
    ;



