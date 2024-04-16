(*
                         CS 51 Final Project
                           MiniML -- Parser

   This grammar is processed by menhir. See
   http://gallium.inria.fr/~fpottier/menhir/manual.html and
   https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html
   for documentation of the various declarations used here.  *)
                  
%{
  open Expr ;;
%}

(* Tokens *)
%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG
%token PLUS MINUS 
%token TIMES
%token LESSTHAN EQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token TRUE FALSE

(* Associativity and precedence *)
%nonassoc IF
%left LESSTHAN EQUALS
%left PLUS MINUS
%left TIMES
%nonassoc NEG

(* Start symbol of the grammar and its type *)
%start input
%type <Expr.expr> input

(* Grammar rules with actions to build an `expr` value as defined in
   the `Expr` module. *)
%%
input:  exp EOF                 { $1 }

(* expressions *)
exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

(* expressions except for application expressions *)
expnoapp: INT                   { Num $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
;

%%
