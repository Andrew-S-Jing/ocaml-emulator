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
%token TIMES DIVIDE
%token LESSTHAN EQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token TRUE FALSE
%token UNIT
%token FNEG
%token FPLUS FMINUS
%token FTIMES FDIVIDE
%token CONCAT
%token HEAD TAIL
%token LOPEN LCLOSE
%token SCOLON
%token LEMPTY
%token CONS
%token NOSTRING
%token REF DEREF
%token ASSIGN

(* Associativity and precedence *)
%right SCOLON
%nonassoc IF
%right ASSIGN
%left LESSTHAN EQUALS
%right CONCAT
%right CONS
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIVIDE FTIMES FDIVIDE
%nonassoc NEG FNEG
%nonassoc DEREF

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
        | exp DIVIDE exp        { Binop(Divide, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | IF exp THEN exp ELSE exp        { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp        { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp    { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) }
        | FUNCTION UNIT DOT exp { FunUnit $4 }
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
        | UNIT                  { Unit }
        | FLOAT                 { Float $1 }
        | CHAR                  { Char $1 }
        | NOSTRING              { String "" }
        | STRING                { String $1 }
        | exp FPLUS exp         { Binop(FPlus, $1, $3) }
        | exp FMINUS exp        { Binop(FMinus, $1, $3) }
        | exp FTIMES exp        { Binop(FTimes, $1, $3) }
        | exp FDIVIDE exp        { Binop(FDivide, $1, $3) }
        | FNEG exp              { Unop(FNegate, $2) }
        | exp CONCAT exp        { Binop(Concat, $1, $3) }
        | LET ID fun_construct exp        { Let($2, $3, $4) }
        | LET REC ID fun_construct exp    { Letrec($3, $4, $5) }
        | LEMPTY                { List Empty }
        | LOPEN list_elt LCLOSE { List $2 }
        | exp CONS exp          { Binop(Cons, $1, $3) }
        | HEAD exp              { Unop(Head, $2) }
        | TAIL exp              { Unop(Tail, $2) }
        | REF exp               { Unop(Ref, $2) }
        | DEREF exp             { Unop(Deref, $2) }
        | exp ASSIGN exp        { Binop(Assign, $1, $3) }
        | exp SCOLON exp        { LetUnit($1, $3) }
        | LET UNIT EQUALS exp IN exp      { LetUnit($4, $6) }

list_elt: exp                             { Cons($1, Empty) }
        | exp SCOLON                      { Cons($1, Empty) }
        | exp SCOLON list_elt SCOLON      { Cons($1, $3) }
        | exp SCOLON list_elt             { Cons($1, $3) }

fun_construct: UNIT EQUALS exp IN         { FunUnit $3 }
             | ID EQUALS exp IN           { Fun($1, $3) }
             | UNIT fun_construct         { FunUnit $2 }
             | ID fun_construct           { Fun($1, $2) }

;

%%
