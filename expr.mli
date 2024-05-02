(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(* ......................................................................
  Abstract syntax of MiniML expressions 
 *)

(* Unary operators *)
type unop =
  | Negate
  | FNegate
  | Head
  | Tail
  | Ref
  | Deref
;;

(* Binary operators *)
type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Equals
  | LessThan
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | Concat
  | Cons
  | Assign
;;

(* Variable identifers *)
type varid = string ;;
  
(* Expressions *)
type expr =
  | Var of varid                         (* variables *)
  | Unit                                 (* Unit *)
  | Num of int                           (* integers *)
  | Float of float                       (* float *)
  | Bool of bool                         (* booleans *)
  | Char of char                         (* characters *)
  | String of string                     (* strings *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | FunUnit of expr                      (* function of type unit -> 'a *)
  | FunWild of expr                      (* function of type _ -> 'a *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | LetUnit of expr * expr               (* let () = P in Q *)
  | LetWild of expr * expr               (* let _  = P in Q *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | List of expr list_internal           (* lists *)
  | ClosList of varid list_internal      (* lists of closures (undeclarable) *)
 and 'a list_internal =
   | Empty
   | Elt of 'a * 'a list_internal
;;
    
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
type varidset ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
val same_vars : varidset -> varidset -> bool ;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
val vars_of_list : varid list -> varidset ;;

(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
val free_vars : expr -> varidset ;;

(* new_varname () -- Returns a freshly minted varname-type `varid` *)
val new_varname : unit -> varid ;;

(* new_refname () -- Returns a freshly minted refname-type `varid` *)
val new_refname : unit -> varid ;;

(* is_refname v -- Returns true if v starts with "_l" (i.e. is a refname) *)
val is_refname : varid -> bool ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)
                            
(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
val subst : varid -> expr -> expr -> expr ;;
     
(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
val exp_to_concrete_string : expr -> string ;;

val exp_to_abstract_string : expr -> string ;;

