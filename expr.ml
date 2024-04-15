(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let free_vars (exp : expr) : varidset =
  failwith "free_vars not implemented" ;;
  
(* new_varname () -- Returns a freshly minted `varid` with prefix
   "var" and a running counter a la `gensym`. Assumes no other
   variable names use the prefix "var". (Otherwise, they might
   accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let exp_to_concrete_string (exp : expr) : string =
  let parenthensize x = "(" ^ x ^ ")" in
  let rec to_string spaces e =
    let to_string' = to_string spaces in
    let str = 
      match e with
      | Var x -> x
      | Num x -> string_of_int x
      | Bool x -> string_of_bool x
      | Unop (Negate, x) -> parenthensize ("~-" ^ to_string' x)
      | Binop (op, x, y) ->
          let binop_combine s =
            parenthensize (to_string' x ^ " "
                          ^ s ^ " "
                          ^ to_string' y) in
          (match op with
           | Plus -> binop_combine "+"
           | Minus -> binop_combine "-"
           | Times -> binop_combine "*"
           | Equals -> binop_combine "="
           | LessThan -> binop_combine "<")
      | Conditional (c, t, f) ->
          let spaces' = spaces + 2 in
          "if " ^ to_string' c
          ^ " then\n" ^ to_string spaces' t
          ^ "\n else\n" ^ to_string spaces' f
      | Fun (v, x) ->
          parenthensize ("fun " ^ v ^ " -> " ^ to_string' x)
      | Let (v, p, q) ->
          "let " ^ v ^
          " =\n" ^ to_string (spaces + 2) p
          ^ " in\n" ^ to_string' q
      | Letrec (v, p, q) ->
          "let rec " ^ v
          ^ " =\n" ^ to_string (spaces + 2) p
          ^ " in\n" ^ to_string' q
      | Raise -> "raise an exception"
      | Unassigned -> "unbound variable"
      | App (f, x) ->
          parenthensize (to_string' f ^ " " ^ to_string' x) in
    String.make spaces ' ' ^ str in
  to_string 0 exp ;;

     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let exp_to_abstract_string (exp : expr) : string =
  failwith "exp_to_abstract_string not implemented" ;;
