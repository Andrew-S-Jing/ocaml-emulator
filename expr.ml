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
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v
  | Num _ -> SS.empty
  | Bool _ -> SS.empty
  | Unop (_op, x) -> free_vars x
  | Binop (_op, x, y) -> SS.union (free_vars x) (free_vars y)
  | Conditional (c, t, f) ->
      free_vars f
      |> SS.union (free_vars t)
      |> SS.union (free_vars c)
  | Fun (v, x) -> SS.remove v (free_vars x)
  | Let (v, x, y) ->
      SS.union (SS.remove v (free_vars y)) (free_vars x)
  | Letrec (v, x, y) ->
      SS.remove v (SS.union (free_vars y) (free_vars x))
  | Raise -> SS.empty
  | Unassigned -> SS.empty
  | App (f, x) -> SS.union (free_vars f) (free_vars x) ;;
  
(* new_varname () -- Returns a freshly minted `varid` with prefix
   "var" and a running counter a la `gensym`. Assumes no other
   variable names use the prefix "var". (Otherwise, they might
   accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  let suffix = ref ~-1 in
  incr suffix;
  "var" ^ string_of_int !suffix ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let subst' = subst var_name repl in
  let is_v_bound v = SS.mem v (free_vars repl) in
  match exp with
  | Var v -> if v = var_name then repl else Var v
  | Num n -> Num n
  | Bool b -> Bool b
  | Unop (op, x) -> Unop (op, subst' x)
  | Binop (op, x, y) -> Binop (op, subst' x, subst' y)
  | Conditional (c, t, f) -> Conditional (subst' c, subst' t, subst' f)
  | Fun (v, x) ->
      if v = var_name then Fun (v, x)
      else if is_v_bound v then
        let v' = new_varname () in
        let x' = subst v (Var v') x |> subst' in
        Fun (v', x')
      else Fun (v, subst' x)
  | Let (v, x, y) ->
      if v = var_name then Let (v, subst' x, y)
      else if is_v_bound v then
        let v' = new_varname () in
        let y' = subst v (Var v') y |> subst' in
        Let (v', subst' x, y')
      else Let (v, subst' x, subst' y)
  | Letrec (v, x, y) ->
      if v = var_name then Let (v, x, y)
      else if is_v_bound v then
        let v' = new_varname () in
        let x' = subst v (Var v') x |> subst' in
        let y' = subst v (Var v') y |> subst' in
        Letrec (v', x', y')
      else Letrec (v, x, subst' y)
  | Raise -> Raise
  | Unassigned -> Unassigned
  | App (f, x) -> App (subst' f, subst' x) ;;
     
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
      | Var v -> v
      | Num n -> string_of_int n
      | Bool b -> string_of_bool b
      | Unop (op, x) ->
          (match op with
           | Negate -> parenthensize ("~-" ^ to_string' x))
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
  let parenthensize x = "(" ^ x ^ ")" in
  let form constructor internal =
    constructor ^ parenthensize (String.concat ", " internal) in
  let rec to_string e =
    match e with
    | Var v -> form "Var" [v]
    | Num n -> form "Num" [string_of_int n]
    | Bool b -> form "Bool" [string_of_bool b]
    | Unop (op, x) ->
        (match op with
         | Negate -> form "Unop" ["Negate"; to_string x])
    | Binop (op, x, y) ->
        let op_name =
          match op with
          | Plus -> "Plus"
          | Minus -> "Minus"
          | Times -> "Times"
          | Equals -> "Equals"
          | LessThan -> "LessThan" in
        form "Binop" [op_name; to_string x; to_string y]
    | Conditional (c, t, f) ->
        form "Conditional" [to_string c; to_string t; to_string f]
    | Fun (v, x) -> form "Fun" [v; to_string x]
    | Let (v, x, y) -> form "Let" [v; to_string x; to_string y]
    | Letrec (v, x, y) -> form "Letrec" [v; to_string x; to_string y]
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (f, x) -> form "App" [to_string f; to_string x] in
  to_string exp ;;