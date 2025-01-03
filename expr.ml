(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

let cTAB_SIZE = 2 ;;

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | FNegate
  | Head
  | Tail
  | Ref
  | Deref
;;
    
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

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Unit                                 (* unit *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | Char of char                         (* char *)
  | String of string                     (* strings *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | FunUnit of expr                      (* functions of type unit -> 'a *)
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
  | Unit
  | Num _
  | Float _
  | Bool _
  | Char _
  | String _ -> SS.empty
  | Unop (_op, x) -> free_vars x
  | Binop (_op, x, y) -> SS.union (free_vars x) (free_vars y)
  | Conditional (c, t, f) ->
      free_vars f
      |> SS.union (free_vars t)
      |> SS.union (free_vars c)
  | Fun (v, x) -> SS.remove v (free_vars x)
  | FunUnit x
  | FunWild x -> free_vars x
  | Let (v, x, y)
  | Letrec (v, x, y) ->
      SS.union (SS.remove v (free_vars y)) (free_vars x)
  | LetUnit (x, y)
  | LetWild (x, y) -> SS.union (free_vars x) (free_vars y)
  | Raise -> SS.empty
  | Unassigned -> SS.empty
  | App (f, x) -> SS.union (free_vars f) (free_vars x)
  | List Empty -> SS.empty
  | List (Elt (hd, tl)) ->
      SS.union (free_vars hd) (free_vars (List tl))
  | ClosList _ -> raise (Failure "free_vars: closure in eval_s model") ;;
  
(* new_varname () -- Returns a freshly minted `varid` with prefix
   "var" and a running counter a la `gensym`. Assumes no other
   variable names use the prefix "var". (Otherwise, they might
   accidentally be the same as a generated variable name.) *)
let new_varname : unit -> varid =
  let suffix = ref ~-1 in
  fun () -> incr suffix;
            "var" ^ (string_of_int !suffix) ;;

let new_refname : unit -> varid =
  let suffix = ref ~-1 in
  fun () -> incr suffix;
            "_l" ^ (string_of_int !suffix) ;;
let is_refname (v : varid) : bool =
  v.[0] = '_' && v.[1] = 'l' ;;

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
  let frees = free_vars repl in
  let is_v_in_free_vars v = SS.mem v frees in
  match exp with
  | Var v -> if v = var_name then repl else Var v
  | Unit -> Unit
  | Num n -> Num n
  | Float f -> Float f
  | Bool b -> Bool b
  | Char c -> Char c
  | String s -> String s
  | Unop (op, x) -> Unop (op, subst' x)
  | Binop (op, x, y) -> Binop (op, subst' x, subst' y)
  | Conditional (c, t, f) -> Conditional (subst' c, subst' t, subst' f)
  | Fun (v, x) ->
      if v = var_name then Fun (v, x)
      else if is_v_in_free_vars v then
        let v' = new_varname () in
        let x' = (subst v (Var v') x) |> subst' in
        Fun (v', x')
      else Fun (v, subst' x)
  | FunUnit x -> FunUnit (subst' x)
  | FunWild x -> FunWild (subst' x)
  | Let (v, x, y) ->
      if v = var_name then Let (v, subst' x, y)
      else if is_v_in_free_vars v then
        let v' = new_varname () in
        let y' = (subst v (Var v') y) |> subst' in
        Let (v', subst' x, y')
      else Let (v, subst' x, subst' y)
  | Letrec (v, x, y) ->
      if v = var_name then Letrec (v, subst' x, y)
      else if is_v_in_free_vars v then
        let v' = new_varname () in
        let y' = (subst v (Var v') y) |> subst' in
        Letrec (v', subst' x, y')
      else Letrec (v, subst' x, subst' y)
  | LetUnit (x, y) -> LetUnit (subst' x, subst' y)
  | LetWild (x, y) -> LetWild (subst' x, subst' y)
  | Raise -> Raise
  | Unassigned -> Unassigned
  | App (f, x) -> App (subst' f, subst' x)
  | List Empty -> List Empty
  | List (Elt (hd, tl)) ->
      let tl' =
        match subst' (List tl) with
        | List x -> x
        | _ -> raise (Failure "reached impossible branch") in
      List (Elt (subst' hd, tl'))
  | ClosList _ -> raise (Failure "subst: closure in eval_s model") ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let exp_to_concrete_string (exp : expr) : string =
  let parenthensize x = "(" ^ x ^ ")" in
  let rec to_string ?(is_list = false) tabs e =
    let to_string' = to_string tabs in
    let to_string_tab = to_string (tabs + 1) in
    let enter = "\n" ^ String.make (tabs * cTAB_SIZE) ' ' in
    let enter_tab = enter ^ String.make cTAB_SIZE ' ' in
    let str = 
      match e with
      | Var v -> v
      | Unit -> "()"
      | Num n -> if n < 0 then "~" ^ string_of_int n else string_of_int n
      | Float f -> if f < 0. then "~" ^ string_of_float f else string_of_float f
      | Bool b -> string_of_bool b
      | Char c -> "\'" ^ Char.escaped c ^ "\'"
      | String s -> "\"" ^ s ^ "\""
      | Unop (op, x) ->
          (match op, x with
           | Negate, Num n -> to_string' (Num ~-n)
           | FNegate, Float f -> to_string' (Float ~-.f)
           | Negate, _ -> "~-" ^ to_string' x
           | FNegate, _ -> "~-." ^ to_string' x
           | Head, List (Elt (hd, _tl)) -> to_string' hd
           | Head, ClosList (Elt (hd, _tl)) -> "env: " ^ hd
           | Tail, List (Elt (_hd, tl)) -> to_string' (List tl)
           | Tail, ClosList (Elt (_hd, tl)) -> to_string' (ClosList tl)
           | Head, _ -> "head " ^ to_string' x
           | Tail, _ -> "tail " ^ to_string' x
           | Ref, _ -> "ref " ^ to_string' x
           | Deref, _ -> "!" ^ to_string' x)
      | Binop (op, x, y) ->
          let op_str = 
            match op with
            | Plus -> "+"
            | Minus -> "-"
            | Times -> "*"
            | Divide -> "/"
            | Equals -> "="
            | LessThan -> "<"
            | FPlus -> "+."
            | FMinus -> "-."
            | FTimes -> "*."
            | FDivide -> "/."
            | Concat -> "^"
            | Cons -> "::"
            | Assign -> ":=" in
          parenthensize (String.concat " " [to_string' x; op_str; to_string' y])
      | Conditional (c, t, f) ->
          String.concat "" ["if "; to_string' c; " then"; enter_tab;
                            to_string' t; enter;
                            "else"; enter_tab;
                            to_string_tab f]
      | Fun (v, x) ->
          parenthensize (String.concat "" ["fun "; v; " ->"; enter_tab;
                                           to_string_tab x])
      | FunUnit x -> to_string' (Fun ("()", x))
      | FunWild x -> to_string' (Fun ("_", x))
      | Let (v, p, q) ->
          String.concat "" ["let "; v; " ="; enter_tab;
                            to_string_tab p; " in"; enter;
                            to_string' q]
      | Letrec (v, p, q) -> to_string' (Let ("rec " ^ v, p, q))
      | LetUnit (p, q) -> to_string' (Let ("()", p, q))
      | LetWild (p, q) -> to_string' (Let ("_", p, q))
      | Raise -> "(raise EvalException)"
      | Unassigned -> "Unassigned"
      | App (f, x) ->
          parenthensize (String.concat " " [to_string' f; to_string' x])
      | List Empty
      | ClosList Empty -> if is_list then "]" else "[]"
      | List (Elt (hd, tl)) ->
          let start, sep = if is_list then "", "; " else "[", "" in
          start ^ sep ^ (to_string' hd)
                ^ (to_string ~is_list:true tabs (List tl))
      | ClosList (Elt (hd, tl)) ->
          let start = if is_list then "" else "[" in
          start ^ "closure: " ^ hd
                ^ (to_string ~is_list:true tabs (ClosList tl)) in
    str in
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
    | Unit -> "Unit"
    | Num n ->
        form "Num" [if n < 0 then "~" ^ string_of_int n else string_of_int n]
    | Float f ->
        form "Float" [if f < 0. then "~" ^ string_of_float f
                      else string_of_float f]
    | Bool b -> form "Bool" [string_of_bool b]
    | Char c -> form "Char" ["\'" ^ Char.escaped c ^ "\'"]
    | String s -> form "String" ["\"" ^ s ^ "\""]
    | Unop (op, x) ->
        let op_name =
          match op with
          | Negate -> "Negate"
          | FNegate -> "FNegate"
          | Head -> "Head"
          | Tail -> "Tail"
          | Ref -> "Ref"
          | Deref -> "Deref" in
        form "Unop" [op_name; to_string x]
    | Binop (op, x, y) ->
        let op_name =
          match op with
          | Plus -> "Plus"
          | Minus -> "Minus"
          | Times -> "Times"
          | Divide -> "Divide"
          | Equals -> "Equals"
          | LessThan -> "LessThan"
          | FPlus -> "FPlus"
          | FMinus -> "FMinus"
          | FTimes -> "FTimes"
          | FDivide -> "FDivide"
          | Concat -> "Concat"
          | Cons -> "Cons"
          | Assign -> "Assign" in
        form "Binop" [op_name; to_string x; to_string y]
    | Conditional (c, t, f) ->
        form "Conditional" [to_string c; to_string t; to_string f]
    | Fun (v, x) -> form "Fun" [v; to_string x]
    | FunUnit x -> form "FunUnit" ["()"; to_string x]
    | FunWild x -> form "FunWild" ["_"; to_string x]
    | Let (v, x, y) -> form "Let" [v; to_string x; to_string y]
    | Letrec (v, x, y) -> form "Letrec" [v; to_string x; to_string y]
    | LetUnit (x, y) -> form "LetUnit" [to_string x; to_string y]
    | LetWild (x, y) -> form "LetWild" [to_string x; to_string y]
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (f, x) -> form "App" [to_string f; to_string x]
    | List Empty -> "List(Empty)"
    | ClosList Empty -> "ClosList(Empty)"
    | List (Elt (hd, tl)) -> form "List" [to_string hd; to_string (List tl)]
    | ClosList (Elt (hd, tl)) ->
        form "ClosList" ["closure: " ^ hd; to_string (ClosList tl)] in
  to_string exp ;;