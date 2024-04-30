(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
           
    (* the type of values (including closures) stored in
       environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* copy env -- Returns a copied environment with structural equality
       but not physical equality. Useful for updating value pointers without
       affecting upstream pointer contents *)
    val copy : env -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = []

    let close (exp : expr) (env : env) : value =
      Closure (exp, env)

    let rec lookup (env : env) (varname : varid) : value =
      match env with
      | [] -> raise (EvalError ("unbound variable " ^ varname))
      | (var, x) :: tl ->
          if var = varname then !x else lookup tl varname

    let extend (env : env) (varname : varid) (loc : value ref) : env =
      match List.find_opt (fun (var, _pointer) -> var = varname) env with
      | None -> (varname, loc) :: env
      | Some (_var, pointer) -> pointer := !loc; env

    let copy (env : env) : env =
      List.map (fun (var, pointer) -> var, ref !pointer) env

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val exp -> exp_to_concrete_string exp
      | Closure (exp, env) ->
          let exp_str = exp_to_concrete_string exp in
          if printenvp then
            let rec to_string' acc e =
              match e with
              | [] -> acc
              | (var, x) :: tl ->
                  to_string' (var ^ " = " ^ value_to_string !x ^ "\n") tl in
            "Expression:" ^ exp_str ^ "\nEnvironment: " ^ to_string' "" env
          else "Expression: " ^ exp_str ^ "\n"

    let env_to_string (env : env) : string =
      let rec to_string' acc e =
        match e with
        | [] -> acc
        | (var, x) :: tl ->
            to_string' (var ^ " = " ^ value_to_string !x ^ "\n") tl in
      to_string' "" env
  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)
   
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_s' e =
    match e with
    | Var v -> raise (EvalError ("unbound variable: " ^ v))
    | Unit -> Unit
    | Num n ->  Num n
    | Float f -> Float f
    | Bool b -> Bool b
    | Char c -> Char c
    | String s -> String s
    | Unop (op, x) ->
        (match op, eval_s' x with
         | Negate, Num n -> Num ~-n
         | FNegate, Float f -> Float ~-.f
         | Negate, _ -> raise (EvalError "(~-) expects type int")
         | FNegate, _ -> raise (EvalError "(~-.) expects type float"))
    | Binop (op, x, y) ->
        (match op, eval_s' x, eval_s' y with
         | Plus, Num a, Num b -> Num (a + b)
         | Minus, Num a, Num b -> Num (a - b)
         | Times, Num a, Num b -> Num (a * b)
         | Equals, Num a, Num b -> Bool (a = b)
         | Equals, Float a, Float b -> Bool (a = b)
         | Equals, Bool a, Bool b -> Bool (a = b)
         | LessThan, Num a, Num b -> Bool (a < b)
         | FPlus, Float a, Float b -> Float (a +. b)
         | FMinus, Float a, Float b -> Float (a -. b)
         | FTimes, Float a, Float b -> Float (a *. b)
         | FPower, Float a, Float b -> Float (a ** b)
         | Concat, String a, String b -> String (a ^ b)
         | x, _, _ -> 
             let o, t =
               match x with
               | Plus -> "(+)", "int"
               | Minus -> "(-)", "int"
               | Times -> "( * )", "int"
               | Equals -> "(=)", "'a for both args"
               | LessThan -> "(<)", "int"
               | FPlus -> "(+.)", "float"
               | FMinus -> "(-.)", "float"
               | FTimes -> "( *. )", "float"
               | FPower -> "(**)", "float"
               | Concat -> "(^)", "string" in
             raise (EvalError (o ^ " expects type " ^ t)))
    | Conditional (c, t, f) ->
        (match eval_s' c with
         | Bool b -> if b then eval_s' t else eval_s' f
         | _ ->  raise (EvalError "conditional is expected to be type bool"))
    | Fun (v, x) -> Fun (v, x)
    | UnitFun x -> UnitFun x
    | Let (v, x, y) ->
        let x' = eval_s' x in
        eval_s' (subst v x' y)
    | Letrec (v, x, y) ->
        let x' = eval_s' x in
        eval_s' (subst v (subst v (Letrec (v, x', Var v)) x') y)
    | Raise -> raise EvalException
    | Unassigned -> raise (EvalError "Unassigned constructor called")
    | App (f, x) ->
        let f', x' = eval_s' f, eval_s' x in
        (match f' with
         | Fun (a, b) -> eval_s' (subst a x' b)
         | UnitFun b ->
             if x' = Unit then eval_s' b
             else raise (EvalError "function app expects type unit")
         | _ -> raise (EvalError "function expected of type 'a -> 'b"))
    | List Empty -> List Empty
    | List (Cons (hd, tl)) ->
        (match eval_s' (List tl) with
         | List x -> List (Cons (eval_s' hd, x))
         | _ -> raise (Failure "list should eval to list")) in
  Env.Val (eval_s' exp) ;;
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
   
let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  let eval_d' e = eval_d e env in
  match exp with
  | Var v -> Env.lookup env v
  | Unit -> Val Unit
  | Num n -> Val (Num n)
  | Float f -> Val (Float f)
  | Bool b -> Val (Bool b)
  | Char c -> Val (Char c)
  | String s -> Val (String s)
  | Unop (op, x) ->
      (match op, eval_d' x with
       | Negate, Val (Num n) -> Val (Num ~-n)
       | FNegate, Val (Float f) -> Val (Float ~-.f)
       | Negate, _ -> raise (EvalError "(~-) expects type int")
       | FNegate, _ -> raise (EvalError "(~-.) expects type float"))
  | Binop (op, x, y) ->
      (match op, eval_d' x, eval_d' y with
       | Plus, Val (Num a), Val (Num b) -> Val (Num (a + b))
       | Minus, Val (Num a), Val (Num b) -> Val (Num (a - b))
       | Times, Val (Num a), Val (Num b) -> Val (Num (a * b))
       | Equals, Val (Num a), Val (Num b) -> Val (Bool (a = b))
       | Equals, Val (Float a), Val (Float b) -> Val (Bool (a = b))
       | Equals, Val (Bool a), Val (Bool b) -> Val (Bool (a = b))
       | LessThan, Val (Num a), Val (Num b) -> Val (Bool (a < b))
       | FPlus, Val (Float a), Val (Float b) -> Val (Float (a +. b))
       | FMinus, Val (Float a), Val (Float b) -> Val (Float (a -. b))
       | FTimes, Val (Float a), Val (Float b) -> Val (Float (a *. b))
       | FPower, Val (Float a), Val (Float b) -> Val (Float (a ** b))
       | Concat, Val (String a), Val (String b) -> Val (String (a ^ b))
       | x, _, _ ->
           let o, t =
             match x with
             | Plus -> "(+)", "int"
             | Minus -> "(-)", "int"
             | Times -> "( * )", "int"
             | Equals -> "(=)", "'a for both args"
             | LessThan -> "(<)", "int"
             | FPlus -> "(+.)", "float"
             | FMinus -> "(-.)", "float"
             | FTimes -> "( *. )", "float"
             | FPower -> "(**)", "float"
             | Concat -> "(^)", "string" in
           raise (EvalError (o ^ " expects type " ^ t)))
  | Conditional (c, t, f) ->
      (match eval_d' c with
       | Val (Bool b) -> if b then eval_d' t else eval_d' f
       | _ -> raise (EvalError "conditional is expected to be of type bool"))
  | Fun (v, x) -> Val (Fun (v, x))
  | UnitFun x -> Val (UnitFun x)
  | Let (v, x, y)
  | Letrec (v, x, y) -> eval_d y (Env.extend env v (ref (eval_d' x)))
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "evaluated to \"Unassigned\"")
  | App (f, x) ->
      (match eval_d' f, eval_d' x with
       | Val (Fun (v, f')), (Val _ as x') -> 
           eval_d f' (Env.extend env v (ref x'))
       | Val (UnitFun f'), Val unit_ ->
           if unit_ = Unit then eval_d' f'
           else raise (EvalError "Fun app expects type unit")
       | Val _, _ -> raise (EvalError "Fun should be of type \"Fun\"")
       | _ -> raise (EvalError "eval_d: unexpected closure in dynamic env"))
  | List Empty -> Val (List Empty)
  | List (Cons (hd, tl)) ->
      (match eval_d' hd, eval_d' (List tl) with
       | Val a, Val (List b) -> Val (List (Cons (a, b)))
       | Closure _, _
       | _, Closure _ ->
           raise (Failure "eval_d: unexpected closure in dynamic env")
       | _ -> raise (Failure "list should eval to list")) ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  let eval_l' e = eval_l e env in
  match exp with
  | Var v -> Env.lookup env v
  | Unit -> Val Unit
  | Num n -> Val (Num n)
  | Float f -> Val (Float f)
  | Bool b -> Val (Bool b)
  | Char c -> Val (Char c)
  | String s -> Val (String s)
  | Unop (op, x) ->
      (match op, eval_l' x with
       | Negate, Val (Num n) -> Val (Num ~-n)
       | FNegate, Val (Float f) -> Val (Float ~-.f)
       | Negate, _ -> raise (EvalError "(~-) expects type int")
       | FNegate, _ -> raise (EvalError "(~-.) expects type float"))
  | Binop (op, x, y) ->
      (match op, eval_l' x, eval_l' y with
       | Plus, Val (Num a), Val (Num b) -> Val (Num (a + b))
       | Minus, Val (Num a), Val (Num b) -> Val (Num (a - b))
       | Times, Val (Num a), Val (Num b) -> Val (Num (a * b))
       | Equals, Val (Num a), Val (Num b) -> Val (Bool (a = b))
       | Equals, Val (Float a), Val (Float b) -> Val (Bool (a = b))
       | Equals, Val (Bool a), Val (Bool b) -> Val (Bool (a = b))
       | LessThan, Val (Num a), Val (Num b) -> Val (Bool (a < b))
       | FPlus, Val (Float a), Val (Float b) -> Val (Float (a +. b))
       | FMinus, Val (Float a), Val (Float b) -> Val (Float (a -. b))
       | FTimes, Val (Float a), Val (Float b) -> Val (Float (a *. b))
       | FPower, Val (Float a), Val (Float b) -> Val (Float (a ** b))
       | Concat, Val (String a), Val (String b) -> Val (String (a ^ b))
       | x, _, _ ->
           let o, t =
             match x with
             | Plus -> "(+)", "int"
             | Minus -> "(-)", "int"
             | Times -> "( * )", "int"
             | Equals -> "(=)", "'a for both args"
             | LessThan -> "(<)", "int"
             | FPlus -> "(+.)", "float"
             | FMinus -> "(-.)", "float"
             | FTimes -> "( *. )", "float"
             | FPower -> "(**)", "float"
             | Concat -> "(^)", "string" in
           raise (EvalError (o ^ " expects type " ^ t)))
  | Conditional (c, t, f) ->
      (match eval_l' c with
       | Val (Bool b) -> if b then eval_l' t else eval_l' f
       | _ -> raise (EvalError "conditional is expected to be of type bool"))
  | Fun (v, x) -> Closure (Fun (v, x), env)
  | UnitFun x -> Closure (UnitFun x, env)
  (* Let statement's lexical scoping needs to not be affected by future
     dynamical changes, so the downstream evaluation needs to have different
     value pointers, thus the `Env.copy` result being passed downstream. *)
  | Let (v, x, y) -> eval_l y (Env.extend (Env.copy env) v (ref (eval_l' x)))
  (* Letrec statement here follows the workaround enabled by the `Unassigned`
     constructor of the `Env.value` type *)
  | Letrec (v, x, y) ->
      let rec_pointer = ref (Env.Val Unassigned) in
      let env_lex = Env.extend env v rec_pointer in
      let v_D = eval_l x env_lex in
      rec_pointer := v_D;
      eval_l y env_lex
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "evaluated to \"Unassigned\"")
  | App (f, x) ->
      let dynamic = env in
      (match eval_l f dynamic, eval_l x dynamic with
       | Closure (Fun (v, f'), lexical), (Val _ as x') ->
           eval_l f' (Env.extend lexical v (ref x'))
       | Closure (UnitFun f', lexical), Val unit_ ->
           if unit_ = Unit then eval_l f' lexical
           else raise (EvalError "Fun app expects type ()")
       | Closure _, _ -> raise (EvalError "Fun should be of type \"Fun\"")
       | _ -> raise (Failure "eval_l: expects type \"Fun\" as a closure"))
  | List Empty -> Val (List Empty)
  | List (Cons (hd, tl)) ->
      (match eval_l' hd, eval_l' (List tl) with
       | Val a, Val (List b) -> Val (List (Cons (a, b)))
       | Closure (hd'exp, hd'env), Closure ((List _, _) as tl') ->
           Closure ()
       | _ -> raise (EvalError "'a list expects type 'a"))


(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)
   
let evaluate = eval_l ;;
