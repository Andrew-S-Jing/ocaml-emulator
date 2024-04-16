(* Final Project testing *)
open CS51Utils.Absbook ;;
let test f tests results str =
  print_endline ("\nTESTING: " ^ str);
  let rec test' ts ress =
    match ts, ress with
    | [], [] -> print_endline ("finished: " ^ str)
    | [], _
    | _, [] ->
        raise (Invalid_argument ("test: unequal test-list lengths for " ^ str))
    | (name, arg) :: t1, res :: t2 ->
        unit_test ((f arg) = res) (name ^ ": ");
        test' t1 t2 in
  test' tests results;
  print_endline ("FINISHED: " ^ str)

open Expr;;
let tests_wellformed =
  [
    (*1*)  "Var", Var "x";
    (*2*)  "Num", Num 0;
    (*3*)  "Bool", Bool true;
    (*4*)  "Negate 0", Unop (Negate, Num 0);
    (*5*)  "Negate +", Unop (Negate, Num 1);
    (*6*)  "Negate -", Unop (Negate, Num ~-1);
    (*7*)  "Negate x", Unop (Negate, Var "x");
    (*8*)  "Plus 1 1", Binop (Plus, Num 1, Num 1);
    (*9*)  "Plus ~-4 x", Binop (Plus, Num ~-4, Var "x");
    (*10*) "Minus 1 1", Binop (Minus, Num 1, Num 1);
    (*11*) "Minus x ~-1", Binop (Minus, Var "x", Num ~-1);
    (*12*) "Times 2 ~-2", Binop (Times, Num 2, Num ~-2);
    (*13*) "Times ~-1 x", Binop (Times, Num ~-1, Var "x");
    (*14*) "Equals t f", Binop (Equals, Bool true, Bool false);
    (*15*) "Equals x 1", Binop (Equals, Var "x", Num 1);
    (*16*) "Equals 1 2", Binop (Equals, Num 1, Num 2);
    (*17*) "Equals x y", Binop (Equals, Var "x", Var "y");
    (*18*) "LessThan 0 1", Binop (LessThan, Num 0, Num 1);
    (*19*) "LessThan x 5", Binop (LessThan, Var "x", Num 5);
    (*20*) "Cond t t f", Conditional (Bool true, Bool true, Bool false);
    (*21*) "Cond f x y", Conditional (Bool false, Var "x", Var "y");
    (*22*) "Fun x -> 1", Fun ("x", Num 1);
    (*23*) "Fun y -> y = true", Fun ("y", Binop (Equals, Var "y", Bool true));
    (*24*) "Fun z -> if z = false then 0 else 1",
            Fun ("z", Conditional (Binop (Equals, Var "z", Bool false),
                                   Num 0, Num 1));
    (*25*) "Let x 1 (x + 1)", Let ("x", Num 1, Binop (Plus, Var "x", Num 1));
    (*26*) "Letrec f (fun x -> if x = 0 then true else f (x - 1)) in f 5",
            Letrec ("f",
                    Fun ("x",
                         Conditional (Binop (Equals, Var "x", Num 0),
                                      Bool true,
                                      App (Var "f",
                                           Binop (Minus, Var "x", Num 1)))),
                    App (Var "f", Num 5));
    (*27*) "Letrec f (fun f -> if f = 0 then true else f (f - 1)) in f 5",
            Letrec ("f",
                    Fun ("f",
                         Conditional (Binop (Equals, Var "f", Num 0),
                                      Bool true,
                                      App (Var "f",
                                           Binop (Minus, Var "f", Num 1)))),
                    App (Var "f", Num 5));
    (*28*) "Raise", Raise;
    (*29*) "Unassigned", Unassigned;
    (*30*) "App (fun x -> x + 1) 2",
            App (Fun ("x", Binop (Plus, Var "x", Num 1)), Num 2);
  ] ;;


(* TESTING: module Expr *)
(* test exp_to_concrete_string *)
let results_etcs =
  [
    (* WELL FORMED: *)
    (*1*)  "x";
    (*2*)  "0";
    (*3*)  "true";
    (*4*)  "0";
    (*5*)  "~-1";
    (*6*)  "1";
    (*7*)  "~-x";
    (*8*)  "(1 + 1)";
    (*9*)  "(~-4 + x)";
    (*10*) "(1 - 1)";
    (*11*) "(x - ~-1)";
    (*12*) "(2 * ~-2)";
    (*13*) "(~-1 * x)";
    (*14*) "(true = false)";
    (*15*) "(x = 1)";
    (*16*) "(1 = 2)";
    (*17*) "(x = y)";
    (*18*) "(0 < 1)";
    (*19*) "(x < 5)";
    (*20*) "if true then\n  true\nelse\n  false";
    (*21*) "if false then\n  x\nelse\n  y";
    (*22*) "(fun x ->\n  1)";
    (*23*) "(fun y ->\n  (y = true))";
    (*24*) "(fun z ->\n  if (z = false) then\n    0\n  else\n    1)";
    (*25*) "let x =\n  1 in\n(x + 1)";
    (*26*) "let rec f =\n  (fun x ->\n    if (x = 0) then\n"
             ^ "      true\n    else\n      (f (x - 1))) in\n(f 5)";
    (*27*) "let rec f =\n  (fun f ->\n    if (f = 0) then\n"
             ^ "      true\n    else\n      (f (f - 1))) in\n(f 5)";
    (*28*) "(raise EvalException)";
    (*29*) "";
    (*30*) "((fun x ->\n  (x + 1)) 2)";
  ] in
test exp_to_concrete_string
     tests_wellformed results_etcs
     "exp_to_concrete_string" ;;

(* test: exp_to_abstract_string *)
let results_etas =
  [
    (* WELL FORMED: *)
    (*1*)  "Var(x)";
    (*2*)  "Num(0)";
    (*3*)  "Bool(true)";
    (*4*)  "Unop(Negate, Num(0))";
    (*5*)  "Unop(Negate, Num(1))";
    (*6*)  "Unop(Negate, Num(~-1))";
    (*7*)  "Unop(Negate, Var(x))";
    (*8*)  "Binop(Plus, Num(1), Num(1))";
    (*9*)  "Binop(Plus, Num(~-4), Var(x))";
    (*10*) "Binop(Minus, Num(1), Num(1))";
    (*11*) "Binop(Minus, Var(x), Num(~-1))";
    (*12*) "Binop(Times, Num(2), Num(~-2))";
    (*13*) "Binop(Times, Num(~-1), Var(x))";
    (*14*) "Binop(Equals, Bool(true), Bool(false))";
    (*15*) "Binop(Equals, Var(x), Num(1))";
    (*16*) "Binop(Equals, Num(1), Num(2))";
    (*17*) "Binop(Equals, Var(x), Var(y))";
    (*18*) "Binop(LessThan, Num(0), Num(1))";
    (*19*) "Binop(LessThan, Var(x), Num(5))";
    (*20*) "Conditional(Bool(true), Bool(true), Bool(false))";
    (*21*) "Conditional(Bool(false), Var(x), Var(y))";
    (*22*) "Fun(x, Num(1))";
    (*23*) "Fun(y, Binop(Equals, Var(y), Bool(true)))";
    (*24*) "Fun(z, Conditional(Binop(Equals, Var(z), Bool(false)), "
                            ^ "Num(0), Num(1)))";
    (*25*) "Let(x, Num(1), Binop(Plus, Var(x), Num(1)))";
    (*26*) "Letrec(f, "
                ^ "Fun(x, "
                    ^ "Conditional(Binop(Equals, Var(x), Num(0)), "
                                ^ "Bool(true), "
                                ^ "App(Var(f), "
                                ^ "Binop(Minus, Var(x), Num(1))))), "
                ^ "App(Var(f), Num(5)))";
    (*27*) "Letrec(f, "
                ^ "Fun(f, "
                    ^ "Conditional(Binop(Equals, Var(f), Num(0)), "
                                ^ "Bool(true), "
                                ^ "App(Var(f), "
                                ^ "Binop(Minus, Var(f), Num(1))))), "
                ^ "App(Var(f), Num(5)))";
    (*28*) "Raise";
    (*29*) "Unassigned";
    (*30*) "App(Fun(x, Binop(Plus, Var(x), Num(1))), Num(2))";
  ] in
test exp_to_abstract_string
     tests_wellformed results_etas
     "exp_to_abstract_string" ;;