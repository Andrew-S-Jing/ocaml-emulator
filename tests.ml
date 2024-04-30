(* Final Project testing *)
open CS51Utils.Absbook ;;
let test (f : 'a -> 'b) (tests : (string * 'a) list) (results : 'b list) str =
  print_endline ("\nTESTING: " ^ str);
  let rec test' counter ts ress =
    match ts, ress with
    | [], [] -> print_endline ("FINISHED: " ^ str)
    | [], _
    | _, [] ->
        raise (Invalid_argument ("test: unequal test-list lengths for " ^ str))
    | (name, arg) :: t1, res :: t2 ->
        print_int counter;
        print_char ' ';
        unit_test ((f arg) = res) (name ^ ": ");
        test' (counter + 1) t1 t2 in
  test' 1 tests results ;;

open Expr;;
let tests_trivial =
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
    (* TRIVIAL: *)
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
    (*29*) "Unassigned";
    (*30*) "((fun x ->\n  (x + 1)) 2)";
  ] in
test exp_to_concrete_string
     tests_trivial results_etcs
     "exp_to_concrete_string" ;;

(* test: exp_to_abstract_string *)
let results_etas =
  [
    (* WELL FORMED TRIVIAL: *)
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
     tests_trivial results_etas
     "exp_to_abstract_string" ;;

(* test: subst *)
let tests_subst_wellformed =
  let s = subst "x" (Num 2) in
  (* a couple substitutions are evaluated here avoid repeat mutable iteration *)
  let a = subst "x" (Var "y") (Fun ("y", Binop (Plus, Var "y", Var "x"))) in
  (* let _ = print_endline (exp_to_concrete_string a) in *)
  let b = subst "x" (Var "y")
                (Let ("y", Binop (Plus, Var "x", Num 1),
                            Binop (Plus, Var "x", Var "y"))) in
  (* let _ = print_endline (exp_to_concrete_string b) in *)
  [
    (*1*)  "Var x", s (Var "x") = Num 2;
    (*2*)  "Var y", s (Var "y") = Var "y";
    (*3*)  "Num 0", s (Num 0) = Num 0;
    (*4*)  "Bool true", s (Bool true) = Bool true;
    (*5*)  "Negate x", s (Unop (Negate, Var "x")) = Unop (Negate, Num 2);
    (*6*)  "Negate y", s (Unop (Negate, Var "y")) = Unop (Negate, Var "y");
    (*7*)  "Plus 1 1", s (Binop (Plus, Num 1, Num 1))
                       = Binop (Plus, Num 1, Num 1);
    (*8*)  "Minus 1 x", s (Binop (Minus, Num 1, Var "x"))
                        = Binop (Minus, Num 1, Num 2);
    (*9*)  "Times x y", s (Binop (Times, Var "x", Var "y"))
                        = Binop (Times, Num 2, Var "y");
    (*10*) "Equals y z", s (Binop (Equals, Var "y", Var "z"))
                         = Binop (Equals, Var "y", Var "z");
    (*11*) "Lessthan x x", s (Binop (LessThan, Var "x", Var "x"))
                           = Binop (LessThan, Num 2, Num 2);
    (*12*) "Cond t t f", s (Conditional (Bool true, Bool true, Bool false))
                         = Conditional (Bool true, Bool true, Bool false);
    (*13*) "Fun x -> x+1", s (Fun ("x", Binop (Plus, Var "x", Num 1)))
                           = Fun ("x", Binop (Plus, Var "x", Num 1));
    (*14*) "Fun y -> x + 1", s (Fun ("y", Binop (Plus, Var "x", Num 1)))
                             = Fun ("y", Binop (Plus, Num 2, Num 1));
    (*15*) "Fun y -> y + 1", s (Fun ("y", Binop (Plus, Var "y", Num 1)))
                             = Fun ("y", Binop (Plus, Var "y", Num 1));
    (*16*) "Fun y -> y + x", (* subst "x" (Var "y")
                                (Fun ("y", Binop (Plus, Var "y", Var "x")))*)
                             a
                             = Fun ("var0", Binop (Plus, Var "var0", Var "y"));
    (*17*) "Let x x+1 x+4", s (Let ("x", Binop (Plus, Var "x", Num 1),
                                         Binop (Plus, Var "x", Num 4)))
                            = Let ("x", Binop (Plus, Num 2, Num 1),
                                   Binop (Plus, Var "x", Num 4));
    (*18*) "Let y x+1 x+y", s (Let ("y", Binop (Plus, Var "x", Num 1),
                                         Binop (Plus, Var "x", Var "y")))
                            = Let ("y", Binop (Plus, Num 2, Num 1),
                                        Binop (Plus, Num 2, Var "y"));

    (*19*) "Let y x+1 x+y", (* subst "x" (Var "y")
                               (Let ("y", Binop (Plus, Var "x", Num 1),
                                          Binop (Plus, Var "x", Var "y"))) *)
                            b
                            = Let ("var1", Binop (Plus, Var "y", Num 1),
                                           Binop (Plus, Var "y", Var "var1"));
  ] in
let test lst str =
  print_endline ("\nTESTING: " ^ str);
  let rec test' counter l =
    match l with
    | [] -> print_endline ("FINISHED: " ^ str)
    | (name, boolean) :: tl ->
        print_int counter;
        print_char ' ';
        unit_test boolean name;
        test' (counter + 1) tl in
  test' 1 lst in
test tests_subst_wellformed "subst" ;;

(* TESTING EVAL_S: *)
let test_eval f tests results str =
  print_endline ("\nTESTING: " ^ str);
  let rec test' counter ts ress =
    match ts, ress with
    | [], [] -> print_endline ("FINISHED: " ^ str)
    | [], _
    | _, [] ->
        raise (Invalid_argument ("test: unequal test-list lengths for " ^ str))
    | (name, arg) :: t1, res :: t2 ->
        print_int counter;
        print_char ' ';
        let open Evaluation.Env in
        (if res = String "ManualPass" then unit_test true (name ^ ": ")
        else unit_test ((f arg (empty ())) = Val res) (name ^ ": "));
        test' (counter + 1) t1 t2 in
  test' 1 tests results ;;

let tests_wellformed =
  [
    (*1*)  "Num 1", Num 1;
    (*2*)  "Bool true", Bool true;
    (*3*)  "Negate 1", Unop (Negate, Num 1);
    (*4*)  "Negate 0", Unop (Negate, Num 0);
    (*5*)  "Negate ~-1", Unop (Negate, Unop (Negate, Num 1));
    (*6*)  "Plus 1 1", Binop (Plus, Num 1, Num 1);
    (*7*)  "Minus 1 3", Binop (Minus, Num 1, Num 3);
    (*8*)  "Times ~-3 4", Binop (Times, Unop (Negate, Num 3), Num 4);
    (*9*)  "Equals t f", Binop (Equals, Bool true, Bool false);
    (*10*) "LessThan 4 ~-0", Binop (LessThan, Num 4, Unop (Negate, Num 0));
    (*11*) "Cond t t f", Conditional (Bool true, Bool true, Bool false);
    (*12*) "Cond f t f", Conditional (Bool false, Bool true, Bool false);
    (*13*) "Fun x -> x + 1", Fun ("x", Binop (Plus, Var "x", Num 1));
    (*14*) "let x=5 in x", Let ("x", Num 5, Var "x");
    (*15*) "let x=5 in let x=1 in x",
            Let ("x", Num 5, Let ("x", Num 1, Var "x"));
    (*16*) "let s=(fun x -> x + 1) in s (s 1)",
            Let ("s", Fun ("x", Binop (Plus, Var "x", Num 1)),
                 App (Var "s", App (Var "s", Num 1)));
    (*17*) "let s=(fun s -> s + 1) in s (s 1)",
            Let ("s", Fun ("s", Binop (Plus, Var "s", Num 1)),
                 App (Var "s", App (Var "s", Num 1)));
    (*18*) "let succ = fun x -> x + 1 in \
            let square = fun x -> x * x in \
            let y = 3 in \
            succ (square y)",
            Let ("succ", Fun ("x", Binop (Plus, Var "x", Num 1)),
            Let ("square", Fun ("x", Binop (Times, Var "x", Var "x")),
            Let ("y", Num 3,
            App (Var "succ", App (Var "square", Var "y")))));
    (*19*) "let x = 3 in \
            let f = fun x -> x + 1 in \
            let y = fun f -> f in \
            let z = fun z -> (f z) + (y z) in \
            z x",
            Let ("x", Num 3,
            Let ("f", Fun ("x", Binop (Plus, Var "x", Num 1)),
            Let ("y", Fun ("f", Var "f"),
            Let ("z", Fun ("z", Binop (Plus, App (Var "f", Var "z"),
                                             App (Var "y", Var "z"))),
            App (Var "z", Var "x")))));
    (*20*) "let x = 2 in \
            let f = fun y -> x + y in \
            let x = 0 in \
            f x",
            Let ("x", Num 2,
            Let ("f", Fun ("y", Binop (Plus, Var "x", Var "y")),
            Let ("x", Num 0,
            App (Var "f", Var "x"))));
    (*21*) "let rec f = fun x -> if x = 0 then true else f (x - 1) in \
            let x = 3 in \
            f x",
            Letrec ("f",
                    Fun ("x",
                         Conditional (Binop (Equals, Var "x", Num 0),
                                      Bool true,
                                      App (Var "f",
                                      Binop (Minus,
                                             Var "x",
                                             Num 1)))),
            Let ("x", Num 3,
            App (Var "f", Var "x")));
    (*22*) "let y = true in \
            let rec f = \
              let g = fun x -> if x = 0 then y else f (x - 1) in \
              let y = false in \
              g in \
            let x = 3 in \
            let y = false in \
            f x",
            Let ("y", Bool true, 
            Letrec ("f",
                    Fun ("x",
                         Let ("g", 
                              Conditional (Binop (Equals, Var "x", Num 0),
                                           Var "y",
                                           App (Var "f",
                                           Binop (Minus,
                                                  Var "x",
                                                  Num 1))),
                         Let ("y", Bool false,
                         Var "g"))),
            Let ("x", Num 3,
            Let ("y", Bool false,
            App (Var "f", Var "x")))));

    (* Testing: Unit, Float, Char, String *)
    (*23*) "Unit", Unit;
    (*24*) "Float 1.2", Float 1.2;
    (*25*) "Float ~-.1.2", Float ~-.1.2;
    (*26*) "Char a", Char 'a';
    (*27*) "String asdf", String "asdf";
    (*28*) "1. +. 1.", Binop (FPlus, Float 1., Float 1.);
    (*29*) "Negate ~-.1.", Unop (FNegate, Float ~-.1.);
    (*30*) "as ^ df", Binop (Concat, String "as", String "df");
  ] ;;

let results_eval_s =
  [
    (*1*)  Num 1;
    (*2*)  Bool true;
    (*3*)  Num ~-1;
    (*4*)  Num 0;
    (*5*)  Num 1;
    (*6*)  Num 2;
    (*7*)  Num ~-2;
    (*8*)  Num ~-12;
    (*9*)  Bool false;
    (*10*) Bool false;
    (*11*) Bool true;
    (*12*) Bool false;
    (*13*) Fun ("x", Binop (Plus, Var "x", Num 1));
    (*14*) Num 5;
    (*15*) Num 1;
    (*16*) Num 3;
    (*17*) Num 3;
    (*18*) Num 10;
    (*19*) Num 7;
    (*20*) Num 2;
    (*21*) Bool true;
    (*22*) Bool true;

    (*23*) Unit;
    (*24*) Float 1.2;
    (*25*) Float ~-.1.2;
    (*26*) Char 'a';
    (*27*) String "asdf";
    (*28*) Float 2.;
    (*29*) Float 1.;
    (*30*) String "asdf";
  ] in
test_eval Evaluation.eval_s tests_wellformed results_eval_s "eval_s" ;;

(* TESTING: EVAL_D *)
let results_eval_d =
  [
    (*1*)  Num 1;
    (*2*)  Bool true;
    (*3*)  Num ~-1;
    (*4*)  Num 0;
    (*5*)  Num 1;
    (*6*)  Num 2;
    (*7*)  Num ~-2;
    (*8*)  Num ~-12;
    (*9*)  Bool false;
    (*10*) Bool false;
    (*11*) Bool true;
    (*12*) Bool false;
    (*13*) Fun ("x", Binop (Plus, Var "x", Num 1));
    (*14*) Num 5;
    (*15*) Num 1;
    (*16*) Num 3;
    (*17*) Num 3;
    (*18*) Num 10;
    (*19*) Num 7;
    (*20*) Num 0; (* Differs here from eval_s and eval_l *)
    (*21*) Bool true;
    (*22*) Bool false;

    (*23*) Unit;
    (*24*) Float 1.2;
    (*25*) Float ~-.1.2;
    (*26*) Char 'a';
    (*27*) String "asdf";
    (*28*) Float 2.;
    (*29*) Float 1.;
    (*30*) String "asdf";
  ] in
test_eval Evaluation.eval_d tests_wellformed results_eval_d "eval_d" ;;

let results_eval_l =
  [
    (*1*)  Num 1;
    (*2*)  Bool true;
    (*3*)  Num ~-1;
    (*4*)  Num 0;
    (*5*)  Num 1;
    (*6*)  Num 2;
    (*7*)  Num ~-2;
    (*8*)  Num ~-12;
    (*9*)  Bool false;
    (*10*) Bool false;
    (*11*) Bool true;
    (*12*) Bool false;
    (*13*) String "ManualPass";
    (*14*) Num 5;
    (*15*) Num 1;
    (*16*) Num 3;
    (*17*) Num 3;
    (*18*) Num 10;
    (*19*) Num 7;
    (*20*) Num 2; (* differs here from eval_d *)
    (*21*) Bool true;
    (*22*) Bool true;

    (*23*) Unit;
    (*24*) Float 1.2;
    (*25*) Float ~-.1.2;
    (*26*) Char 'a';
    (*27*) String "asdf";
    (*28*) Float 2.;
    (*29*) Float 1.;
    (*30*) String "asdf";
  ] in
test_eval Evaluation.eval_l tests_wellformed results_eval_l "eval_l" ;;