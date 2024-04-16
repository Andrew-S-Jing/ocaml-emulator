(* Final Project testing *)
open CS51Utils.Absbook ;;
let test f tests str =
  print_endline ("\nTESTING: " ^ str);
  let rec test' t =
    match t with
    | [] -> print_endline ("finished: " ^ str)
    | (name, arg, res) :: tl ->
        unit_test ((f arg) = res) (name ^ ": ");
        test' tl in
  test' tests;
  print_endline ("FINISHED: " ^ str)

(* TESTING: module Expr *)
open Expr ;;
(* test exp_to_concrete_string *)
let f = exp_to_concrete_string in
let tests =
  [
    "Var", Var "x", "x";
    "Num", Num 0, "0";
    "Bool", Bool true, "true";
    "Negate 0", Unop (Negate, Num 0), "0";
    "Negate +", Unop (Negate, Num 1), "~-1";
    "Negate -", Unop (Negate, Num ~-1), "1";
    "Negate x", Unop (Negate, Var "x"), "~-x";
    "Plus 1 1", Binop (Plus, Num 1, Num 1), "(1 + 1)";
    "Plus ~-4 x", Binop (Plus, Num ~-4, Var "x"), "(~-4 + x)";
    "Minus 1 1", Binop (Minus, Num 1, Num 1), "(1 - 1)";
    "Minus x ~-1", Binop (Minus, Var "x", Num ~-1), "(x - ~-1)";
    "Times 2 ~-2", Binop (Times, Num 2, Num ~-2), "(2 * ~-2)";
    "Times ~-1 x", Binop (Times, Num ~-1, Var "x"), "(~-1 * x)";
    "Equals t f", Binop (Equals, Bool true, Bool false), "(true = false)";
    "Equals x 1", Binop (Equals, Var "x", Num 1), "(x = 1)";
    "Equals 1 2", Binop (Equals, Num 1, Num 2), "(1 = 2)";
    "Equals x y", Binop (Equals, Var "x", Var "y"), "(x = y)";
    "LessThan 0 1", Binop (LessThan, Num 0, Num 1), "(0 < 1)";
    "LessThan x 5", Binop (LessThan, Var "x", Num 5), "(x < 5)";
    "Cond t t f", Conditional (Bool true, Bool true, Bool false),
      "if true then\n  true\nelse\n  false";
    "Cond f x y", Conditional (Bool false, Var "x", Var "y"),
      "if false then\n  x\nelse\n  y";
    "Fun x -> 1", Fun ("x", Num 1), "(fun x ->\n  1)";
    "Fun y -> y = true", Fun ("y", Binop (Equals, Var "y", Bool true)),
      "(fun y ->\n  (y = true))";
    "Fun z -> if z = false then 0 else 1",
      Fun ("z", Conditional (Binop (Equals, Var "z", Bool false),
                             Num 0, Num 1)),
      "(fun z ->\n  if (z = false) then\n    0\n  else\n    1)";
    "Let x 1 (x + 1)", Let ("x", Num 1, Binop (Plus, Var "x", Num 1)),
      "let x =\n  1 in\n(x + 1)";
    "Letrec f (fun x -> if x = 0 then true else f (x - 1)) in f 5",
      Letrec ("f",
              Fun ("x", Conditional (Binop (Equals, Var "x", Num 0),
                                     Bool true,
                                     App (Var "f",
                                          Binop (Minus, Var "x", Num 1)))),
              App (Var "f", Num 5)),
      "let rec f =\n  (fun x ->\n    if (x = 0) then\n"
        ^ "      true\n    else\n      (f (x - 1))) in\n(f 5)";
    "Letrec f (fun f -> if f = 0 then true else f (f - 1)) in f 5",
      Letrec ("f",
              Fun ("f", Conditional (Binop (Equals, Var "f", Num 0),
                                     Bool true,
                                     App (Var "f",
                                          Binop (Minus, Var "f", Num 1)))),
              App (Var "f", Num 5)),
      "let rec f =\n  (fun f ->\n    if (f = 0) then\n"
        ^ "      true\n    else\n      (f (f - 1))) in\n(f 5)";
    "Raise", Raise, "(raise EvalException)";
    "Unassigned", Unassigned, "";
    "App (fun x -> x + 1) 2",
      App (Fun ("x", Binop (Plus, Var "x", Num 1)), Num 2),
      "((fun x ->\n  (x + 1)) 2)"
  ] in
test f tests "exp_to_concrete_string" ;;