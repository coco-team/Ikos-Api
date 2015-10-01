(********************************************************************************
 *
 * Author: Maxime Arthaud (maxime@arthaud.me)
 *
 * Copyright (c) 2014 Carnegie Mellon University
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 *******************************************************************************)

open Printf
open Ast
open Ast_types

let error_count = ref 0;;
let test_count = ref 0;;

(* colors *)
let all_off = "\x1b[1;0m";;
let bold = "\x1b[1;1m";;
let blue = "\x1b[1;34m";;
let green = "\x1b[1;32m";;
let red = "\x1b[1;31m";;
let yellow = "\x1b[1;33m";;

let check description test =
    incr test_count;
    printf "  %s ... " description;
    if test then
        printf "%sOK%s\n" bold all_off
    else (
        printf "%s%sERROR%s\n" bold red all_off;
        incr error_count;
    );;

let check_eq description a b = check description (a = b);;

let tests_start () =
    printf "%sStarting test suite..%s\n" bold all_off;;

let tests_end () =
    printf "\n%sResults :%s\n  " bold all_off;
    if !error_count > 0 then
        printf "%s%s%d test(s) failed.%s\n" bold red !error_count all_off
    else
        printf "%s%s%d tests passed successfully.%s\n" bold green !test_count all_off;
    exit (!error_count)
;;

tests_start ();;

check_eq "string_of_type 1" (string_of_type ZType) "int";;
check_eq "string_of_type 2" (string_of_type QType) "rat";;

check_eq "string_of_expression 1" (string_of_expression (Var "x")) "x";;
check_eq "string_of_expression 2" (string_of_expression (Const(ZConst 42))) "42";;
check_eq "string_of_expression 3" (string_of_expression (Const(QConst(1, 2)))) "1/2";;
check_eq "string_of_expression 4" (string_of_expression (Add(Var("x"), Const(ZConst 1)))) "(x) + (1)";;
check_eq "string_of_expression 5" (string_of_expression (Sub(Var("x"), Const(ZConst 1)))) "(x) - (1)";;
check_eq "string_of_expression 6" (string_of_expression (Mul(Var("x"), Const(ZConst 1)))) "(x) * (1)";;
check_eq "string_of_expression 7" (string_of_expression (Div(Var("x"), Const(ZConst 1)))) "(x) / (1)";;
check_eq "string_of_expression 8" (string_of_expression (Minus(Var "x"))) "-(x)";;
check_eq "string_of_expression 9" (string_of_expression (Add (Const (ZConst 1), Mul (Const (ZConst 2), Var "x")))) "(1) + ((2) * (x))";;
check_eq "string_of_expression 10" (string_of_expression (Sub (Sub (Const (ZConst 1), Mul (Const (ZConst 2), Minus (Var "x"))),
   Var "y"))) "((1) - ((2) * (-(x)))) - (y)";;

check_eq "string_of_condition 1" (string_of_condition (Inf(Var "x", Var "y"))) "x < y";;
check_eq "string_of_condition 2" (string_of_condition (InfEq(Var "x", Var "y"))) "x <= y";;
check_eq "string_of_condition 3" (string_of_condition (Eq(Var "x", Var "y"))) "x == y";;
check_eq "string_of_condition 4" (string_of_condition (NotEq(Var "x", Var "y"))) "x != y";;
check_eq "string_of_condition 5" (string_of_condition (Sup(Var "x", Var "y"))) "x > y";;
check_eq "string_of_condition 6" (string_of_condition (SupEq(Var "x", Var "y"))) "x >= y";;
check_eq "string_of_condition 7" (string_of_condition (And(Inf(Var "x", Var "y"), Sup(Var "x", Var "y")))) "(x < y) && (x > y)";;
check_eq "string_of_condition 8" (string_of_condition (Or(Inf(Var "x", Var "y"), Sup(Var "x", Var "y")))) "(x < y) || (x > y)";;
check_eq "string_of_condition 9" (string_of_condition (Not(Inf(Var "x", Var "y")))) "!(x < y)";;

check_eq "string_of_statement 1" (string_of_statement (Checkpoint("c1"))) "checkpoint(c1);";;
check_eq "string_of_statement 2" (string_of_statement (Define(ZType, "x", None))) "int x;";;
check_eq "string_of_statement 3" (string_of_statement (Define(ZType, "x", Some(Const(ZConst 1))))) "int x = 1;";;
check_eq "string_of_statement 4" (string_of_statement (Define(QType, "x", None))) "rat x;";;
check_eq "string_of_statement 5" (string_of_statement (Define(QType, "x", Some(Const(ZConst 1))))) "rat x = 1;";;
check_eq "string_of_statement 6" (string_of_statement (Assign("x", Const(ZConst 1)))) "x = 1;";;
check_eq "string_of_statement 7" (string_of_statement (Assert(Inf(Var "x", Var "y")))) "assert(x < y);";;
check_eq "string_of_statement 8" (string_of_statement (If(Inf(Var "x", Var "y"), [Assign("x", Const(ZConst 1))], None))) "if (x < y) {\n\tx = 1;\n}";;
check_eq "string_of_statement 9" (string_of_statement (If(Inf(Var "x", Var "y"), [Assign("x", Const(ZConst 1))], Some [Assign("x", Const(ZConst 2))]))) "if (x < y) {\n\tx = 1;\n} else {\n\tx = 2;\n}";;
check_eq "string_of_statement 10" (string_of_statement (While(Inf(Var "x", Var "y"), [Assign("x", Const(ZConst 1))]))) "while (x < y) {\n\tx = 1;\n}";;
check_eq "string_of_statement 11" (string_of_statement (For(Assign("x", Const(ZConst 0)), Inf(Var "x", Var "y"), Assign("x", Add(Var "x", Const(ZConst 1))), [Assign("z", Const(ZConst 1))]))) "for (x = 0; , x < y , x = (x) + (1);) {\n\tz = 1;\n}";;
check_eq "string_of_statement with nested blocs" (string_of_statement (If (Inf (Var "x", Const (ZConst 0)), [If (Sup (Var "y", Const (ZConst 0)), [Assign ("z", Const (ZConst 42))], None)], None))) "if (x < 0) {\n\tif (y > 0) {\n\t\tz = 42;\n\t}\n}";;

check_eq "string_of_program 1" (string_of_program [Checkpoint "c1"]) "checkpoint(c1);";;
check_eq "string_of_program 2" (string_of_program [Checkpoint "c1"; Assign("x", Const(ZConst 1))]) "checkpoint(c1);\nx = 1;";;

check_eq "program_of_string : expression 1" (program_of_string "x = 1;") [Assign("x", Const(ZConst 1))];;
check_eq "program_of_string : expression 2" (program_of_string "x = 1/2;") [Assign("x", Const(QConst(1,2)))];;
check_eq "program_of_string : expression 3" (program_of_string "x = x + 1;") [Assign("x", Add(Var "x", Const(ZConst(1))))];;
check_eq "program_of_string : expression 4" (program_of_string "x = x - 1;") [Assign("x", Sub(Var "x", Const(ZConst(1))))];;
check_eq "program_of_string : expression 5" (program_of_string "x = x * 1;") [Assign("x", Mul(Var "x", Const(ZConst(1))))];;
check_eq "program_of_string : expression 6" (program_of_string "x = x / 1;") [Assign("x", Div(Var "x", Const(ZConst(1))))];;
check_eq "program_of_string : expression 7" (program_of_string "x = -x;") [Assign("x", Minus(Var "x"))];;
check_eq "program_of_string : expression associativity 1" (program_of_string "x = x + y + z;")
[Assign("x", Add(Add(Var "x", Var "y"), Var "z"))];;
check_eq "program_of_string : expression associativity 2" (program_of_string "x = x - y - z;")
[Assign("x", Sub(Sub(Var "x", Var "y"), Var "z"))];;
check_eq "program_of_string : expression associativity 3" (program_of_string "x = x / y * z;")
[Assign("x", Mul(Div(Var "x", Var "y"), Var "z"))];;
check_eq "program_of_string : expression with operator priority 1" (program_of_string "x = x + y * z;")
[Assign("x", Add(Var "x", Mul(Var "y", Var "z")))];;
check_eq "program_of_string : expression with operator priority 2" (program_of_string "x = x + y / z;")
[Assign("x", Add(Var "x", Div(Var "y", Var "z")))];;
check_eq "program_of_string : expression with operator priority 3" (program_of_string "x = x - y * z;")
[Assign("x", Sub(Var "x", Mul(Var "y", Var "z")))];;
check_eq "program_of_string : expression with unary operator 1" (program_of_string "x = -x * y;")
[Assign("x", Mul(Minus(Var "x"), Var "y"))];;
check_eq "program_of_string : expression with unary operator 2" (program_of_string "x = x + -y / z;")
[Assign("x", Add(Var "x", Div(Minus(Var "y"), Var "z")))];;
check_eq "program_of_string : complexe expression 1" (program_of_string "x = a * (b - c - d) - e / f * (g - h);")
[Assign ("x",
  Sub (Mul (Var "a", Sub (Sub (Var "b", Var "c"), Var "d")),
     Mul (Div (Var "e", Var "f"), Sub (Var "g", Var "h"))))];;
check_eq "program_of_string : complexe expression 2" (program_of_string "x = a - -b - (c + d) / e;")
[Assign ("x",
  Sub (Sub (Var "a", Minus (Var "b")), Div (Add (Var "c", Var "d"), Var "e")))];;
check_eq "program_of_string : complexe expression 3" (program_of_string "x = a * (b + (c / d) - e * (f - g));")
[Assign ("x",
  Mul (Var "a",
     Sub (Add (Var "b", Div (Var "c", Var "d")),
         Mul (Var "e", Sub (Var "f", Var "g")))))];;

check_eq "program_of_string : condition 1" (program_of_string "if(x < y) {}") [If(Inf(Var "x", Var "y"), [], None)];;
check_eq "program_of_string : condition 2" (program_of_string "if(x <= y) {}") [If(InfEq(Var "x", Var "y"), [], None)];;
check_eq "program_of_string : condition 3" (program_of_string "if(x == y) {}") [If(Eq(Var "x", Var "y"), [], None)];;
check_eq "program_of_string : condition 4" (program_of_string "if(x != y) {}") [If(NotEq(Var "x", Var "y"), [], None)];;
check_eq "program_of_string : condition 5" (program_of_string "if(x > y) {}") [If(Sup(Var "x", Var "y"), [], None)];;
check_eq "program_of_string : condition 6" (program_of_string "if(x >= y) {}") [If(SupEq(Var "x", Var "y"), [], None)];;
check_eq "program_of_string : condition 7" (program_of_string "if(x < y && y < z) {}") [If(And(Inf(Var "x", Var "y"), Inf(Var "y", Var "z")), [], None)];;
check_eq "program_of_string : condition 8" (program_of_string "if(x < y || y < z) {}") [If(Or(Inf(Var "x", Var "y"), Inf(Var "y", Var "z")), [], None)];;
check_eq "program_of_string : condition 9" (program_of_string "if(!x < y) {}") [If(Not(Inf(Var "x", Var "y")), [], None)];;
check_eq "program_of_string : condition associativity 1" (program_of_string "if(a < b && a < c && a < d) {}") [If(And(And(Inf(Var "a", Var "b"), Inf(Var "a", Var "c")), Inf(Var "a", Var "d")), [], None)];;
check_eq "program_of_string : condition associativity 2" (program_of_string "if(a < b || a < c || a < d) {}") [If(Or(Or(Inf(Var "a", Var "b"), Inf(Var "a", Var "c")), Inf(Var "a", Var "d")), [], None)];;
check_eq "program_of_string : condition with operator priority 1" (program_of_string "if(a < b || a < c && a < d) {}") [If(Or(Inf(Var "a", Var "b"), And(Inf(Var "a", Var "c"), Inf(Var "a", Var "d"))), [], None)];;
check_eq "program_of_string : condition with operator priority 2" (program_of_string "if(a < b && !a < c || a < d && a < e) {}") [If(Or(And(Inf(Var "a", Var "b"), Not(Inf(Var "a", Var "c"))), And(Inf(Var "a", Var "d"), Inf(Var "a", Var "e"))), [], None)];;

check_eq "program_of_string : statement 1" (program_of_string "checkpoint(c1);") [Checkpoint "c1"];;
check_eq "program_of_string : statement 2" (program_of_string "int x;") [Define(ZType, "x", None)];;
check_eq "program_of_string : statement 3" (program_of_string "int x = 1;") [Define(ZType, "x", Some(Const(ZConst 1)))];;
check_eq "program_of_string : statement 4" (program_of_string "rat x = 1/2;") [Define(QType, "x", Some(Const(QConst(1,2))))];;
check_eq "program_of_string : statement 5" (program_of_string "x = 2;") [Assign("x", Const(ZConst 2))];;
check_eq "program_of_string : statement 6" (program_of_string "assert(x < y);") [Assert(Inf(Var "x", Var "y"))];;
check_eq "program_of_string : statement 7" (program_of_string "if(x < y) { x = 2; }") [If(Inf(Var "x", Var "y"), [Assign("x", Const(ZConst 2))], None)];;
check_eq "program_of_string : statement 8" (program_of_string "while(x < y) { x = 2; }") [While(Inf(Var "x", Var "y"), [Assign("x", Const(ZConst 2))])];;
check_eq "program_of_string : statement 9" (program_of_string "for(x = 1; , x < y , x = x + 1; ) { x = 2; }") [For(Assign("x", Const(ZConst 1)), Inf(Var "x", Var "y"), Assign("x", Add(Var "x", Const(ZConst 1))), [Assign("x", Const(ZConst 2))])];;

check_eq "program_of_string : complexe 1" (program_of_string "
int x = 0;
rat y = 0/1;
while(x < 10) {
  y = y + 1 / y;
  x = x + 1;
}")
[Define (ZType, "x", Some (Const (ZConst 0)));
 Define (QType, "y", Some (Const (QConst (0, 1))));
  While (Inf (Var "x", Const (ZConst 10)),
    [Assign ("y", Add (Var "y", Div (Const (ZConst 1), Var "y")));
       Assign ("x", Add (Var "x", Const (ZConst 1)))])];;

check_eq "program_of_string : complexe 2" (program_of_string "
int x = 42;
while(x >= 0) {
  x = 2 * x - 1;
  if(x < 21) {
    y = y + 1;
  } else {
    y = y - 1;
  }
  checkpoint(val);
}")
[Define (ZType, "x", Some (Const (ZConst 42)));
 While (SupEq (Var "x", Const (ZConst 0)),
   [Assign ("x", Sub (Mul (Const (ZConst 2), Var "x"), Const (ZConst 1)));
      If (Inf (Var "x", Const (ZConst 21)),
          [Assign ("y", Add (Var "y", Const (ZConst 1)))],
              Some [Assign ("y", Sub (Var "y", Const (ZConst 1)))]);
                 Checkpoint "val"])];;

tests_end ();;
