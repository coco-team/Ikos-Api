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

open Ikos
open Ast_types
open Ast
open Printf

let prg =
"int x = 10;
int y = 0;
while(x >= 0) {
    x = x - 1;
    y = 2 * y + 1;
    checkpoint(loop);
}
int z = y + 1;
";;

printf "======== C program ========\n%s" prg;;
print_newline ();;

check_types (program_of_string prg);;

let cfg = program_to_cfg (program_of_string prg);;

printf "======== CFG ========";;
print_newline ();;
print_cfg cfg;;

printf "======== Fixpoint ========";;
print_newline ();;
let checkpoints = compute_fixpoint IntervalDomain IntervalDomain cfg in
let z_constraints, q_constraints = Hashtbl.find checkpoints "loop" in
print_string "checkpoint loop : ";
print_z_linear_constraints z_constraints;
print_q_linear_constraints q_constraints;
print_newline ()
