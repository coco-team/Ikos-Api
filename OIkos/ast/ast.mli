(********************************************************************************
 *
 * High-level API for IKOS
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

open Ast_types
open Ikos

(* print functions *)
val string_of_type : var_type -> string
val string_of_expression : expression -> string
val string_of_condition : condition -> string
val string_of_statement : statement -> string
val string_of_program : program -> string

val print_type : var_type -> unit
val print_expression : expression -> unit
val print_condition : condition -> unit
val print_statement : statement -> unit
val print_program : program -> unit

(* conversion from string to Ast *)
val program_of_lexbuf : Lexing.lexbuf -> program
val program_of_string : string -> program

(* check functions *)
type environment = (string * var_type) list
exception Bad_type of string

(* throws a Not_found exception when the variable is not in the environment *)
val type_of_var : environment -> string -> var_type

val type_of_constant : constant -> var_type

(* throws a Bad_type exception when the expression is uncorrectly typed *)
val type_of_expression : environment -> expression -> var_type

(* throws a Bad_type exception when the program is uncorrectly typed *)
val check_types : program -> unit

(* conversion from AST to CFG *)
val program_to_cfg : program -> cfg

val compute_fixpoint_program : abstract_domain -> abstract_domain -> program -> fixpoint
