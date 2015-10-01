(********************************************************************************
 *
 * Low-level API for IKOS
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

(* IKOS cfg *)
type cfg_var = string
type cfg_block_name = string

type cfg_z_term =
    |C_ZConst of int
    |C_ZTerm of int * cfg_var

type cfg_q_term =
    |C_QConst of int * int
    |C_QTerm of int * int * cfg_var

type cfg_z_linear_expression = cfg_z_term list
type cfg_q_linear_expression = cfg_q_term list

type cfg_comp_op = C_InfEq | C_Eq | C_NotEq
type cfg_z_linear_constraint = cfg_z_linear_expression * cfg_comp_op
type cfg_q_linear_constraint = cfg_q_linear_expression * cfg_comp_op

type cfg_statement =
    |C_ZAdd of cfg_var * cfg_var * cfg_var
    |C_ZSub of cfg_var * cfg_var * cfg_var
    |C_ZMul of cfg_var * cfg_var * cfg_var
    |C_ZDiv of cfg_var * cfg_var * cfg_var
    |C_QAdd of cfg_var * cfg_var * cfg_var
    |C_QSub of cfg_var * cfg_var * cfg_var
    |C_QMul of cfg_var * cfg_var * cfg_var
    |C_QDiv of cfg_var * cfg_var * cfg_var
    |C_ZAssign of cfg_var * cfg_z_linear_expression
    |C_QAssign of cfg_var * cfg_q_linear_expression
    |C_ZAssert of cfg_z_linear_constraint
    |C_QAssert of cfg_q_linear_constraint
    |C_Checkpoint of string

type cfg_block = {
    name : cfg_block_name;
    statements : cfg_statement list;
}

type cfg = {
    entry : cfg_block_name;
    blocks : cfg_block list;
    links : (cfg_block_name * cfg_block_name) list
}

val string_of_z_linear_expression : cfg_z_linear_expression -> string
val string_of_q_linear_expression : cfg_q_linear_expression -> string
val string_of_z_linear_constraint : cfg_z_linear_constraint -> string
val string_of_q_linear_constraint : cfg_q_linear_constraint -> string
val string_of_cfg : cfg -> string

val print_z_linear_expression : cfg_z_linear_expression -> unit
val print_q_linear_expression : cfg_q_linear_expression -> unit
val print_z_linear_constraint : cfg_z_linear_constraint -> unit
val print_q_linear_constraint : cfg_q_linear_constraint -> unit
val print_cfg : cfg -> unit

(* IKOS call *)
type abstract_domain =
    |ConstantDomain
    |IntervalDomain
    |OctagonDomain

type constraint_op =
    |O_Inf
    |O_InfEq
    |O_Sup
    |O_SupEq
    |O_Eq
    |O_NotEq
    |O_Mod of int

type z_linear_constraint = cfg_z_linear_expression * constraint_op
type q_linear_constraint = cfg_q_linear_expression * constraint_op

type z_linear_constraints = z_linear_constraint list
type q_linear_constraints = q_linear_constraint list
type linear_constraints = z_linear_constraints * q_linear_constraints

type fixpoint = (string, linear_constraints) Hashtbl.t

exception IkosException of string

val reduce_q : int * int -> int * int
val string_of_q : int * int -> string

val constant_of_z_linear_expression : cfg_z_linear_expression -> int
val constant_of_q_linear_expression : cfg_q_linear_expression -> int * int
val z_predicate_of_constraint_op : constraint_op -> int -> bool
val q_predicate_of_constraint_op : constraint_op -> int * int -> bool

val z_constraint_is_tautology : z_linear_constraint -> bool
val q_constraint_is_tautology : q_linear_constraint -> bool
val z_constraint_is_contradiction : z_linear_constraint -> bool
val q_constraint_is_contradiction : q_linear_constraint -> bool

val string_of_constraint_op : constraint_op -> string
val string_of_z_linear_constraint : z_linear_constraint -> string
val string_of_q_linear_constraint : q_linear_constraint -> string
val string_of_z_linear_constraints : z_linear_constraints -> string
val string_of_q_linear_constraints : q_linear_constraints -> string

val print_constraint_op : constraint_op -> unit
val print_z_linear_constraint : z_linear_constraint -> unit
val print_q_linear_constraint : q_linear_constraint -> unit
val print_z_linear_constraints : z_linear_constraints -> unit
val print_q_linear_constraints : q_linear_constraints -> unit

(* compute_fixpoint `domain for integers` `domain for rational numbers` cfg *)
val compute_fixpoint : abstract_domain -> abstract_domain -> cfg -> fixpoint
