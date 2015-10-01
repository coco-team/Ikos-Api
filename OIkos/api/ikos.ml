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

external string_of_z_linear_expression : cfg_z_linear_expression -> string = "string_of_z_linear_expression"
external string_of_q_linear_expression : cfg_q_linear_expression -> string = "string_of_q_linear_expression"
external string_of_z_linear_constraint : cfg_z_linear_constraint -> string = "string_of_z_linear_constraint"
external string_of_q_linear_constraint : cfg_q_linear_constraint -> string = "string_of_q_linear_constraint"
external string_of_cfg : cfg -> string = "string_of_cfg"

let print_z_linear_expression e = print_string (string_of_z_linear_expression e)
let print_q_linear_expression e = print_string (string_of_q_linear_expression e)
let print_z_linear_constraint c = print_string (string_of_z_linear_constraint c)
let print_q_linear_constraint c = print_string (string_of_q_linear_constraint c)
let print_cfg cfg = print_string (string_of_cfg cfg)

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

(* gcd : int -> int -> int *)
let rec gcd n m =
    if m = 0 then n
    else if n > m then gcd (n-m) m
    else gcd n (m-n)

(* reduce_q : int * int -> int * int *)
let reduce_q (n, d) =
    if n = 0 then (0, 1)
    else
        let n, d = n * (if d < 0 then -1 else 1), abs(d) in
        let cd = gcd (abs n) d in
        n / cd, d / cd

(* string_of_q : int * int -> string *)
let string_of_q (n, d) =
    let n, d = reduce_q (n, d) in
    if d = 1 then string_of_int n
    else Printf.sprintf "%d/%d" n d

let rec constant_of_z_linear_expression = function
    |[] -> 0
    |C_ZConst(cst)::q -> cst + (constant_of_z_linear_expression q)
    |C_ZTerm(_)::q -> constant_of_z_linear_expression q

let rec constant_of_q_linear_expression = function
    |[] -> 0, 1
    |C_QConst(n, d)::q ->
        let rn, rd = constant_of_q_linear_expression q in
        reduce_q (n * rd + rn * d, d * rd)
    |C_QTerm(_)::q -> constant_of_q_linear_expression q

let z_predicate_of_constraint_op op x = match op with
    |O_Inf -> x < 0
    |O_InfEq -> x <= 0
    |O_Sup -> x > 0
    |O_SupEq -> x >= 0
    |O_Eq -> x = 0
    |O_NotEq -> x != 0
    |O_Mod m -> x mod m = 0

let q_predicate_of_constraint_op op (n, d) = match op with
    |O_Inf -> n * d < 0
    |O_InfEq -> n * d <= 0
    |O_Sup -> n * d > 0
    |O_SupEq -> n * d >= 0
    |O_Eq -> n = 0
    |O_NotEq -> n != 0
    |_ -> failwith "using operator mod on a q_linear_expression"

let z_constraint_is_tautology (expr, op) =
    List.for_all (function |C_ZConst _ -> true |C_ZTerm _ -> false) expr
    && z_predicate_of_constraint_op op (constant_of_z_linear_expression expr)

let q_constraint_is_tautology (expr, op) =
    List.for_all (function |C_QConst _ -> true |C_QTerm _ -> false) expr
    && q_predicate_of_constraint_op op (constant_of_q_linear_expression expr)

let z_constraint_is_contradiction (expr, op) =
    List.for_all (function |C_ZConst _ -> true |C_ZTerm _ -> false) expr
    && not(z_predicate_of_constraint_op op (constant_of_z_linear_expression expr))

let q_constraint_is_contradiction (expr, op) =
    List.for_all (function |C_QConst _ -> true |C_QTerm _ -> false) expr
    && not(q_predicate_of_constraint_op op (constant_of_q_linear_expression expr))

let string_of_constraint_op = function
    |O_Inf -> "<"
    |O_InfEq -> "<="
    |O_Sup -> ">"
    |O_SupEq -> ">="
    |O_Eq -> "="
    |O_NotEq -> "!="
    |O_Mod m -> "=[" ^ string_of_int(m) ^ "]"

let string_of_z_linear_constraint (expr, op) =
    if z_constraint_is_tautology (expr, op) then "true"
    else if z_constraint_is_contradiction (expr, op) then "false"
    else
        let rec aux str = function
            |[] ->
                (
                let cst = constant_of_z_linear_expression expr in
                match op with
                |O_Mod m -> Printf.sprintf "%s = %d [%d]" str (-cst) m
                |_ -> Printf.sprintf "%s %s %d" str (string_of_constraint_op op) (-cst)
                )
            |C_ZConst(_)::q -> aux str q
            |C_ZTerm(factor, var)::q ->
                let fact = if abs(factor) != 1 then string_of_int (abs factor) else "" in
                if str = "" then
                    let sign = if factor < 0 then "-" else "" in
                    aux (sign ^ fact ^ var) q
                else
                    let sign = if factor < 0 then " - " else " + " in
                    aux (str ^ sign ^ fact ^ var) q
        in aux "" expr

let string_of_q_linear_constraint (expr, op) =
    if q_constraint_is_tautology (expr, op) then "true"
    else if q_constraint_is_contradiction (expr, op) then "false"
    else
        let rec aux str = function
            |[] ->
                let (n, d) = constant_of_q_linear_expression expr in
                let cst_str = string_of_q (-n, d) in
                Printf.sprintf "%s %s %s" str (string_of_constraint_op op) cst_str
            |C_QConst(_)::q -> aux str q
            |C_QTerm(n, d, var)::q ->
                let n, d = reduce_q (n, d) in
                let fact = if abs(n) != d then string_of_q (abs n, d) else "" in
                if str = "" then
                    let sign = if n < 0 then "-" else "" in
                    aux (sign ^ fact ^ var) q
                else
                    let sign = if n < 0 then " - " else " + " in
                    aux (str ^ sign ^ fact ^ var) q
        in aux "" expr

let string_of_z_linear_constraints csts =
    let rec aux = function
        |[] -> ""
        |cst::[] -> string_of_z_linear_constraint cst
        |cst::q -> Printf.sprintf "%s ; %s" (string_of_z_linear_constraint cst) (aux q)
    in
    if csts = [] then "{}"
    else Printf.sprintf "{ %s }" (aux csts)

let string_of_q_linear_constraints csts =
    let rec aux = function
        |[] -> ""
        |cst::[] -> string_of_q_linear_constraint cst
        |cst::q -> Printf.sprintf "%s ; %s" (string_of_q_linear_constraint cst) (aux q)
    in
    if csts = [] then "{}"
    else Printf.sprintf "{ %s }" (aux csts)

let print_constraint_op op = print_string (string_of_constraint_op op)
let print_z_linear_constraint c = print_string (string_of_z_linear_constraint c)
let print_q_linear_constraint c = print_string (string_of_q_linear_constraint c)
let print_z_linear_constraints csts = print_string (string_of_z_linear_constraints csts)
let print_q_linear_constraints csts = print_string (string_of_q_linear_constraints csts)

let _ =
    Callback.register_exception "ikos_exception" (IkosException "");
    Callback.register "hashtbl_create" (Hashtbl.create ~random:false);
    Callback.register "hashtbl_add" Hashtbl.add

external compute_fixpoint : abstract_domain -> abstract_domain -> cfg -> fixpoint = "compute_fixpoint"
