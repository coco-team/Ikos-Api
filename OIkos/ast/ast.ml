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
open Ast_parser
open Ast_lexer
open Ikos
open Str

(* print functions *)
let string_of_type = function
    |ZType -> "int"
    |QType -> "rat"

let rec string_of_expression = function
    |Var name -> name
    |Const(ZConst x) -> string_of_int x
    |Const(QConst(n, d)) -> (string_of_int n) ^ "/" ^ (string_of_int d)
    |Add(left, right) -> "(" ^ (string_of_expression left) ^ ") + (" ^ (string_of_expression right) ^ ")"
    |Sub(left, right) -> "(" ^ (string_of_expression left) ^ ") - (" ^ (string_of_expression right) ^ ")"
    |Mul(left, right) -> "(" ^ (string_of_expression left) ^ ") * (" ^ (string_of_expression right) ^ ")"
    |Div(left, right) -> "(" ^ (string_of_expression left) ^ ") / (" ^ (string_of_expression right) ^ ")"
    |Minus expr -> "-(" ^ (string_of_expression expr) ^ ")"
;;

let rec string_of_condition = function
    |Inf(left, right)   -> (string_of_expression left) ^ " < "  ^ (string_of_expression right)
    |InfEq(left, right) -> (string_of_expression left) ^ " <= "  ^ (string_of_expression right)
    |Eq(left, right)    -> (string_of_expression left) ^ " == "  ^ (string_of_expression right)
    |NotEq(left, right) -> (string_of_expression left) ^ " != "  ^ (string_of_expression right)
    |Sup(left, right)   -> (string_of_expression left) ^ " > "  ^ (string_of_expression right)
    |SupEq(left, right) -> (string_of_expression left) ^ " >= "  ^ (string_of_expression right)
    |And(left, right) -> "(" ^ (string_of_condition left) ^ ") && (" ^ (string_of_condition right) ^ ")"
    |Or(left, right)  -> "(" ^ (string_of_condition left) ^ ") || (" ^ (string_of_condition right) ^ ")"
    |Not c -> "!(" ^ (string_of_condition c) ^ ")"
;;

let rec
string_of_statement = function
    |Checkpoint name -> "checkpoint(" ^ name ^ ");"
    |Define(var_t, name, None) -> (string_of_type var_t) ^ " " ^ name ^ ";"
    |Define(var_t, name, Some expr) -> (string_of_type var_t) ^ " " ^ name ^ " = " ^ (string_of_expression expr) ^ ";"
    |Assign(name, expr) -> name ^ " = " ^ (string_of_expression expr) ^ ";"
    |Assert cond -> "assert(" ^ (string_of_condition cond) ^ ");"
    |If(cond, statements, None) ->
        "if (" ^ (string_of_condition cond) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
    |If(cond, statements_true, Some statements_false) ->
        "if (" ^ (string_of_condition cond) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements_true)) ^
        "\n} else {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements_false)) ^
        "\n}"
    |While(cond, statements) ->
        "while (" ^ (string_of_condition cond) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
    |For(init, cond, incr, statements) ->
        "for (" ^ (string_of_statement init) ^ " , " ^ (string_of_condition cond) ^ " , " ^ (string_of_statement incr) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
and
string_of_program = function
    |[] -> ""
    |statement::[] -> string_of_statement statement
    |statement::program -> (string_of_statement statement) ^ "\n" ^ (string_of_program program)
;;

let print_type vt = print_string (string_of_type vt);;
let print_expression e = print_string (string_of_expression e);;
let print_condition c = print_string (string_of_condition c);;
let print_statement s = print_string (string_of_statement s);;
let print_program p = print_string (string_of_program p);;

(* conversion from string to Ast *)
let program_of_lexbuf lexbuf = nt_program tokenize lexbuf;;
let program_of_string src = program_of_lexbuf (Lexing.from_string src);;

(* check functions *)
type environment = (string * var_type) list
exception Bad_type of string

let type_of_var env name = List.assoc name env;;

let type_of_constant = function
    |ZConst _ -> ZType
    |QConst _ -> QType

let rec type_of_expression env expr =
    let type_of_binary_expr op left right =
        let left_type = type_of_expression env left
        and right_type = type_of_expression env right
        in
            if left_type = right_type then left_type
            else raise (Bad_type ("cannot " ^ op ^ " types " ^ (string_of_type left_type) ^ " and " ^ (string_of_type right_type) ^ "."))
    in
    match expr with
        |Var name ->
                (
                try
                    type_of_var env name
                with Not_found ->
                    raise (Bad_type ("variable " ^ name ^ " is not defined."))
                )
        |Const cst -> type_of_constant cst
        |Add(left, right) -> type_of_binary_expr "add" left right
        |Sub(left, right) -> type_of_binary_expr "subtract" left right
        |Mul(left, right) -> type_of_binary_expr "multiply" left right
        |Div(left, right) -> type_of_binary_expr "divide" left right
        |Minus expr -> type_of_expression env expr

let rec check_types_of_condition env cond =
    let check_binary_expr left right =
        let left_type = type_of_expression env left
        and right_type = type_of_expression env right
        in
            if not(left_type = right_type) then
                raise (Bad_type ("cannot compare types " ^ (string_of_type left_type) ^ " and " ^ (string_of_type right_type) ^ "."))
    in
    match cond with
        |Inf(left, right) -> check_binary_expr left right
        |InfEq(left, right) -> check_binary_expr left right
        |Eq(left, right) -> check_binary_expr left right
        |NotEq(left, right) -> check_binary_expr left right
        |Sup(left, right) -> check_binary_expr left right
        |SupEq(left, right) -> check_binary_expr left right
        |And(left, right) -> check_types_of_condition env left; check_types_of_condition env right
        |Or(left, right) -> check_types_of_condition env left; check_types_of_condition env right
        |Not cond -> check_types_of_condition env cond

let check_types =
    let rec aux env = function
        |[] -> ()
        |Checkpoint(_)::q -> aux env q
        |Define(var_t, name, None)::q ->
            if List.mem_assoc name env then
                raise (Bad_type ("variable " ^ name ^ " already defined."))
            else aux ((name, var_t)::env) q
        |Define(var_t, name, Some expr)::q ->
            if List.mem_assoc name env then
                raise (Bad_type ("variable " ^ name ^ " already defined."))
            else if not(var_t = type_of_expression env expr) then
                raise (Bad_type ("expression of type " ^ (string_of_type (type_of_expression env expr)) ^ " cannot be assigned to variable " ^ name ^ " of type " ^ (string_of_type var_t) ^ "."))
            else aux ((name, var_t)::env) q
        |Assign(name, expr)::q ->
            (
            try
                let var_t = type_of_var env name in
                if not(var_t = type_of_expression env expr) then
                    raise (Bad_type ("expression of type " ^ (string_of_type (type_of_expression env expr)) ^ " cannot be assigned to variable " ^ name ^ " of type " ^ (string_of_type var_t) ^ "."))
                else
                    aux env q
            with Not_found ->
                raise (Bad_type ("variable " ^ name ^ " is not defined."))
            )
        |Assert(cond)::q ->
            check_types_of_condition env cond;
            aux env q
        |If(cond, statements_true, statements_false_option)::q ->
            check_types_of_condition env cond;
            aux env statements_true;
            (
            match statements_false_option with
                |Some statements_false -> aux env statements_false
                |None -> ()
            );
            aux env q
        |While(cond, statements)::q ->
            check_types_of_condition env cond;
            aux env statements;
            aux env q
        |For(init, cond, incr, statements)::q ->
            aux env [init; While(cond, statements @ [incr])];
            aux env q
        in
        aux [];;

(* conversion from Ast to CFG *)

(* functions working on linear expressions *)
let rec multiply_z_linear_expression factor = function
    |[] -> []
    |C_ZConst(cst)::q -> C_ZConst(factor * cst)::(multiply_z_linear_expression factor q)
    |C_ZTerm(cst, var)::q -> C_ZTerm(factor * cst, var)::(multiply_z_linear_expression factor q)

let rec multiply_q_linear_expression (fact_n, fact_d) = function
    |[] -> []
    |C_QConst(n, d)::q -> C_QConst(fact_n * n, fact_d * d)::(multiply_q_linear_expression (fact_n, fact_d) q)
    |C_QTerm(n, d, var)::q -> C_QTerm(fact_n * n, fact_d * d, var)::(multiply_q_linear_expression (fact_n, fact_d) q)

(* simplifies an expression to minimize the number of statements needed to compute it
 *
 * - removes all Sub
 * - tries to put constants at the end
*)
let simplify_expression expr =
    let rec aux = function
        |Var n -> Var n
        |Const cst -> Const cst

        (* Minus *)
        |Minus(Const(ZConst cst)) -> Const(ZConst(-cst))
        |Minus(Const(QConst(n, d))) -> Const(QConst(-n, d))
        |Minus(Minus x) -> aux x
        |Minus x -> Minus (aux x)

        (* Add *)
        |Add(Const(ZConst c1), Const(ZConst c2)) -> Const(ZConst(c1 + c2))
        |Add(Const(QConst(n1, d1)), Const(QConst(n2, d2))) -> Const(QConst(n1 * d2 + n2 * d1, d1 * d2))

        |Add(Add(Const cst1, left), Const cst2) -> Add(aux left, aux (Add(Const cst1, Const cst2)))
        |Add(Add(left, Const cst1), Const cst2) -> Add(aux left, aux (Add(Const cst1, Const cst2)))
        |Add(Add(Const cst, left), right) -> Add(aux (Add(left, right)), Const cst)
        |Add(Add(left, Const cst), right) -> Add(aux (Add(left, right)), Const cst)

        |Add(left, right) -> Add(aux left, aux right)

        (* Sub *)
        |Sub(left, right) -> aux (Add(left, Minus(right)))

        (* Mul *)
        |Mul(Const(ZConst c1), Const(ZConst c2)) -> Const(ZConst(c1 * c2))
        |Mul(Const(QConst(n1, d1)), Const(QConst(n2, d2))) -> Const(QConst(n1 * n2, d1 * d2))

        |Mul(Mul(Const cst1, left), Const cst2) -> Mul(aux left, aux (Mul(Const cst1, Const cst2)))
        |Mul(Mul(left, Const cst1), Const cst2) -> Mul(aux left, aux (Mul(Const cst1, Const cst2)))
        |Mul(Mul(Const cst, left), right) -> Mul(aux (Mul(left, right)), Const cst)
        |Mul(Mul(left, Const cst), right) -> Mul(aux (Mul(left, right)), Const cst)

        |Mul(left, right) -> Mul(aux left, aux right)

        (* Div *)
        |Div(Const(QConst(n1, d1)), Const(QConst(n2, d2))) -> Const(QConst(n1 * d2, d1 * n2))

        |Div(Div(left1, left2), right) -> Div(aux left1, aux (Mul(left2, right)))
        |Div(left, Div(right1, right2)) -> Div(aux (Mul(left, right2)), aux right1)

        |Div(left, right) -> Div(aux left, aux right)
    in
    let rec repeat_until_no_update e =
        let r = aux e in
        if r = e then r else repeat_until_no_update r
    in
    repeat_until_no_update expr
;;

(* simplifies a condition to compute the cfg
 *
 * - removes all Not, Inf, Sup, SupEq
 * - puts Const(ZConst 0) as the right operand
 *)
let rec simplify_condition = function
    (* comparison operators *)
    |Inf(left, right) -> And(InfEq(Sub(left, right), Const(ZConst 0)), NotEq(Sub(left, right), Const(ZConst 0)))
    |InfEq(left, right) -> InfEq(Sub(left, right), Const(ZConst 0))
    |Eq(left, right) -> Eq(Sub(left, right), Const(ZConst 0))
    |NotEq(left, right) -> NotEq(Sub(left, right), Const(ZConst 0))
    |Sup(left, right) -> simplify_condition (Inf(right, left))
    |SupEq(left, right) -> simplify_condition (InfEq(right, left))

    (* boolean operators *)
    |And(left, right) -> And(simplify_condition left, simplify_condition right)
    |Or(left, right) -> Or(simplify_condition left, simplify_condition right)

    (* Not *)
    |Not(Inf(left, right)) -> simplify_condition (SupEq(left, right))
    |Not(InfEq(left, right)) -> simplify_condition (Sup(left, right))
    |Not(Eq(left, right)) -> simplify_condition (NotEq(left, right))
    |Not(NotEq(left, right)) -> simplify_condition (Eq(left, right))
    |Not(Sup(left, right)) -> simplify_condition (InfEq(left, right))
    |Not(SupEq(left, right)) -> simplify_condition (Inf(left, right))
    |Not(And(left, right)) -> Or(simplify_condition (Not(left)), simplify_condition (Not(right)))
    |Not(Or(left, right)) -> And(simplify_condition (Not(left)), simplify_condition (Not(right)))
    |Not(Not x) -> simplify_condition x
;;

(* ident_generator : string -> (unit -> string)
 *
 * returns a function generating incremental identifiants *)
let ident_generator fmt =
    let counter = ref 0 in
    fun () -> incr counter; replace_first (regexp "%d") (string_of_int (!counter - 1)) fmt;;

(* expression_to_statements : environment -> cfg_var -> expression -> cfg_statement list * cfg_var
 *
 * returns a list of statements to compute the expression,
 * and the name of the variable containing the result.
 *
 * precondition : this function needs the expression to be simplified before.
 * warning : statements are in reverse order *)
let expression_to_statements env tmp_var_prefix expr =
    (* type of the expression *)
    let expr_type = type_of_expression env expr in

    (* manage temporary variables *)
    let get_tmp_var = ident_generator (tmp_var_prefix ^ "%d") in

    (* aux : expression -> cfg_statement list * cfg_var
     * returns (statements, r) with r the name of the variable containing the result *)
    let rec aux = function
        (* Var *)
        |Var name -> [], name

        (* Const *)
        |Const(ZConst cst) ->
            let tmp = get_tmp_var () in
            [C_ZAssign(tmp, [C_ZConst cst])], tmp
        |Const(QConst(n, d)) ->
            let tmp = get_tmp_var () in
            [C_QAssign(tmp, [C_QConst(n, d)])], tmp

        (* Minus *)
        |Minus e ->
            let stmts, r = aux e in
            (
            match stmts with
                |C_ZAssign(_, terms)::q -> C_ZAssign(r, multiply_z_linear_expression (-1) terms)::q, r
                |C_QAssign(_, terms)::q -> C_QAssign(r, multiply_q_linear_expression (-1, 1) terms)::q, r
                |_ ->
                    let tmp = get_tmp_var () in
                    if expr_type = ZType then C_ZAssign(tmp, [C_ZTerm(-1, r)])::stmts, tmp
                    else C_QAssign(tmp, [C_QTerm(-1, 1, r)])::stmts, tmp
            )

        (* Add *)
        |Add(left, right) ->
            let stmts_left, r1 = aux left in
            let stmts_right, r2 = aux right in
            (
            match stmts_left, stmts_right with
                (* tries to merge linear expressions *)
                |C_ZAssign(_, terms1)::q1, C_ZAssign(_, terms2)::q2 -> C_ZAssign(r1, terms1 @ terms2)::(q1 @ q2), r1
                |C_ZAssign(_, terms)::q, _ -> C_ZAssign(r1, C_ZTerm(1, r2)::terms)::(q @ stmts_right), r1
                |_, C_ZAssign(_, terms)::q -> C_ZAssign(r2, C_ZTerm(1, r1)::terms)::(q @ stmts_left), r2

                |C_QAssign(_, terms1)::q1, C_QAssign(_, terms2)::q2 -> C_QAssign(r1, terms1 @ terms2)::(q1 @ q2), r1
                |C_QAssign(_, terms)::q, _ -> C_QAssign(r1, C_QTerm(1, 1, r2)::terms)::(q @ stmts_right), r1
                |_, C_QAssign(_, terms)::q -> C_QAssign(r2, C_QTerm(1, 1, r1)::terms)::(q @ stmts_left), r2

                |_ ->
                    let tmp = get_tmp_var () in
                    if expr_type = ZType then C_ZAssign(tmp, [C_ZTerm(1, r1); C_ZTerm(1, r2)])::(stmts_left @ stmts_right), tmp
                    else C_QAssign(tmp, [C_QTerm(1, 1, r1); C_QTerm(1, 1, r2)])::(stmts_left @ stmts_right), tmp
            )

        (* Sub : always replaced by a Add *)
        |Sub(_, _) -> failwith "use simplify_expression before calling expression_to_statements"

        (* Mul *)
        |Mul(left, Const(ZConst cst)) ->
            let stmts, r = aux left in
            (
            match stmts with
                |C_ZAssign(_, terms)::q -> C_ZAssign(r, multiply_z_linear_expression cst terms)::q, r
                |_ ->
                    let tmp = get_tmp_var () in
                    C_ZAssign(tmp, [C_ZTerm(cst, r)])::stmts, tmp
            )
        |Mul(left, Const(QConst(n, d))) ->
            let stmts, r = aux left in
            (
            match stmts with
                |C_QAssign(_, terms)::q -> C_QAssign(r, multiply_q_linear_expression (n, d) terms)::q, r
                |_ ->
                    let tmp = get_tmp_var () in
                    C_QAssign(tmp, [C_QTerm(n, d, r)])::stmts, tmp
            )
        |Mul(Const(ZConst cst), right) -> aux (Mul(right, Const(ZConst cst)))
        |Mul(Const(QConst(n, d)), right) -> aux (Mul(right, Const(QConst(n, d))))
        |Mul(left, right) ->
            let stmts_left, r1 = aux left in
            let stmts_right, r2 = aux right in
            let tmp = get_tmp_var () in
            if expr_type = ZType then C_ZMul(tmp, r1, r2)::(stmts_left @ stmts_right), tmp
            else C_QMul(tmp, r1, r2)::(stmts_left @ stmts_right), tmp

        (* Div : we cannot do better *)
        |Div(left, right) ->
            let stmts_left, r1 = aux left in
            let stmts_right, r2 = aux right in
            let tmp = get_tmp_var () in
            if expr_type = ZType then C_ZDiv(tmp, r1, r2)::(stmts_left @ stmts_right), tmp
            else C_QDiv(tmp, r1, r2)::(stmts_left @ stmts_right), tmp
    in
    aux expr
;;

(* make_assign : cfg_var -> environment -> cfg_var -> expression -> statement list
 *
 * returns the list of statements needed to compute the expression and assign it to `var`,
 * in reverse order *)
let make_assign var env tmp_var_prefix expr =
    let stmts, result_var = expression_to_statements env tmp_var_prefix (simplify_expression expr) in
    match stmts with
        |[] ->
            if type_of_expression env expr = ZType then [C_ZAssign(var, [C_ZTerm(1, result_var)])]
            else [C_QAssign(var, [C_QTerm(1, 1, result_var)])]

        (* if possible, just replace the temporary var by the true one *)
        |C_ZAdd(_, a, b)::q -> C_ZAdd(var, a, b)::q
        |C_ZSub(_, a, b)::q -> C_ZSub(var, a, b)::q
        |C_ZMul(_, a, b)::q -> C_ZMul(var, a, b)::q
        |C_ZDiv(_, a, b)::q -> C_ZDiv(var, a, b)::q
        |C_QAdd(_, a, b)::q -> C_QAdd(var, a, b)::q
        |C_QSub(_, a, b)::q -> C_QSub(var, a, b)::q
        |C_QMul(_, a, b)::q -> C_QMul(var, a, b)::q
        |C_QDiv(_, a, b)::q -> C_QDiv(var, a, b)::q
        |C_ZAssign(_, terms)::q -> C_ZAssign(var, terms)::q
        |C_QAssign(_, terms)::q -> C_QAssign(var, terms)::q
        |_ -> failwith "unreachable"

(* make_assert : environment -> cfg_var -> expression -> cfg_comp_op -> statement list
 *
 * returns the list of statements needed to compute the assertion `expr op={<=,=,!=} 0` *)
let make_assert env tmp_var_prefix expr op =
    let stmts, result_var = expression_to_statements env tmp_var_prefix (simplify_expression expr) in
    List.rev (
    match stmts with
        |[] ->
            if type_of_expression env expr = ZType then [C_ZAssert([C_ZTerm(1, result_var)], op)]
            else [C_QAssert([C_QTerm(1, 1, result_var)], op)]
        |C_ZAdd(_)::q | C_ZSub(_)::q | C_ZMul(_)::q | C_ZDiv(_)::q -> C_ZAssert([C_ZTerm(1, result_var)], op)::q
        |C_QAdd(_)::q | C_QSub(_)::q | C_QMul(_)::q | C_QDiv(_)::q -> C_QAssert([C_QTerm(1, 1, result_var)], op)::q
        |C_ZAssign(_, terms)::q -> C_ZAssert(terms, op)::q
        |C_QAssign(_, terms)::q -> C_QAssert(terms, op)::q
        |_ -> failwith "unreachable"
    );;

(* condition_to_cfg : environment -> cfg_var -> condition -> cfg_block_name -> cfg_block_name
 *      -> cfg_block list * (cfg_block_name * cfg_block_name) list * cfg_statement list
 *
 * returns the tuple (blocks, links, stmts) needed to build the condition `cond`
 * between blocks `prev_block` and `next_block`.
 *
 * stmts is the list of statements needed at the begining of the block `next_block`.
 *)
let condition_to_cfg env tmp_var_prefix cond prev_block next_block =
    (* manage temporary variables *)
    let get_tmp_expr = ident_generator ("tmp_" ^ tmp_var_prefix ^ "_expr%d_v") in
    let get_block_and = ident_generator (tmp_var_prefix ^ "_and%d") in
    let get_block_or = ident_generator (tmp_var_prefix ^ "_or%d") in

    let rec aux p_block n_block = function
        |InfEq(expr, left) -> [], [(p_block, n_block)], (make_assert env (get_tmp_expr ()) expr C_InfEq)
        |Eq(expr, _) -> [], [(p_block, n_block)], (make_assert env (get_tmp_expr ()) expr C_Eq)
        |NotEq(expr, _) -> [], [(p_block, n_block)], (make_assert env (get_tmp_expr ()) expr C_NotEq)
        |And(left, right) ->
            (* builds the graph :
                p_block -> condition_left -> condition_right -> n_block
             *)
            let and_ident = get_block_and() in
            let left_blocks, left_links, left_statements = aux p_block and_ident left
            and right_blocks, right_links, right_statements = aux and_ident n_block right
            in
            ({name=and_ident; statements=left_statements}::(left_blocks @ right_blocks)),
            (left_links @ right_links),
            right_statements
        |Or(left, right) ->
            (* builds the graph :
                          |-> condition_left  -|
                p_block --|                    |-> n_block
                          |-> condition_right -|
             *)
            let or_ident = get_block_or() in
            let left_blocks, left_links, left_statements = aux p_block (or_ident ^ "_left") left
            and right_blocks, right_links, right_statements = aux p_block (or_ident ^ "_right") right
            in
            ({name=or_ident ^ "_left"; statements=left_statements}
                ::{name=or_ident ^ "_right"; statements=right_statements}::(left_blocks @ right_blocks)),
            ((or_ident ^ "_left", n_block)::(or_ident ^ "_right", n_block)::(left_links @ right_links)),
            []
        |_ -> failwith "use simplify_condition before calling condition_to_cfg"
    in
    aux prev_block next_block (simplify_condition cond)
;;

(* program_to_cfg : program -> cfg *)
let program_to_cfg prg =
    (* manage temporary variables *)
    let get_tmp_var_prefix = ident_generator "tmp_expr%d_v" in
    let get_block_assert = ident_generator "assert%d" in
    let get_block_if = ident_generator "if%d" in
    let get_block_while = ident_generator "while%d" in
    let get_block_for = ident_generator "for%d" in

    (* aux : environment -> cfg_statement list -> cfg_block list
     *      -> (cfg_block_name * cfg_block_name) list -> cfg_block_name -> program
     *      -> cfg_block list * (cfg_block_name * cfg_block_name) list
     *
     * env : current environment
     * statements : list of statements for the current block, in reverse order
     * blocks : blocks already computed
     * links : links already computed
     * block_name : name of the current block
     *)
    let rec aux env statements blocks links block_name = function
        |[] ->
            {name=block_name; statements=List.rev statements}::blocks, links
        |Checkpoint(name)::q ->
            aux env (C_Checkpoint(name)::statements) blocks links block_name q
        |Define(var_t, name, None)::q ->
            aux ((name, var_t)::env) statements blocks links block_name q
        |Define(var_t, name, Some expr)::q ->
            aux ((name, var_t)::env) ((make_assign name env (get_tmp_var_prefix ()) expr) @ statements) blocks links block_name q
        |Assign(name, expr)::q ->
            aux env ((make_assign name env (get_tmp_var_prefix ()) expr) @ statements) blocks links block_name q
        |Assert(cond)::q ->
            let assert_ident = get_block_assert () in

            let cond_blocks, cond_links, cond_statements =
                condition_to_cfg env (assert_ident ^ "_cond") cond block_name assert_ident in

            aux env (List.rev cond_statements)
                ({name=block_name; statements=List.rev statements}::(cond_blocks @ blocks))
                (cond_links @ links)
                assert_ident q
        |If(cond, statements_true, None)::q ->
            aux env statements blocks links block_name (If(cond, statements_true, Some [])::q)
        |If(cond, statements_true, Some statements_false)::q ->
            (* builds the graph :
                                |-> condition ------> if_block ---|
                current_block --|                                 |-> if_end_block
                                |-> not(condition) -> else_block -|
             *)
            let if_ident = get_block_if () in

            let cond_true_blocks, cond_true_links, cond_true_statements =
                condition_to_cfg env (if_ident ^ "_cond") cond block_name if_ident in
            let cond_false_blocks, cond_false_links, cond_false_statements =
                condition_to_cfg env (if_ident ^ "_notcond") (Not(cond)) block_name (if_ident ^ "_else") in

            let true_blocks, true_links =
                aux env (List.rev cond_true_statements) [] [] if_ident statements_true in
            let false_blocks, false_links =
                aux env (List.rev cond_false_statements) [] [] (if_ident ^ "_else") statements_false in

            let last_true_block = List.hd true_blocks in
            let last_false_block = List.hd false_blocks in

            aux env []
                ({name=block_name; statements=List.rev statements}
                    ::(cond_true_blocks @ cond_false_blocks @ true_blocks @ false_blocks @ blocks))
                ((last_true_block.name, if_ident ^ "_end")::(last_false_block.name, if_ident ^ "_end")
                    ::(cond_true_links @ cond_false_links @ true_links @ false_links @ links))
                (if_ident ^ "_end") q
        |While(cond, while_statements)::q ->
            (* builds the graph :
                                     |-------------------------|
                                    \/                         |
                current_block -> pre_while -> condition -> while_block
                                     |
                                     |------> not(condition) -> while_end_block
             *)
            let while_ident = get_block_while () in

            let cond_true_blocks, cond_true_links, cond_true_statements =
                condition_to_cfg env (while_ident ^ "_cond") cond ("pre_" ^ while_ident) while_ident in
            let cond_false_blocks, cond_false_links, cond_false_statements =
                condition_to_cfg env (while_ident ^ "_not_cond") (Not(cond)) ("pre_" ^ while_ident) (while_ident ^ "_end") in

            let while_blocks, while_links = aux env (List.rev cond_true_statements) [] [] while_ident while_statements in
            let last_while_block = List.hd while_blocks in

            aux env (List.rev cond_false_statements)
                ({name=block_name; statements=List.rev statements}
                    ::{name="pre_" ^ while_ident; statements=[]}
                        ::(cond_true_blocks @ cond_false_blocks @ while_blocks @ blocks))
                ((block_name, "pre_" ^ while_ident)
                    ::(last_while_block.name, "pre_" ^ while_ident)::(cond_true_links @ cond_false_links @ while_links @ links))
                (while_ident ^ "_end") q
        |For(init, cond, incr, for_statements)::q ->
            (* builds the graph : current_block -> for_block = init + while -> for_end_block
             * simply uses the while statement
             *)
            let for_ident = get_block_for () in

            let for_blocks, for_links = aux env [] [] [] for_ident [init; While(cond, for_statements @ [incr])] in
            let last_for_block = List.hd for_blocks in

            aux env []
                ({name=block_name; statements=List.rev statements}::(for_blocks @ blocks))
                ((block_name, for_ident)::(last_for_block.name, for_ident ^ "_end")::(for_links @ links))
                (for_ident ^ "_end") q
    in
    let blocks, links = aux [] [] [] [] "main" prg in
    {entry="main"; blocks=blocks; links=links}

(* compute_fixpoint_program : abstract_domain -> abstract_domain -> program -> fixpoint *)
let compute_fixpoint_program z_domain q_domain prog =
    check_types prog;
    let cfg = program_to_cfg prog in
    let checkpoints = compute_fixpoint z_domain q_domain cfg in

    (* removes constraints on temporary variables (variables whose names starts with tmp_) *)
    let z_term_is_temporary_var = function
        |C_ZTerm(_, name) -> String.length name > 4 && String.sub name 0 4 = "tmp_"
        |_ -> false in
    let q_term_is_temporary_var = function
        |C_QTerm(_, _, name) -> String.length name > 4 && String.sub name 0 4 = "tmp_"
        |_ -> false in
    let remove_z_temporary_vars csts =
        List.filter (fun (expr, op) -> not(List.exists z_term_is_temporary_var expr)) csts in
    let remove_q_temporary_vars csts =
        List.filter (fun (expr, op) -> not(List.exists q_term_is_temporary_var expr)) csts in

    Hashtbl.iter (fun key (z_csts, q_csts) ->
        let z_csts =
            match z_domain with
            |ConstantDomain
            |IntervalDomain ->
                remove_z_temporary_vars z_csts
            |OctagonDomain ->
                z_csts (* do nothing for relational domains *)
        in
        let q_csts =
            match q_domain with
            |ConstantDomain
            |IntervalDomain ->
                remove_q_temporary_vars q_csts
            |OctagonDomain ->
                q_csts (* do nothing for relational domains *)
        in
        Hashtbl.replace checkpoints key (z_csts, q_csts)
    ) checkpoints;
    checkpoints
