(********************************************************************************
 *
 * IKOS backend
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

open Corelang
open LustreSpec
open Machine_code

open Ast_types

open Format

(* ast_type : Types.type_expr -> Ast_types.var_type *)
let rec ast_type type_expr =
  let error () =
      eprintf "Cannot use type %a with IKOS@." Types.print_node_struct_ty_field ("", type_expr);
      assert false
  in
  match (Types.repr type_expr).Types.tdesc with
  | Types.Tint -> ZType
  | Types.Tbool -> ZType
  | Types.Treal  -> QType
  | Types.Tenum _ -> ZType
  | Types.Tconst name ->
      (match Hashtbl.find type_table (Tydec_const name) with
      |Tydec_enum _ -> ZType
      | _ -> error())
  | _ -> error ()

(* get_node : LustreSpec.top_decl_desc -> LustreSpec.node_desc *)
let get_node = function
  | Node nd -> nd
  | _ -> failwith "get_node"

(* get_main_machine : Machine_code.machine_t list -> Machine_code.machine_t *)
let get_main_machine machines =
  match get_machine_opt (!Options.main_node) machines with
  | Some m -> m
  | None ->
      eprintf "Unable to find a main node named %s@.@?" (!Options.main_node);
      assert false

(* Variable naming conventions *)
let local_var_name name = name
let memory_var_name name = Printf.sprintf "memory.%s" name
let instance_var_name name = Printf.sprintf "inst.%s.first" name

(* define_rand : string -> Ast_types.var_type -> Ast_types.statement *)
let define_rand name = function
  | ZType -> Define(ZType, name, None)
  | QType -> Define(QType, name, None)

(* define : string -> int -> Ast_types.var_type -> Ast_types.statement *)
let define name default = function
  | ZType -> Define(ZType, name, Some(Const(ZConst default)))
  | QType -> Define(QType, name, Some(Const(QConst(default, 1))))

(* declare_rand_vars : (string -> string) -> LustreSpec.var_decl list -> Ast_types.statement list *)
let declare_rand_vars namer vars =
  (* add assertions for bool and enum *)

  (* assert_enum : string -> int -> 'a list -> Ast_types.condition *)
  let rec assert_enum var_name i = function
    |[] -> failwith "unreachable"
    |t::[] -> Eq(Var var_name, Const(ZConst i))
    |t::q -> Or(Eq(Var var_name, Const(ZConst i)), assert_enum var_name (i+1) q)
  in

  (* fold_fun : Ast_types.statement list -> LustreSpec.var_decl -> Ast_types.statement list *)
  let fold_fun accu id =
    let ty = ast_type id.var_type in
    let var_name = namer id.var_id in
    match (Types.repr id.var_type).Types.tdesc with
    | Types.Tbool ->
      (define_rand var_name ty)
        ::Assert(assert_enum var_name 0 ["true"; "false"])
        ::accu
    | Types.Tenum idents ->
      (define_rand var_name ty)
        ::Assert(assert_enum var_name 0 idents)
        ::accu
    | Types.Tconst name ->
      (match Hashtbl.find type_table (Tydec_const name) with
      |Tydec_enum idents ->
        (define_rand var_name ty)
          ::Assert(assert_enum var_name 0 idents)
          ::accu
      | _ -> failwith "unreachable"
      )
    | _ -> (define_rand var_name ty)::accu
  in
  List.fold_left fold_fun [] vars

(* declare_vars : (string -> string) -> LustreSpec.var_decl list -> Ast_types.statement list *)
let declare_vars namer vars =
  List.map (fun id -> define (namer id.var_id) 0 (ast_type id.var_type)) vars

(* declare_instances : (ident * static_call) list -> Ast_types.statement list *)
let declare_instances instances =
  List.map (fun (ident, (n, _)) ->
    if (node_name n) = "_arrow" then
      define (instance_var_name ident) 1 ZType
    else
      failwith "TODO: declare_instances") instances

(**********************)
(* Manage expressions *)
(**********************)

(*
 * the problem is that LustreC expressions can also be conditions,
 * when Ast makes the difference.
 *)
type extended_expression =
  |AstExpr of expression
  |AstCond of condition

(* force_condition : extended_expression -> condition *)
let force_condition = function
  |AstExpr e -> Eq(e, Const(ZConst 1))
  |AstCond c -> c

(* ext_expression_of_value : Machine_code.value_t -> extended_expression *)
let ext_expression_of_value value =
  let error () =
      eprintf "Cannot use the expression %a with IKOS@." pp_val value;
      assert false
  in
  let tag_value = function
    |"true" -> 1
    |"false" -> 0
    |name ->
      let fold_fun key value = function
        |Some x -> Some x
        |None ->
          (match value with
            |Tydec_enum tags when (List.exists (fun x -> name = x) tags) ->
              Some (Utils.position (fun x -> name = x) (List.rev tags))
            |_ -> None) in
      let value = Hashtbl.fold fold_fun type_table None in
      (match value with
      |Some x -> x
      |None -> error ()) in
  let rec binary_expr v1 v2 builder =
    match (aux v1, aux v2) with
    |AstExpr e1, AstExpr e2 -> builder e1 e2
    |_ -> error ()
  and binary_cond v1 v2 builder =
    builder (force_condition (aux v1)) (force_condition (aux v2))
  and aux = function
    |Cst(Const_int x) -> AstExpr(Const(ZConst x))
    |Cst(Const_real x) -> aux (Cst(Const_float (float_of_string x)))
    |Cst(Const_float x) ->
      let prec = 1000000 in
      AstExpr(Const(QConst(int_of_float(x *. float_of_int(prec)), prec)))
    |Cst(Const_tag name) -> AstExpr(Const(ZConst(tag_value name)))
    |LocalVar var -> AstExpr(Var (local_var_name var.var_id))
    |StateVar var -> AstExpr(Var (memory_var_name var.var_id))
    |Fun("uminus", [v]) ->
      (match aux v with
      |AstExpr e -> AstExpr(Minus e)
      |_ -> error ())
    |Fun("+", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstExpr(Add(e1, e2)))
    |Fun("-", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstExpr(Sub(e1, e2)))
    |Fun("*", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstExpr(Mul(e1, e2)))
    |Fun("/", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstExpr(Div(e1, e2)))
    |Fun("<", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstCond(Inf(e1, e2)))
    |Fun("<=", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstCond(InfEq(e1, e2)))
    |Fun(">", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstCond(Sup(e1, e2)))
    |Fun(">=", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstCond(SupEq(e1, e2)))
    |Fun("!=", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstCond(NotEq(e1, e2)))
    |Fun("=", [v1; v2]) -> binary_expr v1 v2 (fun e1 e2 -> AstCond(Eq(e1, e2)))
    |Fun("&&", [v1; v2]) -> binary_cond v1 v2 (fun c1 c2 -> AstCond(And(c1, c2)))
    |Fun("||", [v1; v2]) -> binary_cond v1 v2 (fun c1 c2 -> AstCond(Or(c1, c2)))
    |Fun("not", [v]) -> AstCond(Not(force_condition (aux v)))
    |Fun("impl", [v1; v2]) -> binary_cond v1 v2 (fun c1 c2 -> AstCond(Or(Not(c1), c2)))
    |Fun("equi", [v1; v2]) -> binary_cond v1 v2 (fun c1 c2 -> AstCond(And(Or(Not(c1), c2), Or(Not(c2), c1))))
    |_ -> error ()
  in aux value

(* define_from_value : Ast_types.var_type -> string -> Machine_code.value_t -> Ast_types.statement *)
let define_from_value var_t var_name value =
  match ext_expression_of_value value with
  |AstExpr e -> Define(var_t, var_name, Some e)
  |AstCond c ->
    If(c,
      [Define(var_t, var_name, Some(Const(ZConst 1)))],
      Some [Define(var_t, var_name, Some(Const(ZConst 0)))])

(* assign_from_value : string -> Machine_code.value_t -> Ast_types.statement *)
let assign_from_value var_name value =
  match ext_expression_of_value value with
  |AstExpr e -> Assign(var_name, e)
  |AstCond c ->
    If(c,
      [Assign(var_name, Const(ZConst 1))],
      Some [Assign(var_name, Const(ZConst 0))])

(* condition_of_value : Machine_code.value_t -> Ast_types.condition *)
let condition_of_value value =
  force_condition (ext_expression_of_value value)

(* translate_expr : LustreSpec.expr -> Machine_code.machine_t -> Machine_code.value_t *)
let rec translate_expr m expr =
  match expr.expr_desc with
  | Expr_const v -> Cst v
  | Expr_ident name ->
    let find_var = List.find (fun o -> o.var_id = name) in
    (try StateVar (find_var m.mmemory)
    with |Not_found -> LocalVar (find_var (m.mstep.step_inputs @ m.mstep.step_outputs @ m.mstep.step_locals)))
  | Expr_when (e1, _, _) -> translate_expr m e1
  | Expr_appl (id, e, _) when Basic_library.is_internal_fun id ->
      let nd = node_from_name id in
      Fun (node_name nd, List.map (translate_expr m) (expr_list_of_expr e))
  | _ -> raise NormalizationError

(* statement_of_instr : (string * bool) list -> Machine_code.machine_t -> Machine_code.instr_t -> Ast_types.statement *)
let rec statement_of_instr dependencies m = function
  | MStep(var_list, ident, values) ->
      let (n, _) = List.assoc ident m.minstances in
      if (node_name n) = "_arrow" then
        let first_var_name = instance_var_name ident in
        let var_name = local_var_name ((List.hd var_list).var_id) in
        let [val_true; val_false] = values in

        If(Eq(Var first_var_name, Const(ZConst 1)),
          [Assign(first_var_name, Const(ZConst 0)); assign_from_value var_name val_true],
          Some [assign_from_value var_name val_false])
      else
        failwith "TODO: MStep"
  | MReset ident ->
      let (n, _) = List.assoc ident m.minstances in
      if (node_name n) = "_arrow" then
        let first_var_name = instance_var_name ident in
        Assign(first_var_name, Const(ZConst 1))
      else
        failwith "TODO: MReset"
  | MLocalAssign(var, value) -> assign_from_value (local_var_name var.var_id) value
  | MStateAssign(var, value) -> assign_from_value (memory_var_name var.var_id) value
  | MBranch(value, branches) ->
      if fst (List.hd branches) = tag_true || fst (List.hd branches) = tag_false then
        let true_branch = try List.assoc tag_true branches with Not_found -> [] in
        let false_branch = try List.assoc tag_false branches with Not_found -> [] in
        If(condition_of_value value,
          List.map (statement_of_instr dependencies m) true_branch,
          Some (List.map (statement_of_instr dependencies m) false_branch))
      else
        failwith "TODO: MBranch with enum type case"
        (* actually, it is unreachable because of the parser *)

let program_to_ast prog machines dependencies =
  assert(List.length prog = 1);
  let main_machine = get_main_machine machines in

  (* TODO: manage types definitions *)
  (* TODO: manage global constants *)
  (* TODO: manage imported nodes ? *)
  (* TODO: be carefull with conflicts for identifiants, see mk_new_name *)

  (* declaration of inputs/outputs variables *)
  let inits = declare_rand_vars local_var_name main_machine.mstep.step_inputs
    @ declare_vars local_var_name main_machine.mstep.step_outputs
    @ declare_vars local_var_name main_machine.mstatic
    @ declare_vars memory_var_name main_machine.mmemory
    @ declare_instances main_machine.minstances
    @ (List.map (statement_of_instr dependencies main_machine) main_machine.minit) in

  (* locals *)
  let locals = declare_vars local_var_name main_machine.mstep.step_locals in

  (* instrs *)
  let instrs = List.map (statement_of_instr dependencies main_machine) main_machine.mstep.step_instrs in

  (* assertions *)
  let asserts = List.map (fun value -> Assert(condition_of_value value)) main_machine.mstep.step_asserts in

  (* properties *)
  let new_property =
      let cpt = ref 0 in
      fun () -> incr cpt; Printf.sprintf "PROPERTY%d" !cpt in

  let properties = List.flatten (List.map (fun a -> a.annots) main_machine.mannot) in
  let properties = List.filter (fun (names, expr) -> names = ["PROPERTY"]) properties in
  let properties_decl = List.map (fun (names, eexpr) ->
    define_from_value ZType (new_property ()) (translate_expr main_machine eexpr.eexpr_qfexpr)) properties in

  inits @ [
    While(
      Eq(Const(ZConst 1), Const(ZConst 1)),
      locals @ instrs @ asserts @ properties_decl @ [Checkpoint "node"])
  ]

let analyze prog machines dependencies =
  (* AST generation *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. AST generation@,");
  let ast = program_to_ast prog machines dependencies in
  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>%s@]@," (Ast.string_of_program ast));

  (* Type checking *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. AST type checking@.");
  Ast.check_types ast;

  (* CFG generation *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. CFG generation@.");
  let cfg = Ast.program_to_cfg ast in
  Log.report ~level:3 (fun fmt -> fprintf fmt "@[<v 2>%s@]@," (Ikos.string_of_cfg cfg));

  (* Fixpoint computation *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. IKOS fixpoint computation@.");
  print_newline ();
  let checkpoints = Ikos.compute_fixpoint Ikos.IntervalDomain Ikos.IntervalDomain cfg in
  let z_constraints, q_constraints = Hashtbl.find checkpoints "node" in

  (* traceability *)
  let main_machine = get_main_machine machines in

  (* (ident * expr) list *)
  let traces =
    let all_annots = List.flatten (List.map (fun ann -> ann.annots) main_machine.mannot) in
    let filtered = List.filter (fun (kwds, _) -> kwds = ["horn_backend"; "trace"]) all_annots in
    let content = List.map snd filtered in
    (* elements are supposed to be a pair: variable, expression *)
    List.map (fun ee ->
      match ee.eexpr_quantifiers, ee.eexpr_qfexpr.expr_desc with
        | [], Expr_tuple [v;e] -> (
          match v.expr_desc with
          | Expr_ident vid -> vid, e
          | _ -> assert false)
        | _ -> assert false) content in

  List.iter (fun (ident, expr) -> printf "%s = %a" ident Printers.pp_expr expr; print_newline ()) traces;
  List.iter (fun cst -> Ikos.print_z_linear_constraint cst; print_newline ()) z_constraints;
  List.iter (fun cst -> Ikos.print_q_linear_constraint cst; print_newline ()) q_constraints
