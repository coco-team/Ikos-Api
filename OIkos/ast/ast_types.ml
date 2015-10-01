(********************************************************************************
 *
 * Definition of the AST (Abstract Syntax Tree) for the high-level API
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

type var_type = ZType | QType

type constant =
    |ZConst of int
    |QConst of int * int

type expression =
    |Var of string
    |Const of constant
    |Add of expression * expression
    |Sub of expression * expression
    |Mul of expression * expression
    |Div of expression * expression
    |Minus of expression

type condition =
    |Inf of expression * expression
    |InfEq of expression * expression
    |Eq of expression * expression
    |NotEq of expression * expression
    |Sup of expression * expression
    |SupEq of expression * expression
    |And of condition * condition
    |Or of condition * condition
    |Not of condition

type statement =
    |Checkpoint of string
    |Define of var_type * string * expression option
    |Assign of string * expression
    |Assert of condition
    |If of condition * statement list * statement list option
    |While of condition * statement list
    |For of statement * condition * statement * statement list

type program = statement list
