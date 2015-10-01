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

let main = {
    name="main";
    statements=[
        C_ZAssign("x", [C_ZConst 4]);
        C_ZAssign("y", [C_ZConst 0])
    ]}

let while_ = {
    name="while";
    statements=[
        C_ZAssert([C_ZTerm(-1, "x")], C_InfEq);
        C_ZAssign("x", [C_ZTerm(1, "x"); C_ZConst(-1)]);
        C_ZAssign("y", [C_ZTerm(1, "y"); C_ZConst(2)])
    ]}

let end_ = {
    name="end";
    statements=[
        C_ZAssert([C_ZTerm(1, "x"); C_ZConst 1], C_InfEq);
        C_ZAssign("z", [C_ZTerm(2, "y"); C_ZConst 1]);
        C_Checkpoint("end")
    ]}

let cfg = {
    entry="main";
    blocks=[main; while_; end_];
    links=[
        ("main", "while");
        ("main", "end");
        ("while", "while");
        ("while", "end")
    ]}

let _ =
    print_cfg cfg;
    let checkpoints = compute_fixpoint IntervalDomain IntervalDomain cfg in
    let z_constraints, q_constraints = Hashtbl.find checkpoints "end" in
    print_z_linear_constraints z_constraints;
    print_q_linear_constraints q_constraints;
    print_newline ()
