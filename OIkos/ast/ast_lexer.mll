(********************************************************************************
 *
 * Lexer for a subset of C
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

{
    open Ast_parser
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '_' '.'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*

rule tokenize = parse
    | [' ' '\t' '\n' '\r']                    { tokenize lexbuf }
    | "int"                                   { TInt }
    | "rat"                                   { TRat }
    | '('                                     { TLeftPar }
    | ')'                                     { TRightPar }
    | '{'                                     { TLeftBrace }
    | '}'                                     { TRightBrace }
    | '+'                                     { TPlus }
    | '-'                                     { TMinus }
    | '*'                                     { TMul }
    | '/'                                     { TDiv }
    | '<'                                     { TInf }
    | "<="                                    { TInfEq }
    | "=="                                    { TEq }
    | "!="                                    { TNotEq }
    | '>'                                     { TSup }
    | ">="                                    { TSupEq }
    | "&&"                                    { TAnd }
    | "||"                                    { TOr }
    | '!'                                     { TNot }
    | ';'                                     { TSemicolon }
    | ','                                     { TComma }
    | '='                                     { TAssign }
    | "checkpoint"                            { TCheckpoint }
    | "assert"                                { TAssert }
    | "if"                                    { TIf }
    | "else"                                  { TElse }
    | "while"                                 { TWhile }
    | "for"                                   { TFor }
    | (digit+ as num) '/' (digit+ as den)     { TQConst(int_of_string num, int_of_string den) }
    | (digit+ as num)                         { TZConst(int_of_string num) }
    | ident as var_name                       { TVar(var_name) }
    | eof                                     { TEOF }
