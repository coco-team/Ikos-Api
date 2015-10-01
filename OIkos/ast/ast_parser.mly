/********************************************************************************
 *
 * Parser for a subset of C
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
 *******************************************************************************/

%{
    open Ast_types
%}

%token <string> TVar
%token <int> TZConst
%token <int * int> TQConst
%token TInt TRat
%token TLeftPar TRightPar TLeftBrace, TRightBrace
%token TPlus TMinus TMul TDiv
%token TInf TInfEq TEq TNotEq TSup TSupEq TAnd TOr TNot
%token TSemicolon TComma TAssign
%token TCheckpoint TAssert TIf TElse TWhile TFor
%token TEOF

%start nt_program
%type <Ast_types.program> nt_program

%left TOr
%left TAnd
%left TNot

%left TPlus TMinus
%left TMul TDiv
%left TNeg

%%

nt_program :
    | TEOF { [] }
    | nt_statement nt_program { $1::$2 }
;

nt_statements :
    | { [] }
    | nt_statement nt_statements { $1::$2 }
;

nt_statement : 
    | TCheckpoint TLeftPar TVar TRightPar TSemicolon { Checkpoint($3) }
    | TInt TVar TSemicolon { Define(ZType, $2, None) }
    | TInt TVar TAssign nt_expression TSemicolon { Define(ZType, $2, Some $4) }
    | TRat TVar TSemicolon { Define(QType, $2, None) }
    | TRat TVar TAssign nt_expression TSemicolon { Define(QType, $2, Some $4) }
    | TVar TAssign nt_expression TSemicolon { Assign($1, $3) }
    | TAssert TLeftPar nt_condition TRightPar TSemicolon { Assert($3) }
    | TIf TLeftPar nt_condition TRightPar TLeftBrace nt_statements TRightBrace TElse TLeftBrace nt_statements TRightBrace { If($3, $6, Some $10) }
    | TIf TLeftPar nt_condition TRightPar TLeftBrace nt_statements TRightBrace { If($3, $6, None) }
    | TWhile TLeftPar nt_condition TRightPar TLeftBrace nt_statements TRightBrace { While($3, $6) }
    | TFor TLeftPar nt_statement TComma nt_condition TComma nt_statement TRightPar TLeftBrace nt_statements TRightBrace { For($3, $5, $7, $10) }
;

nt_condition :
    | TLeftPar nt_condition TRightPar { $2 }
    | TNot nt_condition { Not($2) }
    | nt_condition TOr nt_condition { Or($1, $3) }
    | nt_condition TAnd nt_condition { And($1, $3) }
    | nt_expression TInf nt_expression { Inf($1, $3) }
    | nt_expression TInfEq nt_expression { InfEq($1, $3) }
    | nt_expression TEq nt_expression { Eq($1, $3) }
    | nt_expression TNotEq nt_expression { NotEq($1, $3) }
    | nt_expression TSup nt_expression { Sup($1, $3) }
    | nt_expression TSupEq nt_expression { SupEq($1, $3) }
;

nt_expression :
    | TLeftPar nt_expression TRightPar { $2 }
    | TVar { Var($1) }
    | TZConst { Const(ZConst($1)) }
    | TQConst { Const(QConst(fst($1), snd($1))) }
    | TMinus nt_expression %prec TNeg { Minus($2) }
    | nt_expression TPlus nt_expression { Add($1, $3) }
    | nt_expression TMinus nt_expression { Sub($1, $3) }
    | nt_expression TMul nt_expression { Mul($1, $3) }
    | nt_expression TDiv nt_expression { Div($1, $3) }
;
