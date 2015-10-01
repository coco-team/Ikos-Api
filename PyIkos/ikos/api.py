#################################################################################
#
# Low-level API for IKOS
#
# Author: Maxime Arthaud (maxime@arthaud.me)
#
# Copyright (c) 2014 Carnegie Mellon University
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
#################################################################################

from fractions import Fraction
from ikos import apicore


class Types:
    INT = 'int'
    RATIONAL = 'rational'

    @classmethod
    def all(cls):
        return (cls.INT, cls.RATIONAL)


class TypeException(Exception):
    pass


class Var:
    def __init__(self, name, type):
        assert type in Types.all()
        self.name = name
        self.type = type

    def __repr__(self):
        return 'Var(%s, %s)' % (self.type, self.name)

    def __str__(self):
        return self.name


def IntVar(name):
    return Var(name, Types.INT)


def RationalVar(name):
    return Var(name, Types.RATIONAL)


class LinearExpression:
    def __init__(self, terms):
        assert terms, 'empty expression'
        self.terms = terms

    @staticmethod
    def _constant_type(cst):
        if isinstance(cst, Fraction) or isinstance(cst, float):
            return Types.RATIONAL
        elif isinstance(cst, int):
            return Types.INT
        else:
            raise TypeException('invalid constant: %s' % repr(cst))

    @staticmethod
    def _constant_value(cst, type):
        if type == Types.RATIONAL:
            if isinstance(cst, Fraction) or isinstance(cst, float) or isinstance(cst, int):
                return Fraction(cst)
            else:
                raise TypeException('invalid constant of type %s: %s' % (type, repr(cst)))
        else:
            if isinstance(cst, int):
                return cst
            else:
                raise TypeException('invalid constant of type %s: %s' % (type, repr(cst)))

    def is_constant(self):
        return all(not isinstance(term, tuple) for term in self.terms)

    def constant(self):
        type = self.type
        cst = LinearExpression._constant_value(0, type)

        for term in self.terms:
            if not isinstance(term, tuple):
                cst += LinearExpression._constant_value(term, type)

        return cst

    def variables(self):
        result = set()

        for term in self.terms:
            if isinstance(term, tuple):
                factor, variable = term
                result.add(variable)

        return result

    def normalize(self):
        type = self.type
        terms = []
        cst = LinearExpression._constant_value(0, type)

        for term in self.terms:
            if isinstance(term, tuple):
                factor, variable = term
                factor = LinearExpression._constant_value(factor, type)
                terms.append((factor, variable))
            else:
                cst += LinearExpression._constant_value(term, type)

        return terms, cst

    @property
    def type(self):
        assert self.terms, 'empty expression'
        current_type = None
        only_constant = False

        for term in self.terms:
            if isinstance(term, tuple):
                factor, variable = term
                factor_type = LinearExpression._constant_type(factor)

                if factor_type == Types.RATIONAL and variable.type == Types.INT:
                    raise TypeException('invalid operand types for *: %s and %s' % (factor_type, variable.type))

                # term is well typed

                if current_type is None:
                    current_type = variable.type
                elif only_constant:
                    if current_type == Types.RATIONAL and variable.type == Types.INT:
                        raise TypeException('invalid operand types for +: %s and %s' % (current_type, variable.type))

                    current_type = variable.type
                    only_constant = False
                else:
                    if current_type != variable.type:
                        raise TypeException('invalid operand types for +: %s and %s' % (current_type, variable.type))
            else:
                cst = term
                cst_type = LinearExpression._constant_type(cst)

                if current_type is None:
                    current_type = cst_type
                    only_constant = True
                elif only_constant:
                    if current_type == Types.RATIONAL or cst_type == Types.RATIONAL:
                        current_type = Types.RATIONAL
                    else:
                        current_type = Types.INT
                else:
                    if current_type == Types.INT and cst_type == Types.RATIONAL:
                        raise TypeException('invalid operand types for +: %s and %s' % (current_type, cst_type))

        return current_type

    def multiply(self, constant):
        def multiply_term(term):
            if isinstance(term, tuple):
                factor, var = term
                return factor * constant, var
            else:
                return term * constant

        self.terms = list(map(multiply_term, self.terms))

    def __repr__(self):
        return 'LinearExpression(%s)' % repr(self.terms)

    def __str__(self):
        terms, cst = self.normalize()
        r = ''

        for factor, variable in terms:
            if not r:
                if factor < 0:
                    r += '-'
            else:
                if factor < 0:
                    r += ' - '
                else:
                    r += ' + '

            if abs(factor) != 1:
                r += str(abs(factor))

            r += variable.name

        # constant
        if not r or cst != 0:
            if not r:
                r += str(cst)
            else:
                if cst < 0:
                    r += ' - '
                else:
                    r += ' + '

                r += str(abs(cst))

        return r


class LinearConstraint:
    INF_EQ = '<='
    EQ = '='
    NOT_EQ = '!='

    def __init__(self, expression, operator):
        assert isinstance(expression, LinearExpression)
        assert operator in (LinearConstraint.INF_EQ, LinearConstraint.EQ, LinearConstraint.NOT_EQ)
        self.expression = expression
        self.operator = operator

    def is_tautology(self):
        if self.expression.is_constant():
            if self.operator == LinearConstraint.INF_EQ:
                return self.expression.constant() <= 0
            elif self.operator == LinearConstraint.EQ:
                return self.expression.constant() == 0
            else:
                return self.expression.constant() != 0
        else:
            return False

    def is_contradiction(self):
        if self.expression.is_constant():
            if self.operator == LinearConstraint.INF_EQ:
                return self.expression.constant() > 0
            elif self.operator == LinearConstraint.EQ:
                return self.expression.constant() != 0
            else:
                return self.expression.constant() == 0
        else:
            return False

    @property
    def type(self):
        return self.expression.type

    def __repr__(self):
        return 'LinearConstraint(%s, %s)' % (repr(self.expression), self.operator)

    def __str__(self):
        if self.is_tautology():
            return 'true'
        elif self.is_contradiction():
            return 'false'
        else:
            cst = self.expression.constant()
            return '%s %s %s' % (LinearExpression(self.expression.terms + [-cst]), self.operator, -cst)


class Statement:
    def check_types(self):
        raise NotImplementedError()


class BinaryOperation(Statement):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'

    def __init__(self, var, left, operator, right):
        assert isinstance(var, Var)
        assert isinstance(left, Var)
        assert operator in (BinaryOperation.ADD, BinaryOperation.SUB, BinaryOperation.MUL, BinaryOperation.DIV)
        assert isinstance(right, Var)
        self.var = var
        self.left = left
        self.operator = operator
        self.right = right

    def check_types(self):
        if not(self.var.type == self.left.type and self.var.type == self.right.type):
            raise TypeException('invalid operand types for operation %s: %s, %s and %s' % (self.operator, self.var.type, self.left.type, self.right.type))

    def __repr__(self):
        return 'BinaryOperation(var=%s, left=%s, op=%s, right=%s)' % (repr(self.var), repr(self.left), self.operator, repr(self.right))

    def __str__(self):
        return '%s = %s %s %s;' % (self.var.name, self.left.name, self.operator, self.right.name)


def AddOperation(var, left, right):
    return BinaryOperation(var, left, BinaryOperation.ADD, right)


def SubOperation(var, left, right):
    return BinaryOperation(var, left, BinaryOperation.SUB, right)


def MulOperation(var, left, right):
    return BinaryOperation(var, left, BinaryOperation.MUL, right)


def DivOperation(var, left, right):
    return BinaryOperation(var, left, BinaryOperation.DIV, right)


class Assign(Statement):
    def __init__(self, var, expression):
        assert isinstance(var, Var)
        assert isinstance(expression, LinearExpression)
        self.var = var
        self.expression = expression

    def check_types(self):
        if self.var.type != self.expression.type:
            raise TypeException('invalid assign statement, type mismatch: %s and %s' % (self.var.type, self.expression.type))

    def __repr__(self):
        return 'Assign(%s, %s)' % (repr(self.var), repr(self.expression))

    def __str__(self):
        return '%s = %s;' % (self.var.name, self.expression)


class Assert(Statement):
    def __init__(self, constraint):
        assert isinstance(constraint, LinearConstraint)
        self.constraint = constraint

    def check_types(self):
        self.constraint.type # will check expression type

    def __repr__(self):
        return 'Assert(%s)' % repr(self.constraint)

    def __str__(self):
        return 'assert(%s);' % self.constraint


class Checkpoint(Statement):
    def __init__(self, name):
        self.name = name

    def check_types(self):
        pass

    def __repr__(self):
        return 'Checkpoint(%s)' % self.name

    def __str__(self):
        return 'checkpoint(%s);' % self.name


class BasicBlock:
    def __init__(self, name):
        self.name = name
        self.statements = []
        self.next_blocks = []

    def add_statement(self, stmt):
        assert isinstance(stmt, Statement)
        self.statements.append(stmt)

    def add_statements(self, stmts):
        self.statements += stmts

    def add_next(self, next_block):
        assert isinstance(next_block, BasicBlock)
        self.next_blocks.append(next_block)

    def check_types(self):
        for stmt in self.statements:
            stmt.check_types()

    def __repr__(self):
        return 'BasicBlock(name=%s, statements=[\n%s\n], next_blocks=[%s])' % (
            self.name,
            ',\n'.join('\t' + repr(stmt) for stmt in self.statements),
            ', '.join(b.name for b in self.next_blocks))

    def __str__(self):
        return '%s:\n%s\n--> [%s]' % (
            self.name,
            '\n'.join('\t' + str(stmt) for stmt in self.statements),
            ', '.join(b.name for b in self.next_blocks))


class Cfg:
    ''' represents a Control Flow Graph '''

    def __init__(self, entry):
        self.entry = None
        self.blocks = []

        self.set_entry(entry)

    def add_block(self, block):
        assert isinstance(block, BasicBlock)
        if block not in self.blocks:
            self.blocks.append(block)

    def set_entry(self, entry):
        assert isinstance(entry, BasicBlock)
        self.entry = entry
        self.add_block(entry)

    def check_types(self):
        for block in self.blocks:
            block.check_types()

    def __repr__(self):
        return 'Cfg(entry=%s, blocks=[\n%s\n])' % (
            self.entry.name,
            ',\n'.join('\t' + repr(b).replace('\n', '\n\t') for b in self.blocks))

    def __str__(self):
        return '\n\n'.join(str(b) for b in self.blocks)


class Constraint:
    INF = '<'
    INF_EQ = '<='
    SUP = '>'
    SUP_EQ = '>='
    EQ = '='
    NOT_EQ = '!='
    MOD = 'mod'

    def __init__(self, expression, operator, modulus=None):
        assert isinstance(expression, LinearExpression)
        assert operator in (Constraint.INF, Constraint.INF_EQ, Constraint.SUP, Constraint.SUP_EQ, Constraint.EQ, Constraint.NOT_EQ, Constraint.MOD)
        self.expression = expression
        self.operator = operator
        self.modulus = modulus

    def is_tautology(self):
        if self.expression.is_constant():
            if self.operator == Constraint.INF:
                return self.expression.constant() < 0
            elif self.operator == Constraint.INF_EQ:
                return self.expression.constant() <= 0
            elif self.operator == Constraint.SUP:
                return self.expression.constant() > 0
            elif self.operator == Constraint.SUP_EQ:
                return self.expression.constant() >= 0
            elif self.operator == Constraint.EQ:
                return self.expression.constant() == 0
            elif self.operator == Constraint.NOT_EQ:
                return self.expression.constant() != 0
            else:
                return self.expression.constant() % self.modulus == 0
        else:
            return False

    def is_contradiction(self):
        if self.expression.is_constant():
            if self.operator == Constraint.INF:
                return self.expression.constant() >= 0
            elif self.operator == Constraint.INF_EQ:
                return self.expression.constant() > 0
            elif self.operator == Constraint.SUP:
                return self.expression.constant() <= 0
            elif self.operator == Constraint.SUP_EQ:
                return self.expression.constant() < 0
            elif self.operator == Constraint.EQ:
                return self.expression.constant() != 0
            elif self.operator == Constraint.NOT_EQ:
                return self.expression.constant() == 0
            else:
                return self.expression.constant() % self.modulus != 0
        else:
            return False

    @property
    def type(self):
        return self.expression.type

    def __repr__(self):
        if self.operator == Constraint.MOD:
            return 'Constraint(%s, %s, modulus=%s)' % (repr(self.expression), self.operator, self.modulus)
        else:
            return 'Constraint(%s, %s)' % (repr(self.expression), self.operator)

    def __str__(self):
        if self.is_tautology():
            return 'true'
        elif self.is_contradiction():
            return 'false'
        else:
            cst = self.expression.constant()
            if self.operator == Constraint.MOD:
                return '%s = %s [%s]' % (LinearExpression(self.expression.terms + [-cst]), -cst, self.modulus)
            else:
                return '%s %s %s' % (LinearExpression(self.expression.terms + [-cst]), self.operator, -cst)


class Constraints(list):
    def __repr__(self):
        return 'Constraints(%s)' % ', '.join(map(repr, self))

    def __str__(self):
        if not self:
            return '{}'
        else:
            return '{ %s }' % ', '.join(map(str, self))


class Domain:
    CONSTANT = 'constant'
    INTERVAL = 'interval'
    OCTAGON = 'octagon'


def compute_fixpoint(cfg, z_domain=Domain.INTERVAL, q_domain=Domain.INTERVAL):
    cfg.check_types()
    result = apicore.compute_fixpoint(cfg, z_domain, q_domain)

    def convert_z_term(term):
        if isinstance(term, tuple):
            return term[0], Var(term[1], Types.INT)
        else:
            return term

    def convert_q_term(term):
        if isinstance(term, tuple):
            return term[0], Var(term[1], Types.RATIONAL)
        else:
            return term

    def convert_z_constraint(cst):
        expr = LinearExpression(list(map(convert_z_term, cst[0])))
        return Constraint(expr, *cst[1:])

    def convert_q_constraint(cst):
        expr = LinearExpression(list(map(convert_q_term, cst[0])))
        return Constraint(expr, *cst[1:])

    for key, (z_csts, q_csts) in result.items():
        result[key] = Constraints(map(convert_z_constraint, z_csts)), Constraints(map(convert_q_constraint, q_csts))

    return result

Cfg.fixpoint = compute_fixpoint
