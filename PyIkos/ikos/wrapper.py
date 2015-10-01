#################################################################################
#
# High-level wrapper for IKOS
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
from ikos import api


# warning: api.TypeException != ikos.TypeException
class TypeException(Exception):
    pass


class Var(api.Var):
    # for expressions
    def __pos__(self):
        return VarExpression(self)

    def __neg__(self):
        return UnaryExpression(UnaryExpression.MINUS, VarExpression(self))

    def _method_binary(op):
        def aux(self, other):
            return BinaryExpression(getattr(BinaryExpression, op), VarExpression(self), make_expression(other))
        return aux

    __add__ = _method_binary('ADD')
    __sub__ = _method_binary('SUB')
    __mul__ = _method_binary('MUL')
    __div__ = _method_binary('DIV')
    __radd__ = __add__
    __rsub__ = __sub__
    __rmul__ = __mul__
    __rdiv__ = __div__

    # for conditions
    def _method_comparison(op):
        def aux(self, other):
            return CompCondition(getattr(CompCondition, op), VarExpression(self), make_expression(other))
        return aux

    __eq__ = _method_comparison('EQ')
    __ne__ = _method_comparison('NOT_EQ')
    __lt__ = _method_comparison('INF')
    __le__ = _method_comparison('INF_EQ')
    __gt__ = _method_comparison('SUP')
    __ge__ = _method_comparison('SUP_EQ')


def Int(name):
    return Var(name, api.Types.INT)


def Rational(name):
    return Var(name, api.Types.RATIONAL)


class Expression:
    @property
    def type(self):
        raise NotImplementedError()

    def is_constant(self):
        raise NotImplementedError()

    # for expressions
    def __pos__(self):
        return self

    def __neg__(self):
        return UnaryExpression(UnaryExpression.MINUS, self)

    def _method_binary(op):
        def aux(self, other):
            return BinaryExpression(getattr(BinaryExpression, op), self, make_expression(other))
        return aux

    __add__ = _method_binary('ADD')
    __sub__ = _method_binary('SUB')
    __mul__ = _method_binary('MUL')
    __div__ = _method_binary('DIV')
    __radd__ = __add__
    __rsub__ = __sub__
    __rmul__ = __mul__
    __rdiv__ = __div__

    # for conditions
    def _method_comparison(op):
        def aux(self, other):
            return CompCondition(getattr(CompCondition, op), self, make_expression(other))
        return aux

    __eq__ = _method_comparison('EQ')
    __ne__ = _method_comparison('NOT_EQ')
    __lt__ = _method_comparison('INF')
    __le__ = _method_comparison('INF_EQ')
    __gt__ = _method_comparison('SUP')
    __ge__ = _method_comparison('SUP_EQ')


class VarExpression(Expression):
    def __init__(self, var):
        assert isinstance(var, Var)
        self.var = var

    @property
    def type(self):
        return self.var.type

    def is_constant(self):
        return False

    def __repr__(self):
        return 'VarExpression(%s)' % repr(self.var)

    def __str__(self):
        return self.var.name


class ConstExpression(Expression):
    def __init__(self, constant):
        assert isinstance(constant, Fraction) or isinstance(constant, float) or isinstance(constant, int)
        self.constant = constant

    @property
    def type(self):
        if isinstance(self.constant, Fraction) or isinstance(self.constant, float):
            return api.Types.RATIONAL
        elif isinstance(self.constant, int):
            return api.Types.INT
        else:
            raise TypeException('invalid constant: %s' % repr(self.constant))

    def is_constant(self):
        return True

    def __repr__(self):
        return 'ConstExpression(%s)' % repr(self.constant)

    def __str__(self):
        return str(self.constant)


class UnaryExpression(Expression):
    MINUS = '-'

    def __init__(self, op, expression):
        assert op == UnaryExpression.MINUS
        assert isinstance(expression, Expression)
        self.operator = op
        self.expression = expression

    @property
    def type(self):
        return self.expression.type

    def is_constant(self):
        return self.expression.is_constant()

    @property
    def constant(self):
        assert self.operator == UnaryExpression.MINUS
        return -self.expression.constant

    def __repr__(self):
        return 'UnaryExpression(%s, %s)' % (self.operator, repr(self.expression))

    def __str__(self):
        return '%s(%s)' % (self.operator, str(self.expression))


class BinaryExpression(Expression):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'

    def __init__(self, op, left, right):
        assert op in (BinaryExpression.ADD, BinaryExpression.SUB, BinaryExpression.MUL, BinaryExpression.DIV)
        assert isinstance(left, Expression)
        assert isinstance(right, Expression)
        self.operator = op
        self.left = left
        self.right = right

    @property
    def type(self):
        left_type = self.left.type
        right_type = self.right.type

        if self.left.is_constant():
            if self.right.is_constant():
                if api.Types.RATIONAL in (left_type, right_type):
                    return api.Types.RATIONAL
                else:
                    return api.Types.INT
            else:
                if left_type == api.Types.RATIONAL and right_type == api.Types.INT:
                    raise TypeException('invalid operand types for %s: %s and %s' % (self.operator, left_type, right_type))

                return right_type
        else:
            if self.right.is_constant():
                if right_type == api.Types.RATIONAL and left_type == api.Types.INT:
                    raise TypeException('invalid operand types for %s: %s and %s' % (self.operator, left_type, right_type))

                return left_type
            else:
                if left_type != right_type:
                    raise TypeException('invalid operand types for %s: %s and %s' % (self.operator, left_type, right_type))

                return left_type

    def is_constant(self):
        return self.left.is_constant() and self.right.is_constant()

    @property
    def constant(self):
        assert self.operator in (BinaryExpression.ADD, BinaryExpression.SUB, BinaryExpression.MUL, BinaryExpression.DIV)
        if self.operator == BinaryExpression.ADD:
            return self.left.constant + self.right.constant
        elif self.operator == BinaryExpression.SUB:
            return self.left.constant - self.right.constant
        elif self.operator == BinaryExpression.MUL:
            return self.left.constant * self.right.constant
        else:
            if self.type == api.Types.INT:
                return self.left.constant // self.right.constant
            else:
                return self.left.constant / self.right.constant

    def __repr__(self):
        return 'BinaryExpression(%s, %s, %s)' % (self.operator, repr(self.left), repr(self.right))

    def __str__(self):
        return '(%s) %s (%s)' % (str(self.left), self.operator, str(self.right))


def make_expression(obj):
    if isinstance(obj, Expression):
        return obj
    elif isinstance(obj, Var):
        return VarExpression(obj)
    elif isinstance(obj, Fraction) or isinstance(obj, float) or isinstance(obj, int):
        return ConstExpression(obj)
    else:
        raise TypeException('invalid expression: %s' % repr(obj))


class Condition:
    def check_types(self):
        raise NotImplementedError()

    # operators &, | and ~
    def __invert__(self):
        return UnaryCondition(UnaryCondition.NOT, self)

    def __and__(self, other):
        return BinaryCondition(BinaryCondition.AND, self, make_condition(other))

    def __or__(self, other):
        return BinaryCondition(BinaryCondition.OR, self, make_condition(other))


class CompCondition(Condition):
    INF = '<'
    INF_EQ = '<='
    SUP = '>'
    SUP_EQ = '>='
    EQ = '='
    NOT_EQ = '!='

    def __init__(self, op, left, right):
        assert op in (CompCondition.INF, CompCondition.INF_EQ, CompCondition.SUP, CompCondition.SUP_EQ, CompCondition.EQ, CompCondition.NOT_EQ)
        assert isinstance(left, Expression)
        assert isinstance(right, Expression)
        self.operator = op
        self.left = left
        self.right = right

    def check_types(self):
        self.type # will check expression type

    @property
    def type(self):
        left_type = self.left.type
        right_type = self.right.type

        if self.left.is_constant():
            if self.right.is_constant():
                if api.Types.RATIONAL in (left_type, right_type):
                    return api.Types.RATIONAL
                else:
                    return api.Types.INT
            else:
                if left_type == api.Types.RATIONAL and right_type == api.Types.INT:
                    raise TypeException('cannot compare types %s and %s' % (left_type, right_type))

                return right_type
        else:
            if self.right.is_constant():
                if right_type == api.Types.RATIONAL and left_type == api.Types.INT:
                    raise TypeException('cannot compare types %s and %s' % (left_type, right_type))

                return left_type
            else:
                if left_type != right_type:
                    raise TypeException('cannot compare types %s and %s' % (left_type, right_type))

                return left_type

    def __repr__(self):
        return 'CompCondition(%s, %s, %s)' % (self.operator, repr(self.left), repr(self.right))

    def __str__(self):
        return '%s %s %s' % (str(self.left), self.operator, str(self.right))


class UnaryCondition(Condition):
    NOT = '!'

    def __init__(self, op, condition):
        assert op == UnaryCondition.NOT
        assert isinstance(condition, Condition)
        self.operator = op
        self.condition = condition

    def check_types(self):
        self.condition.check_types()

    def __repr__(self):
        return 'UnaryCondition(%s, %s)' % (self.operator, repr(self.condition))

    def __str__(self):
        return '%s(%s)' % (self.operator, str(self.condition))


class BinaryCondition(Condition):
    AND = '&&'
    OR = '||'

    def __init__(self, op, left, right):
        assert op in (BinaryCondition.AND, BinaryCondition.OR)
        assert isinstance(left, Condition)
        assert isinstance(right, Condition)
        self.operator = op
        self.left = left
        self.right = right

    def check_types(self):
        self.left.check_types()
        self.right.check_types()

    def __repr__(self):
        return 'BinaryCondition(%s, %s, %s)' % (self.operator, repr(self.left), repr(self.right))

    def __str__(self):
        return '%s %s %s' % (str(self.left), self.operator, str(self.right))


def make_condition(obj):
    if isinstance(obj, Condition):
        return obj
    else:
        raise TypeException('invalid condition: %s' % repr(obj))


class Statement:
    def check_types(self):
        raise NotImplementedError()


class Checkpoint(Statement):
    def __init__(self, name):
        self.name = name

    def check_types(self):
        pass

    def __repr__(self):
        return 'Checkpoint(%s)' % self.name

    def __str__(self):
        return 'checkpoint(%s);' % self.name


class Assign(Statement):
    def __init__(self, var, expression):
        assert isinstance(var, Var)
        self.var = var
        self.expression = make_expression(expression)

    def check_types(self):
        expression_type = self.expression.type
        is_constant = self.expression.is_constant()

        if ((is_constant and expression_type == api.Types.RATIONAL and self.var.type == api.Types.INT)
                or (not(is_constant) and self.var.type != expression_type)):
            raise TypeException('invalid assign statement, type mismatch: %s and %s' % (self.var.type, expression_type))

    def __repr__(self):
        return 'Assign(%s, %s)' % (repr(self.var), repr(self.expression))

    def __str__(self):
        return '%s = %s;' % (self.var.name, self.expression)


class Assert(Statement):
    def __init__(self, condition):
        self.condition = make_condition(condition)

    def check_types(self):
        self.condition.check_types()

    def __repr__(self):
        return 'Assert(%s)' % repr(self.condition)

    def __str__(self):
        return 'assert(%s);' % self.condition


class IfStatement(Statement):
    def __init__(self, condition):
        self.condition = make_condition(condition)
        self.true = Program()
        self.false = Program()

    def check_types(self):
        self.condition.check_types()
        self.true.check_types()
        self.false.check_types()

    def __repr__(self):
        return 'IfStatement(%s, true=%s, false=%s)' % (repr(self.condition), repr(self.true), repr(self.false))

    def __str__(self):
        if self.false.statements:
            return 'if (%s) {\n\t%s\n} else {\n\t%s\n}' % (str(self.condition),
                                                           str(self.true).replace('\n', '\n\t'),
                                                           str(self.false).replace('\n', '\n\t'))
        else:
            return 'if (%s) {\n\t%s\n}' % (str(self.condition),
                                           str(self.true).replace('\n', '\n\t'))


class WhileStatement(Statement):
    def __init__(self, condition):
        self.condition = make_condition(condition)
        self.block = Program()

    def check_types(self):
        self.condition.check_types()
        self.block.check_types()

    def __repr__(self):
        return 'WhileStatement(%s, %s)' % (repr(self.condition), repr(self.block))

    def __str__(self):
        return 'while (%s) {\n\t%s\n}' % (str(self.condition),
                                          str(self.block).replace('\n', '\n\t'))


class Program:
    def __init__(self):
        self.statements = []

    def check_types(self):
        for stmt in self.statements:
            stmt.check_types()

    def assign(self, var, expression):
        self.statements.append(Assign(var, expression))

    def add_assert(self, condition):
        self.statements.append(Assert(condition))

    def checkpoint(self, name):
        self.statements.append(Checkpoint(name))

    def add_if(self, condition):
        stmt = IfStatement(condition)
        self.statements.append(stmt)
        return stmt

    def add_while(self, condition):
        stmt = WhileStatement(condition)
        self.statements.append(stmt)
        return stmt.block

    def fixpoint(self, z_domain=api.Domain.INTERVAL, q_domain=api.Domain.INTERVAL):
        self.check_types()
        cfg = program_to_cfg(self)
        result = cfg.fixpoint(z_domain, q_domain)

        # remove constraints on temporary variables
        def contains_tmp_var(cst):
            return any(v.name.startswith('tmp_') for v in cst.expression.variables())

        for key, (z_csts, q_csts) in result.items():
            # only for non-relational domains
            if z_domain != api.Domain.OCTAGON:
                z_csts = api.Constraints(filter(lambda cst: not(contains_tmp_var(cst)), z_csts))

            if q_domain != api.Domain.OCTAGON:
                q_csts = api.Constraints(filter(lambda cst: not(contains_tmp_var(cst)), q_csts))

            result[key] = z_csts, q_csts

        return result

    def __repr__(self):
        return 'Program(%s)' % repr(self.statements)

    def __str__(self):
        return '\n'.join(map(str, self.statements))


###########################################
# Conversion from ikos.Program to api.Cfg #
###########################################

def is_var(expr):
    return isinstance(expr, VarExpression)


def is_const(expr):
    return isinstance(expr, ConstExpression)


def is_minus(expr):
    return isinstance(expr, UnaryExpression) and expr.operator == UnaryExpression.MINUS


def is_add(expr):
    return isinstance(expr, BinaryExpression) and expr.operator == BinaryExpression.ADD


def is_sub(expr):
    return isinstance(expr, BinaryExpression) and expr.operator == BinaryExpression.SUB


def is_mul(expr):
    return isinstance(expr, BinaryExpression) and expr.operator == BinaryExpression.MUL


def is_div(expr):
    return isinstance(expr, BinaryExpression) and expr.operator == BinaryExpression.DIV


def is_inf(cond):
    return isinstance(cond, CompCondition) and cond.operator == CompCondition.INF


def is_inf_eq(cond):
    return isinstance(cond, CompCondition) and cond.operator == CompCondition.INF_EQ


def is_sup(cond):
    return isinstance(cond, CompCondition) and cond.operator == CompCondition.SUP


def is_sup_eq(cond):
    return isinstance(cond, CompCondition) and cond.operator == CompCondition.SUP_EQ


def is_eq(cond):
    return isinstance(cond, CompCondition) and cond.operator == CompCondition.EQ


def is_not_eq(cond):
    return isinstance(cond, CompCondition) and cond.operator == CompCondition.NOT_EQ


def is_not(cond):
    return isinstance(cond, UnaryCondition) and cond.operator == UnaryCondition.NOT


def is_and(cond):
    return isinstance(cond, BinaryCondition) and cond.operator == BinaryCondition.AND


def is_or(cond):
    return isinstance(cond, BinaryCondition) and cond.operator == BinaryCondition.OR


def normalize_constant(cst, type):
    if type == api.Types.RATIONAL:
        if isinstance(cst, Fraction) or isinstance(cst, float) or isinstance(cst, int):
            return Fraction(cst)
        else:
            raise TypeException('invalid constant of type %s: %s' % (type, repr(cst)))
    else:
        if isinstance(cst, int):
            return cst
        else:
            raise TypeException('invalid constant of type %s: %s' % (type, repr(cst)))


def ident_generator(fmt):
    ''' Returns a function generating incremental identifiants '''
    def aux():
        aux.val += 1
        return fmt % aux.val
    aux.val = 0
    return aux


def expression_to_statements(tmp_var_fmt, expr):
    '''
    Returns a list of statements to compute the expression,
    and the name of the variable containing the result.
    '''
    expr_type = expr.type
    tmp_var_name = ident_generator(tmp_var_fmt)

    def aux(e):
        if is_var(e):
            return [], e.var

        elif is_const(e):
            tmp = Var(tmp_var_name(), expr_type)
            constant = normalize_constant(e.constant, expr_type)
            return [api.Assign(tmp, api.LinearExpression([constant]))], tmp

        elif is_minus(e):
            stmts, r = aux(e.expression)
            if stmts and isinstance(stmts[-1], api.Assign):
                stmts[-1].expression.multiply(-1)
                return stmts, r
            else:
                tmp = Var(tmp_var_name(), expr_type)
                stmts.append(api.Assign(tmp, api.LinearExpression([(-1, r)])))
                return stmts, tmp

        elif is_add(e):
            stmts_left, r1 = aux(e.left)
            stmts_right, r2 = aux(e.right)
            if stmts_left and isinstance(stmts_left[-1], api.Assign):
                if stmts_right and isinstance(stmts_right[-1], api.Assign):
                    stmts = stmts_left[:-1] + stmts_right[:-1]
                    stmts.append(api.Assign(r1, api.LinearExpression(stmts_left[-1].expression.terms + stmts_right[-1].expression.terms)))
                    return stmts, r1
                else:
                    stmts = stmts_right + stmts_left
                    stmts[-1].expression.terms.append((1, r2))
                    return stmts, r1
            else:
                if stmts_right and isinstance(stmts_right[-1], api.Assign):
                    stmts = stmts_left + stmts_right
                    stmts[-1].expression.terms.append((1, r1))
                    return stmts, r2
                else:
                    tmp = Var(tmp_var_name(), expr_type)
                    stmts = stmts_left + stmts_right
                    stmts.append(api.Assign(tmp, api.LinearExpression([(1, r1), (1, r2)])))
                    return stmts, tmp

        elif is_sub(e):
            return aux(e.left + (-e.right))

        elif is_mul(e):
            if e.left.is_constant():
                constant = normalize_constant(e.left.constant, expr_type)
                stmts, r = aux(e.right)
                if stmts and isinstance(stmts[-1], api.Assign):
                    stmts[-1].expression.multiply(constant)
                    return stmts, r
                else:
                    tmp = Var(tmp_var_name(), expr_type)
                    stmts.append(api.Assign(tmp, api.LinearExpression([(constant, r)])))
                    return stmts, tmp
            elif e.right.is_constant():
                return aux(e.right * e.left)
            else:
                stmts_left, r1 = aux(e.left)
                stmts_right, r2 = aux(e.right)
                stmts = stmts_left + stmts_right
                tmp = Var(tmp_var_name(), expr_type)
                stmts.append(api.MulOperation(tmp, r1, r2))
                return stmts, tmp

        elif is_div(e):
            stmts_left, r1 = aux(e.left)
            stmts_right, r2 = aux(e.right)
            stmts = stmts_left + stmts_right
            tmp = Var(tmp_var_name(), expr_type)
            stmts.append(api.DivOperation(tmp, r1, r2))
            return stmts, tmp

        else:
            assert True, 'unreachable'

    return aux(expr)


def make_assign(var, tmp_var_fmt, expr):
    if expr.is_constant():
        constant = normalize_constant(expr.constant, var.type)
        return [api.Assign(var, api.LinearExpression([constant]))]
    else:
        stmts, result_var = expression_to_statements(tmp_var_fmt, expr)
        if not stmts:
            return [api.Assign(var, api.LinearExpression([(1, result_var)]))]
        else:
            stmts[-1].var = var
            return stmts


def make_assert(tmp_var_fmt, condition):
    assert isinstance(condition, CompCondition)
    stmts, result_var = expression_to_statements(tmp_var_fmt, condition.left - condition.right)
    if stmts and isinstance(stmts[-1], api.Assign):
        res = stmts[:-1]
        res.append(api.Assert(api.LinearConstraint(stmts[-1].expression, condition.operator)))
        return res
    else:
        expr = api.LinearExpression([(1, result_var)])
        stmts.append(api.Assert(api.LinearConstraint(expr, condition.operator)))
        return stmts


def simplify_condition(cond):
    ''' simplifies a condition to use only <=, ==, != and remove the not operator '''
    if is_inf(cond):
        return (cond.left <= cond.right) & (cond.left != cond.right)
    elif is_inf_eq(cond):
        return cond
    elif is_sup(cond):
        return simplify_condition(cond.right < cond.left)
    elif is_sup_eq(cond):
        return simplify_condition(cond.right <= cond.left)
    elif is_eq(cond):
        return cond
    elif is_not_eq(cond):
        return cond

    elif is_and(cond):
        return simplify_condition(cond.left) & simplify_condition(cond.right)
    elif is_or(cond):
        return simplify_condition(cond.left) | simplify_condition(cond.right)

    elif is_not(cond):
        if is_inf(cond.condition):
            return simplify_condition(cond.condition.left >= cond.condition.right)
        elif is_inf_eq(cond.condition):
            return simplify_condition(cond.condition.left > cond.condition.right)
        elif is_sup(cond.condition):
            return simplify_condition(cond.condition.left <= cond.condition.right)
        elif is_sup_eq(cond.condition):
            return simplify_condition(cond.condition.left < cond.condition.right)
        elif is_eq(cond.condition):
            return simplify_condition(cond.condition.left != cond.condition.right)
        elif is_not_eq(cond.condition):
            return simplify_condition(cond.condition.left == cond.condition.right)

        elif is_and(cond.condition):
            return simplify_condition(~cond.condition.left) | simplify_condition(~cond.condition.right)
        elif is_or(cond.condition):
            return simplify_condition(~cond.condition.left) & simplify_condition(~cond.condition.right)

        elif is_not(cond.condition):
            return simplify_condition(cond.condition.condition)
        else:
            assert True, 'unreachable'
    else:
        assert True, 'unreachable'


def condition_to_cfg(cfg, tmp_prefix, condition, current_block):
    '''
    build the condition starting from `current_block`, returns the end block.
    '''
    tmp_expr = ident_generator('tmp_' + tmp_prefix + '_expr%d_v%%d')
    block_or = ident_generator(tmp_prefix + '_or%d')

    def aux(block, cond):
        if is_inf_eq(cond) or is_eq(cond) or is_not_eq(cond):
            block.add_statements(make_assert(tmp_expr(), cond))
            return block

        elif is_and(cond):
            left_block = aux(block, cond.left)
            return aux(left_block, cond.right)

        elif is_or(cond):
            ident = block_or()
            left_block = api.BasicBlock(ident + '_left')
            right_block = api.BasicBlock(ident + '_right')
            end_block = api.BasicBlock(ident + '_end')
            cfg.add_block(left_block)
            cfg.add_block(right_block)
            cfg.add_block(end_block)
            block.add_next(left_block)
            block.add_next(right_block)

            left_block = aux(left_block, cond.left)
            right_block = aux(right_block, cond.right)

            left_block.add_next(end_block)
            right_block.add_next(end_block)

            return end_block

        else:
            assert True, 'unreachable'

    return aux(current_block, simplify_condition(condition))


def program_to_cfg(main_prog):
    tmp_var_name = ident_generator('tmp_expr%d_v%%d')
    block_assert = ident_generator('assert%d')
    block_if = ident_generator('if%d')
    block_while = ident_generator('while%d')
    main = api.BasicBlock('main')
    cfg = api.Cfg(main)

    def aux(prog, block):
        for statement in prog.statements:
            if isinstance(statement, Checkpoint):
                block.add_statement(api.Checkpoint(statement.name))
            elif isinstance(statement, Assign):
                block.add_statements(make_assign(statement.var, tmp_var_name(), statement.expression))
            elif isinstance(statement, Assert):
                block = condition_to_cfg(cfg, block_assert(), statement.condition, block)
            elif isinstance(statement, IfStatement):
                ident = block_if()

                true_block = api.BasicBlock(ident)
                false_block = api.BasicBlock(ident + '_else')
                end_block = api.BasicBlock(ident + '_end')
                cfg.add_block(true_block)
                cfg.add_block(false_block)
                cfg.add_block(end_block)
                block.add_next(true_block)
                block.add_next(false_block)

                true_block = condition_to_cfg(cfg, ident + '_cond', statement.condition, true_block)
                false_block = condition_to_cfg(cfg, ident + '_notcond', ~statement.condition, false_block)

                true_block = aux(statement.true, true_block)
                false_block = aux(statement.false, false_block)

                true_block.add_next(end_block)
                false_block.add_next(end_block)
                block = end_block
            elif isinstance(statement, WhileStatement):
                ident = block_while()

                pre_while = api.BasicBlock('pre_' + ident)
                while_block = api.BasicBlock(ident)
                end_block = api.BasicBlock(ident + '_end')
                cfg.add_block(pre_while)
                cfg.add_block(while_block)
                cfg.add_block(end_block)
                block.add_next(pre_while)
                pre_while.add_next(while_block)
                pre_while.add_next(end_block)

                while_block = condition_to_cfg(cfg, ident + '_cond', statement.condition, while_block)
                end_block = condition_to_cfg(cfg, ident + '_notcond', ~statement.condition, end_block)

                while_block = aux(statement.block, while_block)

                while_block.add_next(pre_while)
                block = end_block
            else:
                assert True, 'unreachable'

        return block

    aux(main_prog, main)
    return cfg
