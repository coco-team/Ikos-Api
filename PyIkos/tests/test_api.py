#!/usr/bin/env python
#################################################################################
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

from ikos.api import *

x = IntVar('x')
y = IntVar('y')

main = BasicBlock('main')
main.add_statement(Assign(x, LinearExpression([0])))
main.add_statement(Assign(y, LinearExpression([2])))

loop = BasicBlock('loop')
loop.add_statement(Assert(LinearConstraint(LinearExpression([(1, x), -10]), LinearConstraint.INF_EQ)))
loop.add_statement(Assign(x, LinearExpression([(1, x), 1])))
loop.add_statement(Assign(y, LinearExpression([(1, y), 4])))
loop.add_statement(Checkpoint('loop'))

end = BasicBlock('end')
end.add_statement(Assert(LinearConstraint(LinearExpression([(1, x), -11]), LinearConstraint.EQ)))
end.add_statement(Checkpoint('end'))

cfg = Cfg(main)
cfg.add_block(loop)
cfg.add_block(end)

main.add_next(loop)
loop.add_next(loop)
loop.add_next(end)

print('# python print #########################')
print(cfg)

print('# ikos print #########################')
apicore.print_cfg(cfg)


print('# ikos fix point #########################')
result = cfg.fixpoint(Domain.INTERVAL)
print('loop: %s, %s' % (result['loop'][0], result['loop'][1]))
print('end: %s, %s' % (result['end'][0], result['end'][1]))
