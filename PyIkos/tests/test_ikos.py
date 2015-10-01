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

from ikos import *

x = Int('x')
y = Int('y')

p = Program()
p.assign(x, 10)
p.assign(y, 0)

b = p.add_while(x >= 0)
b.assign(x, x-1)
b.assign(y, y+2)
b.checkpoint('loop')

p.checkpoint('end')

print('# print ###################')
print(p)

print('# api cfg #################')
print(program_to_cfg(p))

print('# ikos fix point #############')
result = p.fixpoint(Domain.INTERVAL)
print('loop: %s, %s' % (result['loop'][0], result['loop'][1]))
print('end: %s, %s' % (result['end'][0], result['end'][1]))
