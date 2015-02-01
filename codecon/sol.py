# Timus problem 1013
import sys

class Mat2x2:
  def __init__(self):
    self.m11 = self.m22 = 1
    self.m12 = self.m21 = 0

  def __str__(self):
    return '[ {0} {1};\n  {2} {3} ]'.format(
        self.m11, self.m12,
        self.m21, self.m22)

def mat_mul(a, b, m):
  r = Mat2x2()
  r.m11 = (a.m11 * b.m11 + a.m12 * b.m21) % m
  r.m12 = (a.m11 * b.m12 + a.m12 * b.m22) % m
  r.m21 = (a.m21 * b.m11 + a.m22 * b.m21) % m
  r.m22 = (a.m21 * b.m12 + a.m22 * b.m22) % m
  return r

def mat_pow(x, p, m):
  r = Mat2x2()
  while p != 0:
    if p & 1 == 1:
      r = mat_mul(x, r, m)
    x = mat_mul(x, x, m)
    p >>= 1
  return r

parts = sys.stdin.read().split()
n = int(parts[0])
k = int(parts[1])
m = int(parts[2])

# Let a[n] - # of numbers of length n starting with 0,
#     b[n] - # of numbers of length n starting with non-zero.
# Then:
#   a[n + 1] = b[n]
#   b[n + 1] = (a[n] + b[n]) * (k - 1).
# Initial vector is [1, k - 1] and we are interested in b[n].
step = Mat2x2()
step.m11 = 0
step.m12 = 1
step.m21 = k - 1
step.m22 = k - 1
sp = mat_pow(step, n - 1, m)
# print sp
print (sp.m21 + (k - 1) * sp.m22) % m
