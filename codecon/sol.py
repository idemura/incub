import math
import sys

def min_blocks_in_columns(k):
  return k * (k + 1) // 2

def sum_kth_down(a, k, n):
  s = 0
  i = n - k
  while i >= 0:
    s += a[i]
    # if a[i] == 0: break?
    i -= k
  return s

def dynamic(n):
  d = [[0] * (n + 1), [0] + [1] * n]
  cols = 2
  while min_blocks_in_columns(cols) <= n:
    d[cols] = [sum_kth_down(d[cols - 1], cols, i) for i in range(0, n + 1)]
    cols += 1
  s = 0
  for i in range(1, cols):
    s += d[i][n]
  return s

data_in = sys.stdin.read().split()
ints_in = [int(x) for x in data_in]
dynamic(ints_in[0])
