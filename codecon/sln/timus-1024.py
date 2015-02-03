# Timus problem 1024
import sys

def cycle_length(perm, v):
  visited = [0] * len(perm)
  count = 0
  while not visited[v]:
    visited[v] = 1
    count += 1
    v = perm[v]
  return count

def gcd(a, b):
  if a < b:
    return gcd(b, a)
  if b == 0:
    return a
  else:
    return gcd(b, a % b)

def lcm(a, b):
  return a * b // gcd(a, b)

data_in = sys.stdin.read().split()
t = [int(x) - 1 for x in data_in[1:]]
cycles = [cycle_length(t, i) for i in range(len(t))]
a = 1
for c in cycles:
  a = lcm(a, c)
print(a)
