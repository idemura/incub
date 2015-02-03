# Timus problem 1024
import math
import sys

data_in = sys.stdin.read().split()
positions = [int(x) for x in data_in[1:]]
for p in positions:
  g = int(math.sqrt(2 * p))
  first1 = g * (g - 1) // 2 + 1;
  first2 = g * (g + 1) // 2 + 1;
  if p >= first2:
    first = first2
  else:
    first = first1
  print("1" if p == first else "0", end=' ')
print()
