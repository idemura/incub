import math
import sys

def split_dices(l):
  n = 6
  r = []
  for k in range(0, len(l), n):
    r.append(tuple(l[k : k + n]))
  return r

def order(l):
  k = 1
  if l[2] < l[k]: k = 2
  if l[3] < l[k]: k = 3
  if l[4] < l[k]: k = 4
  if k == 1: return (l[0], l[1], l[2], l[3])
  if k == 2: return (l[0], l[2], l[3], l[4])
  if k == 3: return (l[0], l[3], l[4], l[1])
  if k == 4: return (l[0], l[4], l[1], l[2])

def canonical(dice):
  if dice[0] == 1:
    return order((dice[1], dice[2], dice[3], dice[4], dice[5]))
  if dice[1] == 1:
    return order((dice[0], dice[2], dice[5], dice[4], dice[3]))
  if dice[2] == 1:
    return order((dice[4], dice[1], dice[3], dice[0], dice[5]))
  if dice[3] == 1:
    return order((dice[5], dice[2], dice[1], dice[4], dice[0]))
  if dice[4] == 1:
    return order((dice[2], dice[1], dice[5], dice[0], dice[3]))
  if dice[5] == 1:
    return order((dice[3], dice[2], dice[0], dice[4], dice[1]))

data_in = sys.stdin.read().split()
dices = split_dices([int(x) for x in data_in[1:]])
dice_type = {}
output_order = []
for i, d in enumerate(dices):
  c = canonical(d)
  if c not in dice_type:
    dice_type[c] = []
    output_order.append(c)
  dice_type[c].append(i + 1)

print len(dice_type)
for i in output_order:
  for k in dice_type[i]:
    print k,
  print
