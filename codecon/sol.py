import sys

INF = 0x7fffffff

def parse_pos(s):
  return ("abcdefgh".find(s[0]), int(s[1]) - 1)
def make_table(x):
  return [[x] * 8 for _ in range(8)]
def on_board(p):
  x, y = p
  return 0 <= x and x < 8 and 0 <= y and y < 8;

def pq_min(pq):
  v_min = INF
  k_min = None
  for k, v in pq:
    if v < v_min:
      k_min = k
      v_min = v
  return k_min, v_min

# Neighbours in the order: up, right, bottom, left for each face of the cube in
# the order they appear in the input.
neibs_delta = [(0, 1), (1, 0), (0, -1), (-1, 0)]
neibs = [
    (4, 3, 2, 5),
    (2, 3, 4, 5),
    (0, 3, 1, 5),
    (1, 2, 0, 4),
    (1, 3, 0, 5),
    (1, 4, 0, 2),
]

data_in = sys.stdin.read().split()
s, e = parse_pos(data_in[0]), parse_pos(data_in[1])
faces = [int(x) for x in data_in[2:]]
min_known = {}
# cost = make_table(INF)
prev = make_table(None)

# pp - prev position
# np - next position
# cv - current value
# nv - next value
# nk - next key
pq = {(s, faces[4]) : 0}
pp = None
while len(pq) > 0:
  # Current Position
  (cp, f), cv = pq_min(pq)
  min_known[cp] = cv
  prev[cp] = pp
  for i, (dx, dy) in enumerate(neibs_delta):
    np = (cp[0] + dx, cp[1] + dy)
    if on_board(np) and not min_known[np]:
      nv = cv + faces[i]
      nk = (np, faces[i])  # Next Key
      if nk in pq:
        pq[nk] = min(pq[nk], nv)
      else:
        pq[nk] = nv

print min_known[e],
path = [e]
t = e
while prev[t] is not None:
  path += prev[t]
  t = prev[t]
path += s
for p in reversed(path):
  print "abcdefgh"[p[0]] + "12345678"[p[1]],
print
