import sys

INF = 0x7fffffff

def parse_pos(s):
  return ("abcdefgh".find(s[0]), int(s[1]) - 1)
def pos_to_string(p):
  return "abcdefgh"[p[0]] + "12345678"[p[1]]
# def make_table(x):
#   return [[x] * 8 for _ in range(8)]
def on_board(x, y):
  return 0 <= x and x < 8 and 0 <= y and y < 8;

def pq_min(pq):
  v_min = INF
  k_min = None
  for k in pq:
    v = pq[k]
    if v < v_min:
      k_min = k
      v_min = v
  del pq[k_min]
  return k_min, v_min

def pq_print(pq):
  print 'PQ:'
  items = sorted(pq.items(), key=lambda x: x[1])
  for (p, f), v in items:
    print 'weight', v, 'at', pos_to_string(p), 'face', f

# Neighbours in the order: up, right, bottom, left for each face of the cube in
# the order they appear in the input.
next_cell = [(0, 1), (1, 0), (0, -1), (-1, 0)]
next_face = [
    (4, 3, 2, 5),
    (2, 3, 4, 5),
    (0, 3, 1, 5),
    (1, 2, 0, 4),
    (1, 3, 0, 5),
    (1, 4, 0, 2),
]

data_in = sys.stdin.read().split()
s, e = parse_pos(data_in[0]), parse_pos(data_in[1])
face_value = [int(x) for x in data_in[2:]]
path_value = {}  # Map from position to min path value to this position.
prev = {}

# Priority queue key is pair or position(pair) and the face on the cell.

# cp - current position
# pp - prev position
# np - next position
# cv - current value
# nv - next value
# nk - next key
pq = {(s, 4) : 0}
pp = None
while len(pq) > 0:
  print '-- new cycle'
  pq_print(pq)
  (cp, f), cv = pq_min(pq)
  print "extracted:", pos_to_string(cp), "value", cv
  print "face bottom:", f
  path_value[cp] = cv
  prev[cp] = pp
  pp = cp
  for i, (dx, dy) in enumerate(next_cell):
    np = (cp[0] + dx, cp[1] + dy)
    if on_board(*np) and (np not in path_value):
      print "next position", pos_to_string(np), "is on board and min is not known"
      nf = next_face[f][i]
      print 'next face', nf
      nv = cv + face_value[nf]
      print 'new value', nv
      nk = (np, nf)
      if nk in pq:
        print 'update value from', pq[nk]
        pq[nk] = min(pq[nk], nv)
      else:
        print 'set new'
        pq[nk] = nv

print path_value[e],
r_path = [e]
t = e
print '----'
for i in range(0, 8):
  for j in range(0, 8):
    if (i, j) in prev:
      print "X",
    else:
      print " ",
  print

print prev
while prev[t] is not None:
  r_path += prev[t]
  t = prev[t]
r_path += s
for p in reversed(r_path):
  print pos_to_string(p),
print
