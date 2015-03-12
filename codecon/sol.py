import sys

INF = 0x7fffffff

def parse_pos(s):
  return ("abcdefgh".find(s[0]), int(s[1]) - 1)
def make_table(x):
  return [[x] * 8 for _ in range(8)]
def on_board(x, y):
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
start, end = parse_pos(data_in[0]), parse_pos(data_in[1])
faces = [int(x) for x in data_in[2:]]
min_known = {}
# cost = make_table(INF)
prev = make_table(None)

pq = {start : 0}
p = None
while len(pq) > 0:
  (x, y), v = pq_min(pq)
  min_known[(x, y)] = True
  prev[(x, y)] = p
  for dx, dy in neibs_delta:
    xs = x + dx
    ys = y + dy
    if on_board(xs, ys):
      vs = v +
      if (xs, ys) in pq:
        xs
