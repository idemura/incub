def step(r):
  return (0.1 * r - 1) / (2 + 0.1 * r)

def doSomeSteps(n, x):
  for i in range(n):
    x = step(x)
  return x

for x in [0, 0.1, 0.2, 1, 2, 3, 10, 1000]:
  print '{0}'.format(doSomeSteps(100, x))

# x = 2.0 / 3
# for i in range(100):
#   print '{0}'.format(x)
#   x = step(x)
