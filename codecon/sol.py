from __future__ import print_function
import bisect

def test_run(n):
    print('--- n=' + str(n))
    m = {}
    s = 0
    for q in range(1, n + 1):
        m[q] = []
    for k in range(1, n + 1):
        s += n % k
        m[n / k].append(n % k)
    for q in m:
        l = m[q]
        if len(l) == 0: continue
        print(str(q) + ': ' + str(l))
    print('sum={0}'.format(s))

for n in range(100, 121):
    test_run(n)

