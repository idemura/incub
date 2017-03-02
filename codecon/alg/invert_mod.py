m = 7
r = [0, 1]

for i in range(2, 7):
    # gcd(i, m) -> gcd(m % i, m)
    #   a * i + b * m = 1
    #   x * (m % i) + y * m = 1
    #   m % i = m - (m / i) * i
    # so coefficient with i is:
    #   a = - (m / i)
    a = m - m / i
    r.append(r[m % i] * a % m)
    print r

print "invert modulo m={}:".format(m)
print r

print "check:"
for i in range(m):
    print "{} * {} = {}".format(i, r[i], i * r[i] % m)
