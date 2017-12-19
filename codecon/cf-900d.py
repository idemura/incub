def gcd(m, n):
    if m > n:
        m, n = n, m
    while m != 0:
        m, n = n % m, m
    return n


def gcd_list(ls):
    g = ls[0]
    for n in ls:
        g = gcd(g, n)
        if g == 1:
            return 1
    return g


def enum_sums(n, ls):
    if n == 0:
        if gcd_list(ls) == 2:
            # print(ls)
            return 1
        else:
            return 0
        # print(ls)
        # return 1

    count = 0
    for e in range(1, n + 1):
        ls.append(e)
        count += enum_sums(n - e, ls[:])
        ls.pop()

    return count


print(enum_sums(8, []))
