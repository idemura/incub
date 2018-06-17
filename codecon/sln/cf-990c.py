n = int(input())

def get_balance(s):
    closed_b = 0
    open_b = 0
    for si in s:
        if si == '(':
            open_b += 1
        if si == ')':
            if open_b == 0:
                closed_b += 1
            else:
                open_b -= 1
    return (open_b, closed_b)

cnt_o = {}
cnt_c = {}
balanced = 0
for i in range(n):
    s = input()
    o, c = get_balance(s)
    if o == 0 and c == 0:
        balanced += 1
    if o > 0 and c == 0:
        cnt_o[o] = cnt_o.get(o, 0) + 1
    if o == 0 and c > 0:
        cnt_c[c] = cnt_c.get(c, 0) + 1

n = balanced * balanced
for k in cnt_o:
    if k in cnt_c:
        n += cnt_o[k] * cnt_c[k]

print(n)
