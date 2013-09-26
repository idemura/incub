#include <stdio.h>
#include <vector>
#include <algorithm>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

void prn_v(const vector<int> &v, const char *tag)
{
    if (tag) {
        printf("%s: ", tag);
    }
    for (int i = 0; i < v.size(); i++) {
        printf("%d ", v[i]);
    }
    printf("| size %ld\n", v.size());
}

int lis(const vector<int> &as, vector<int> *lis_out)
{
    vector<int> top;
    for (int i = 0; i < as.size(); i++) {
        // `prn_n` maintained sorted all the time. `top[k]` is the minimal tail
        // element of increasing sequence of length `k`. The invariant is
        // maintained for [0, i) subsequence of `as`. Can prove that it's
        // optimal. On every step, we find the item such that:
        //   a[l-1] < x <= a[l]
        // So, replacement `top[l]` with `x` save our items' order. If `x` is
        // the largest so far, increase top. At algorithm end, `top` is the LIS.
        // prn_v(top, "top");
        int x = as[i];
        int l = 0, r = top.size();
        while (l < r) {
            int m = (r + l) / 2;
            if (x <= top[m]) {
                r = m;
            } else {
                l = m + 1;
            }
        }
        // l is the index of an item >= x.
        if (l == top.size()) {
            top.push_back(as[i]);
        } else {
            top[l] = x;
        }
    }
    lis_out->assign(top.begin(), top.end());
    return 0;
}

int main()
{
    const int vs[] = {10, 30, 20, 15, 5, 8, 25, 40, 50};
    vector<int> v(vs, vs + ARRAY_SIZEOF(vs));
    vector<int> lis_out;
    lis(v, &lis_out);
    prn_v(lis_out, "LIS");
    return 0;
}
