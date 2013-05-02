#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

#define MOD 1000000007

int a, b, n;

/* Gets nth digit of `n` in decimal form. Digits counted from the rightmost,
   least significant, which is 0
*/
int nth_digit(int j, int n)
{
    while (n != 0) {
        int d = n % 10;
        n /= 10;
        if (j == 0) {
            return d;
        }
        j--;
    }
    return 0;
}

int get_num(int k)
{
    return b * k + (n - k) * a;
}

int digit_lower_bound(int d, int pos, int lo, int hi)
{
    // int dlo = nth_digit(pos, get_num(lo));
    // int dhi = nth_digit(pos, get_num(hi));
    // if (nlo < d && nhi < d || nlo > d && nhi > d) {
    //     return -1;
    // }
    while (lo < hi) {
        int mid = (lo + hi) / 2;
        int nmid = get_num(mid);
        int dmid = nth_digit(pos, nmid);
        if (d > dmid) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    return lo;
}

int main(int argc, char **argv)
{
    int i, j;

    scanf("%d%d%d", &a, &b, &n);
    if (a > b) {
        int temp = a;
        a = b;
        b = temp;
    }

    // Min and max sum of digits
    int min_sum = a * n;
    int max_sum = b * n;
    a = 3;
    b = 7;
    n = 14;
    int lb = digit_lower_bound(5, 1, 0, n + 1);
    printf("lb %d\n", lb);
    printf("this num is %d\n", get_num(lb));
    printf("prev num is %d\n", get_num(lb - 1));
    printf("next num is %d\n", get_num(lb + 1));

    printf("%d\n", nth_digit(123, 0));
    printf("%d\n", nth_digit(123, 1));
    printf("%d\n", nth_digit(123, 2));
    printf("%d\n", nth_digit(123, 3));

    return 0;
}
