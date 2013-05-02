#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

#define MOD 1000000007

int nth_digit(int n, int j)
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

int main(int argc, char **argv)
{
    int a, b, n = 0, i, j;

    scanf("%d%d%d", &a, &b, &n);
    if (a > b) {
        int temp = a;
        a = b;
        b = temp;
    }

    // Min and max sum of digits
    int min_sum = a * n;
    int max_sum = b * n;

    printf("%d\n", nth_digit(123, 0));
    printf("%d\n", nth_digit(123, 1));
    printf("%d\n", nth_digit(123, 2));
    printf("%d\n", nth_digit(123, 3));

    return 0;
}
