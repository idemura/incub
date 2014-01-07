#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <stdio.h>
#include <string.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

int digits[24], digits_n;

int rec(int s, int n, int i)
{
  if (i == digits_n) {
    s += n;
    return s % 2 == 0 || s % 3 == 0 || s % 5 == 0 || s % 7 == 0;
  } else {
    int d = digits[i];
    return rec(s + n,  d, i + 1) +
           rec(s + n, -d, i + 1) +
           rec(s, n * 10 + (n < 0? -d: d), i + 1);
  }
}

int main(int argc, char **argv)
{
  FILE *f = fopen(argv[1], "rt");
  if (f) {
    char s[24];
    while (fscanf(f, "%s", s) == 1) {
      for (int i = 0; s[i]; i++) {
        digits[i] = s[i] - '0';
      }
      digits_n = strlen(s);
      printf("%d\n", rec(0, digits[0], 1));
    }
    fclose(f);
  }
  return 0;
}
