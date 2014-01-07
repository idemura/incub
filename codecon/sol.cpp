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

int rec(int s, int i)
{
  if (i == digits_n) {
    return s % 2 == 0 || s % 3 == 0 || s % 5 == 0 || s % 7 == 0;
  } else {
    int n = 0, c = 0;
    for (int j = i; j < digits_n; j++) {
      n = 10 * n + digits[j];
      c += rec(s + n, j + 1);
      if (i > 0) {
        c += rec(s - n, j + 1);
      }
    }
    return c;
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
      printf("%d\n", rec(0, 0));
    }
    fclose(f);
  }
  return 0;
}
