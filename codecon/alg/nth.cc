#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef long long int lli;

void print(lli *a, int a_n, int i, int j)
{
  for (int k = 0; k < a_n; k++) {
    printf("%lld ", a[k]);
  }
  printf("\n");
  for (int k = 0; k < a_n; k++) {
    if (k == i && i == j) {
      printf("i,j");
    } else if (k == i) {
      printf("i ");
    } else if (k == j) {
      printf("j ");
    } else {
      printf("  ");
    }
  }
  printf("\n");
}

lli nthRec(lli *a, int a_n, int i0, int j0, int ith)
{
  if (i0 == j0) {
    return a[i0];
  }

  int i = i0; // 'i' is count of items less than 'd'.
  int j = j0;
  lli d = a[i];
  for (;;) {
    for (; a[i] < d; i++) {
    }
    for (; j > i && a[j] >= d; j--) {
    }
    // 'i' can't be equal to 'j', because can't be at the same time
    // a[i] >= d(from the first loop) and a[i] < d (from the second
    // loop break condition).
    if (i < j) {
      std::swap(a[i], a[j]);
      i++;
      j--;
    } else {
      break;
    }
  }

  // Here 'i' is count of a[i] such that a[i] < d.
  if (i == i0) {
    if (i == ith) {
      return a[i];
    } else {
      i++;
    }
  }

  if (ith < i) {
    return nthRec(a, a_n, i0, i - 1, ith);
  } else {
    return nthRec(a, a_n, i, j0, ith);
  }
}

lli nth(lli *a, int a_n, int ith)
{
  assert(ith >= 0 && ith < a_n);
  return nthRec(a, a_n, 0, a_n - 1, ith);
}

void test1()
{
  lli a_src[] = {5, 8, 3, 7, 2, 4, 6, 9, 1, 0};
  lli a[ARRAY_SIZEOF(a_src)];
  const int a_n = ARRAY_SIZEOF(a);

  for (int i = 0; i < a_n; i++) {
    memcpy(a, a_src, sizeof a_src);
    assert(nth(a, a_n, i) == i);
  }
  printf("test1 OK\n");
}

void test2()
{
  lli a_src[] = {5, 5, 5, 5, 5};
  lli a[ARRAY_SIZEOF(a_src)];
  const int a_n = ARRAY_SIZEOF(a);

  for (int i = 0; i < a_n; i++) {
    memcpy(a, a_src, sizeof a_src);
    assert(nth(a, a_n, i) == 5);
  }
  printf("test2 OK\n");
}

void test3()
{
  lli a_src[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  lli a[ARRAY_SIZEOF(a_src)];
  const int a_n = ARRAY_SIZEOF(a);

  for (int i = 0; i < a_n; i++) {
    memcpy(a, a_src, sizeof a_src);
    assert(nth(a, a_n, i) == i);
  }
  printf("test3 OK\n");
}

int main()
{
  test1();
  test2();
  test3();
  return 0;
}
