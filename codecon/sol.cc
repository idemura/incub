#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

const int DIM = 92;
const int HASH_MOD = 7789;

typedef long long int lli;

// This string has manual memory control to have cheap copy.
struct String {
  static std::vector<char*> str_mem;

  char *s;
  int length;
  int hash;

  String(): s(new char[DIM]), length(), hash()
  {
    str_mem.push_back(s);
  }

  void append(char c)
  {
    s[length++] = c;
    s[length] = 0;
    hash = ((hash << 8) + c) % HASH_MOD;
  }

  void freeMem()
  {
    delete[] s;
    s = NULL;
    length = 0;
  }

  static void freeAllMem()
  {
    for (int i = 0; i < str_mem.size(); i++) {
      delete[] str_mem[i];
    }
    str_mem.clear();
  }

  int hashCompareTo(const String &str) const
  {
    if (hash == str.hash) {
      return strcmp(s, str.s);
    } else {
      return hash - str.hash;
    }
  }

  bool operator<(const String &str) const
  {
    return strcmp(s, str.s) < 0;
  }
};

std::vector<char*> String::str_mem;

struct LcsRoute {
  std::vector<String> rs;
  int length;

  LcsRoute(): length() {}

  void clear()
  {
    length = 0;
    rs.clear();
  }

  void merge(const LcsRoute &v1, const LcsRoute &v2)
  {
    printf("in merge %d %d\n", v1.length, v2.length);
    length = std::max(v1.length, v2.length);
    if (v1.length == v2.length) {
      printf("  equal lcs length\n");
      int i = 0, j = 0;
      for (; i < v1.rs.size() && j < v2.rs.size();) {
        int order = v1.rs[i].hashCompareTo(v2.rs[j]);
        if (order <= 0) {
          printf("  merged: %s\n", v1.rs[i].s);
          rs.push_back(v1.rs[i]);
          i++;
          if (order == 0) {
            j++;
          }
        } else {
          printf("  merged: %s\n", v2.rs[i].s);
          rs.push_back(v2.rs[j]);
          j++;
        }
      }
      for (; i < v1.rs.size(); i++) {
        printf("  merged: %s\n", v1.rs[i].s);
        rs.push_back(v1.rs[i]);
      }
      for (; j < v2.rs.size(); j++) {
        printf("  merged: %s\n", v2.rs[i].s);
        rs.push_back(v2.rs[j]);
      }
    } else {
      // We could avoid copying, but 1000 possibly is not that big.
      if (v1.length == length) {
        printf("  take v1 %d\n", v1.length);
        rs = v1.rs;
      } else {
        printf("  take v2 %d\n", v2.length);
        rs = v2.rs;
      }
    }

    printf("after merge: %d\n", length);
    for (int i = 0; i < rs.size(); i++) {
      printf("  %s\n", rs[i].s);
    }
  }

  void append(const LcsRoute &lr, char c)
  {
    length = lr.length + 1;
    rs = lr.rs;  // Copy, but seems to be necessary.
    if (rs.size() == 0) {
      String s;
      s.append(c);
      rs.push_back(s);
      printf("new string: %s\n", s.s);
    } else {
      printf("append %c:\n", c);
      for (int i = 0; i < rs.size(); i++) {
        printf("  append to %s\n", rs[i].s);
        // assert(rs[i].length == length);
        rs[i].append(c);
      }
    }

    printf("after append: %d\n", length);
    for (int i = 0; i < rs.size(); i++) {
      printf("  %s\n", rs[i].s);
    }
  }

  void sort()
  {
    std::sort(rs.begin(), rs.end());
    // rs.erase(std::unique(rs.begin(), rs.end()), rs.end());
  }
};

// Two rows of LCS matrix.
LcsRoute lcs(char *a, char *b)
{
  LcsRoute rv[2][DIM];
  int an = strlen(a);
  int bn = strlen(b);
  int f = 0;
  for (int i = 1; i <= an; i++) {
    printf("-------- %d\n", i);
    for (int j = 1; j <= bn; j++) {
      rv[1 - f][j].clear();
      if (a[i - 1] == b[j - 1]) {
        printf("append %d\n", j);
        if (i == 3 && j == 2) {
        }
        rv[1 - f][j].append(rv[f][j - 1], a[i - 1]);
      } else {
        printf("merge %d\n", j);
        rv[1 - f][j].merge(rv[1 - f][j - 1], rv[f][j]);
      }
    }
    f = 1 - f;
  }
  rv[f][bn].sort();
  return rv[f][bn];
}

char a[84], b[84];

void routes()
{
  const LcsRoute &lr = lcs(a, b);
  // printf("%d\n", r.length);
  for (int i = 0; i < lr.rs.size(); i++) {
    printf("%s\n", lr.rs[i].s);
  }
  String::freeAllMem();
}

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int t = 0;
  scanf("%d", &t);
  while (t-- > 0) {
    scanf("%s%s", a, b);
    routes();
  }
  return 0;
}
