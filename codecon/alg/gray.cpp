#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

void gray(int n, vector<int>* v_in)
{
  vector<int>& v = *v_in;
  int pn = 1 << n;
  v.resize(pn);
  v[0] = 0;
  v[1] = 1;
  for (int i = 1; i < pn; i++) {
    int pi = 1 << i;
    for (int j = 0; j < pi; j++) {
      v[((pi << 1) - 1) - j] = v[j] | pi;
    }
  }
}

string binary_int(int n, int width)
{
  string s;
  while (n) {
    s.push_back(n & 1? '1': '0');
    n >>= 1;
  }
  while (s.size() < width) {
    s.push_back('0');
  }
  reverse(s.begin(), s.end());
  return s;
}

int main()
{
  int n = 3;
  vector<int> gs;
  gray(n, &gs);
  for (int i = 0; i < gs.size(); i++) {
    printf("%s\n", binary_int(gs[i], n).c_str());
  }
  return 0;
}
