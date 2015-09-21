#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <set>
#include <chrono>
#include <unordered_map>
#include <memory>
#include <vector>
#include <string>
#include <random>
#include <limits>
#include <sstream>
#include <utility>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cmath>  // Overloads for abs.

using namespace std;

using i64 = long long int;
using u64 = unsigned long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 100000007;

string binary_u64(u64 n) {
  string s(64, '0');
  for (int i = 0; i < 64; i++) {
    if (n & (1llu << i)) {
      s[i] = '1';
    }
  }
  return s;
}

u64 lo_bits(u64 n, int b) {
  return n & ~(~0llu << b);
}

void print_bin_array(const u64 *a, int n) {
  cout<<"Binary array:\n";
  for (int i = 0; i < n; i++) {
    cout<<"  #"<<i<<" "<<binary_u64(a[i])<<"\n";
  }
  cout<<endl;
}

void shift(const u64 *a, int n, int s, u64 *b) {
  cout<<"n="<<n<<endl;
  cout<<"s="<<s<<endl;
  for (int i = 0; i < n; i += 64) {
    cout<<"i="<<i<<endl;
    int s_mod = s & 63;
    cout<<"s_mod="<<s_mod<<endl;
    int w = s >> 6;
    cout<<"w="<<w<<endl;
    if (s + 64 <= n) {
      cout<<"64 bit word fits, no wrap"<<endl;
      // `bw` least significant bits consumed, `64 - bw` left to consume.
      cout<<"a[w]="<<binary_u64(a[w])<<endl;
      cout<<"high 64-s_mod bits moved to lo bits:\n";
      cout<<binary_u64(a[w] >> s_mod)<<endl;
      cout<<"a[w+1]="<<binary_u64(a[w+1])<<endl;
      cout<<"low b bits for next word moved to high:\n";
      cout<<binary_u64(a[w + 1] << (64 - s_mod))<<endl;
      b[i >> 6] = (a[w] >> s_mod) | (a[w + 1] << (64 - s_mod));
      cout<<"b[]="<<binary_u64(b[i >> 6])<<endl;
      s += 64;
    } else {
      cout<<"CASE2\n";
      // 64 bits word wrapped around @n. Read m=`n - b` and `64 - m` bits
      // from the beginning. Given that we maintain 0 after n-th bit, we
      // mask out only lower @b bits.
      cout<<"s="<<s<<endl;
      s += 64 - n;
      cout<<"s="<<s<<endl;
      b[i >> 6] = (a[w] >> s_mod) | lo_bits(a[0], s);
    }
  }
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  u64 q[3] = {}, res[3];
  q[0] |= 1llu;
  q[0] |= 1llu<<2;
  q[0] |= 1llu<<63;
  q[1] |= 1llu<<1;
  q[1] |= 1llu<<3;
  q[1] |= 1llu<<15;
  q[2] |= 1llu;
  q[2] |= 1llu<<1;
  print_bin_array(q, 3);
  shift(q, 130, 2, res);
  print_bin_array(res, 3);
  return 0;
  int n, k;
  cin>>n;
  u64 amem[2][160] = {};
  u64 *a = amem[0], *t = amem[1];
  for (int i = 0; i < n && (a[0] & 1) == 0; i++) {
    cout<<"i="<<i<<endl;
    cin>>k;
    k %= n;
    // Rotate if not first (i == 0) or shift makes sense (k != 0).
    if (i == 0 || k == 0) {
    }
  }
  if (a[0] & 1) {
    cout<<"possible!!!"<<endl;
  } else {
    cout<<"0\n";
  }
  return 0;
}
