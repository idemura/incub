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
using u32 = unsigned int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 100000007;

template<class T>
string bitstr(T n) {
  string s;
  for (int i = 0; i < sizeof(T) * 8; i++) {
    if (n & (1llu << i)) {
      s.push_back('1');
    } else {
      s.push_back('0');
    }
  }
  return s;
}

template<class T>
string bitstr(T n, int b, int w) {
  string s;
  for (int i = 0; i < sizeof(T) * 8; i++) {
    if (i == b) s.push_back('[');
    if (i == b + w) s.push_back(']');
    if (n & (1llu << i)) {
      s.push_back('1');
    } else {
      s.push_back('0');
    }
  }
  return s;
}

void print_bin_array(const u32 *a, int n) {
  cout<<"Binary array:\n";
  for (int i = 0; i < n; i++) {
    cout<<"  #"<<i<<" "<<bitstr(a[i])<<"\n";
  }
  cout<<endl;
}

u64& u64_ref(u32 &r) { return *(u64*)&r; }

// @n, @s are bit sizes. @b assumed to be zeroed. Both @a and @b should have
// size aligned to 64 bits.
void rotate_bits_left(const u32 *a, int n, int s, u32 *b) {
  for (int si = 0, di = s; si < n; si += 32) {
    u64 next_32b = u64(a[si >> 5]) << (di & 31);
    cout<<"next 32 bit start "<<(di & 31)<<" "
        <<bitstr(next_32b, di & 31, 32)<<endl;
    u64_ref(b[di >> 5]) |= next_32b;
    //cout<<"updated 64 bit at "<<(di >> 5)<<" "<<bitstr(x)<<endl;
    print_bin_array(b, 3);
    di += 32;
    if (di >= n) {
      cout<<"out of bounds on "<<(di - n)<<endl;
      // If we got some bits behind the end, wrap them to the beginning. 32 bit
      // mem access is enough.
      next_32b >>= 32 - (di - n) + (di & 31);
      cout<<"shifted value: "<<bitstr(next_32b)<<endl;
      b[0] |= next_32b;
      di -= n;
    }
    //if (di >= n) di -= n;
  }
  // Last 64 bits must have trailing zeroes.
  u64_ref(b[n >> 5]) &= ~(~0llu << (n & 63));
}

int bitsets[10002][320];

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  // int n, k;
  // cin>>n;
  // u32 temp[320] = {};
  // for (int i = 0; i < n && (a[0] & 1) == 0; i++) {
  //   cout<<"i="<<i<<endl;
  //   cin>>k;
  //   k %= n;
  //   // Rotate if not first (i == 0) or shift makes sense (k != 0).
  //   memset(b, 0, sizeof bmem);
  //   if (i == 0 || k == 0) {
  //   }
  // }
  // if (a[0] & 1) {
  //   cout<<"possible!!!"<<endl;
  // } else {
  //   cout<<"0\n";
  // }
  cout<<"hello"<<endl;
  return 0;
}
