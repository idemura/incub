#include "base.h"

constexpr int kMod = 100;

struct CycleNum {
  int n = 0;
  int cycle_length = 0;
  int cycle_unique = 0;
};

CycleNum get_cycle(i64 n) {
  cout<<"------- n="<<n<<endl;
  vector<u64> a(kMod / 64 + 2);
  int c = 0;
  int cycle_start = 0;
  for (auto i = n;;) {
    cout<<"i="<<i<<endl;
    auto bm = 1ull << (i & 63);
    if (a[i >> 6] & bm) {
      cycle_start = i;
      break;
    }
    a[i >> 6] |=  bm;
    i = (i * n) % kMod;
    c++;
  }
  cout<<"cycle_start="<<cycle_start<<endl;
  int u = 0;
  for (auto i = n; i != cycle_start;) {
    i = (i * n) % kMod;
    u++;
  }
  CycleNum cn;
  cn.n = n;
  cn.cycle_unique = u;
  cn.cycle_length = c;
  return cn;
}

string to_string(CycleNum cn) {
  stringstream ss;
  ss<<"CycleNum{n="<<cn.n
    <<" length="<<cn.cycle_length
    <<" unique="<<cn.cycle_unique
    <<"}";
  return ss.str();
}

int cycle_num_power(CycleNum cn, int p) {
  return 0;
}

void test(int n) {
  cout<<"n="<<n<<" "<<to_string(get_cycle(n))<<endl;
}

int main(int argc, char **argv) {
  test(1);
  test(2);
  test(3);
  test(4);
  test(5);
  test(6);
  test(7);
  cout << "TESTS PASSED." << endl;
  return 0;
}

