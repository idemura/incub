#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <cstdio>

using namespace std;

using i64 = long long int;

int minor_bit(int n) {
  return n & -n;
}

// Sum of @k elements we store at index (k - 1).
vector<int> make_bit(vector<int> a) {
  cout<<"input: ";
  for (auto t : a) cout<<t<<" ";
  cout<<endl;
  for (int s = 2; s <= a.size(); s += s) {
    cout<<"step "<<s<<endl;
    for (int i = s - 1; i < a.size(); i += s) {
      cout<<"adding to "<<i<<" elem at "<<(i - s/2)
          <<": "<<a[i]<<" + "<<a[i - s/2]<<" = ";
      a[i] += a[i - s / 2];
      cout<<a[i]<<endl;
    }
    cout<<"bs: ";
    for (auto g : a) cout<<g<<" ";
    cout<<endl;
  }
  return move(a);  // Not sure. Probably NRVO doens't kick in.
}

// @n - number of elements.
int bit_sum(const vector<int> &b, int n) {
  int s = 0;
  while (n != 0) {
    s += b[n - 1];
    n -= minor_bit(n);
  }
  return n;
}

int main() {
  int an = 0, qn = 0;
  scanf("%d", &an);
  vector<int> a(an);
  for (auto &x : a) scanf("%d", &x);
  scanf("%d", &qn);
  vector<int> q(qn);
  for (auto &x : q) scanf("%d", &x);
  sort(a.begin(), a.end());
  auto bs = make_bit(a);
  cout<<"bs: ";
  for (auto g : bs) cout<<g<<" ";
  cout<<endl;
  auto positive = lower_bound(a.begin(), a.end(), 0);
  auto s = 0;
  for (auto x : q) {
    s += x;
    vector<int>::iterator i;
    if (s < 0) {
      i = lower_bound(positive, a.end(), -s);
    } else {
      i = lower_bound(a.begin(), positive, -s);
    }
  }
  return 0;
}
