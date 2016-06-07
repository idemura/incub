#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <cstdio>

using namespace std;

using i64 = long long int;

vector<int> bit_sums(vector<int> a) {
  cout<<"input: ";
  for (auto t : a) cout<<t<<" ";
  cout<<endl;
  for (int s = 1; s < a.size(); s += s) {
    cout<<"step "<<s<<endl;
    for (int i = s; i < a.size(); i += s) {
      a[i] += a[i - s];
      cout<<"update "<<i<<" a[i]="<<a[i]<<endl;
    }
  }
  return move(a);  // Not sure. Probably NRVO doens't kick in.
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
  auto bs = bit_sums(a);
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
