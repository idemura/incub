#include <iostream>
#include <utility>

using namespace std;

int main() {
  int constexpr D1{2};
  int constexpr D2{3};
  int constexpr D3{4};
  int m[D1][D2][D3]{}, mt[D3][D2][D1]{};
  int n_elem = D1 * D2 * D3;
  int *p = &m[0][0][0];
  for (int i = 0; i < n_elem; i++) {
    p[i] = i;
  }
  for (int i1 = 0; i1 < D3; i1++) {
    cout<<"place "<<i1<<"\n";
    for (int i2 = 0; i2 < D2; i2++) {
      for (int i3 = 0; i3 < D1; i3++) {
        cout<<m[i3][i2][i1]<<" ";
      }
      cout<<"\n";
    }
  }
  return 0;
}
