#include "base.h"

int main() {
  int a, b;
  cin >> a >> b;
  if (a >= b) {
    cout << a / b << ".";
    a %= b;
  } else {
    cout << "0.";
  }

  vector<int> dds, rems(5001);
  while (rems[a] == 0) {
    rems[a] = dds.size() + 1;
    int d = 10 * a / b;
    int x = 10 * a % b;
    dds.push_back(d);
    a = x;
  }

  const int cycle = rems[a] - 1;
  int i = 0;
  for (; i < cycle; i++) {
    cout << dds[i];
  }
  cout << "(";
  for (; i < dds.size(); i++) {
    cout << dds[i];
  }
  cout << ")" << endl;
  return 0;
}
