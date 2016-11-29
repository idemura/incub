#include <iostream>
#include <vector>
#include <string>

using namespace std;

void dfs(
        const vector<vector<int>> &child,
        int v,
        vector<int> &dfs_seq) {
  dfs_seq.push_back(v);
  for (auto w : child[v]) {
    dfs(child, w, dfs_seq);
  }
}

int main() {
  int n;
  cin>>n;
  vector<vector<int>> child(n + 1);
  for (int i = 1; i < n; i++) {
    int p;
    cin>>p;
    child[p].push_back(i + 1);
  }
  vector<int> a;
  dfs(child, 1, a);
  for (auto v : a) {
    cout<<v<<" ";
  }
  cout<<endl;

  vector<vector<int>> casc;
  casc.push_back(move(a));
  for (int i = 1; ; i++) {
    auto s = 1<<i;
    vector<int> p;

  }
  return 0;
}
